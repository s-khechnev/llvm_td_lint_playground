open Checker_core.Utils
open Spec
open Func
open Myast

module V = struct
  type t = Func.t

  let make = Fun.id
  let compare = Func.compare
  let equal = Func.equal
  let hash = Func.hash
end

module Edge = struct
  type t = (string * string) list

  let make = Fun.id
  let compare = Stdlib.compare
  let equal = Stdlib.( = )
  let hash = Stdlib.Hashtbl.hash
  let default = []
end

module G = Graph.Imperative.Digraph.ConcreteLabeled (V) (Edge)

let dump g outfile =
  let module Dot = Graph.Graphviz.Dot (struct
    include G

    let vertex_name = function
      | F_usual id -> id
      | F_specialized (id, speced) ->
          let spec_str =
            speced
            |> List.map (fun (i, e) ->
                   Format.sprintf "(%s:%s)" (string_of_int i)
                     (Libsail.Ast_util.string_of_exp e))
            |> String.concat " "
          in
          Format.sprintf "<%s %s>" id spec_str

    let get_subgraph _ = None
    let vertex_attributes _ = []
    let graph_attributes _ = []
    let edge_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
  end) in
  Out_channel.with_open_text outfile (fun ch -> Dot.output_graph ch g)

let generate (funcs : (string list * 'a exp) FuncTable.t) myconst_prop
    add_depend_xlen_func =
  let open Libsail in
  let open Ast_util in
  let open Myast_iterator in
  let g = G.create () ~size:1500 in
  let rec helper src_func =
    let classify_func_call id args =
      let speced =
        List.mapi (fun i e -> (i, e)) args
        |> List.filter (fun (_, e) -> Constant_fold.is_constant e)
      in
      if List.is_empty speced then F_usual id else F_specialized (id, speced)
    in
    let rec extract_arg = function
      | E_aux
          ( E_app
              ( Id_aux (Id ("sign_extend" | "zero_extend"), _),
                [ _; E_aux (E_id (Id_aux (Id id, _)), _) ] ),
            _ ) ->
          id
      | E_aux (E_app (Id_aux (Id f_id, _), args), _) -> (
          helper (classify_func_call f_id args);
          match args with
          | [ x ] -> extract_arg x
          | _ -> (
              match
                List.find_map
                  (function
                    | E_aux (E_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
                  args
              with
              | Some id -> id
              | None -> ""))
      | e -> string_of_exp e
    in
    let process src_args dst_id =
      let dst_func = classify_func_call dst_id src_args in
      let add dst_args =
        let args2 =
          List.map2 (fun dst src -> (dst, extract_arg src)) dst_args src_args
        in
        let analyzed = G.mem_vertex g dst_func in
        G.add_edge_e g (dst_func, args2, src_func);
        if not analyzed then helper dst_func
      in
      match FuncTable.find_opt funcs dst_func with
      | Some (dst_args, _) -> add dst_args
      | None -> (
          match FuncTable.find_opt funcs (F_usual (Func.get_id dst_func)) with
          | Some (dst_args, body) ->
              let () =
                let speced_body, _ =
                  let substs =
                    match dst_func with
                    | F_specialized (_, speced) ->
                        speced
                        |> List.map (fun (i, e) ->
                               (List.nth dst_args i |> mk_id, e))
                        |> Bindings.of_list
                    | _ -> assert false
                  in
                  let ref_vars = Constant_propagation.referenced_vars body in
                  myconst_prop
                    (fun () -> add_depend_xlen_func dst_func)
                    ref_vars (substs, KBindings.empty) Bindings.empty body
                in
                FuncTable.add funcs dst_func (dst_args, speced_body)
              in
              add dst_args
          | None -> ())
    in
    let it =
      {
        default_iterator with
        exp_aux =
          (fun self e ->
            match e with
            | E_app
                ( Id_aux (Id "execute", _),
                  [
                    E_aux
                      ( E_app
                          ( Id_aux (Id dst_id, _),
                            [ E_aux (E_tuple src_args, _) ] ),
                        _ );
                  ] ) ->
                process src_args dst_id
            | E_app (Id_aux (Id dst_id, _), src_args)
              when dst_id <> Func.get_id src_func ->
                process src_args dst_id;
                default_iterator.exp_aux self e
            | _ -> default_iterator.exp_aux self e);
      }
    in
    match FuncTable.find_opt funcs src_func with
    | Some (_, body) -> it.exp it body
    | None -> ()
  in
  let () = FuncTable.iter (fun k _ -> helper k) funcs in
  g

let rec dfs g ~start_v ~on_edge =
  G.iter_succ_e
    (fun e ->
      try
        on_edge e;
        dfs g
          ~start_v:
            (let _, _, v_dst = e in
             v_dst)
          ~on_edge
      with _ -> ())
    g start_v

let propogate_operands ~g ~aliases funcs (info : (string * string) list) =
  let result = FuncTable.create 1000 in
  let on_edge : V.t * Edge.t * V.t -> unit =
   fun (v_src, args, v_dst) ->
    match FuncTable.find_opt result v_src with
    | Some src_opers ->
        let mapped =
          let check_aliases func_id xs =
            match FuncTable.find_opt aliases func_id with
            | Some aliases ->
                List.map
                  (fun id ->
                    match List.assoc_opt id aliases with
                    | Some alias -> alias
                    | None -> id)
                  xs
            | None -> xs
          in
          src_opers
          |> List.map (fun id ->
                 match List.assoc_opt id args with
                 | Some id -> id
                 | None -> if id = "0" then "v0" else id)
          |> check_aliases v_dst
        in
        (match FuncTable.find_opt result v_dst with
        | Some dst_opers ->
            FuncTable.replace result v_dst (mapped @ dst_opers |> rm_duplicates)
        | None -> FuncTable.add result v_dst mapped);

        if debug then (
          let src_str = Func.to_string v_src in
          let dst_str = Func.to_string v_dst in
          printfn "%s - %s - %s" src_str
            (String.concat " "
               (List.map (fun (a, b) -> Format.sprintf "(%s, %s) " a b) args))
            dst_str;
          printfn "%s opers: %s" src_str (String.concat " " src_opers);
          printfn "mapped: %s" (String.concat " " mapped);
          printfn "%s opers: %s\n" dst_str
            (String.concat " " (FuncTable.find result v_dst)))
    | None -> ()
  in

  G.iter_vertex
    (fun v ->
      match List.assoc_opt (get_id v) info with
      | Some op_name -> (
          match v with
          | F_usual _ -> FuncTable.add result v [ op_name ]
          | F_specialized (_, speced) -> (
              match FuncTable.find_opt funcs v with
              | Some (args, _) -> (
                  let op_i =
                    match
                      List.find_mapi
                        (fun i a -> if a = op_name then Some i else None)
                        args
                    with
                    | Some e -> e
                    | None -> assert false
                  in
                  match
                    List.find_map
                      (fun (i, e) ->
                        if op_i = i then Some (Libsail.Ast_util.string_of_exp e)
                        else None)
                      speced
                  with
                  | Some e -> FuncTable.add result v [ e ]
                  | None -> FuncTable.add result v [ op_name ])
              | None -> ()))
      | None -> ())
    g;

  let () =
    Seq.iter
      (fun start_v -> dfs g ~start_v ~on_edge)
      (FuncTable.to_seq_keys result)
  in
  result

let get_reachables_from_func g ~start_f ~break_ids =
  let result = FuncTable.create 100 in
  let exception Break in
  let on_edge (_, _, v_dst) =
    if List.mem (get_id v_dst) break_ids then raise Break
    else FuncTable.add result v_dst ()
  in
  try
    dfs g ~start_v:start_f ~on_edge;
    result
  with _ -> result

let get_reachables_from_func_id g funcs ~start_id ~break_ids =
  let result = FuncTable.create 100 in
  let start_nodes =
    FuncTable.fold
      (fun func _ acc -> if get_id func = start_id then func :: acc else acc)
      funcs []
  in
  let exception Break in
  let on_edge (_, _, v_dst) =
    if List.mem (get_id v_dst) break_ids then raise Break
    else FuncTable.add result v_dst ()
  in
  List.iter (fun func -> dfs g ~start_v:func ~on_edge) start_nodes;
  result
