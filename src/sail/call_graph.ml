open Checker_core
open Utils
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
      | E_aux (E_typ (_, e), _) -> extract_arg e
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
      let src_args =
        match src_args with [ E_aux (E_tuple xs, _) ] -> xs | _ -> src_args
      in
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

let propogate_operands ~g ~data_graphs funcs (info : (string * string) list) =
  let open Instruction in
  let result : Operand.t list FuncTable.t = FuncTable.create 1000 in
  G.iter_vertex
    (fun v ->
      match List.assoc_opt (get_id v) info with
      | Some op_name -> (
          match v with
          | F_usual _ -> FuncTable.add result v [ GPR op_name ]
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
                  | Some e -> FuncTable.add result v [ GPR e ]
                  | None -> FuncTable.add result v [ GPR op_name ])
              | None -> ()))
      | None -> ())
    g;

  let dummy_node = F_usual "dummy_node" in
  FuncTable.iter (fun k _ -> G.add_edge_e g (dummy_node, [], k)) result;

  let on_edge (v_src, args, _) acc =
    match FuncTable.find_opt result v_src with
    | Some src_opers ->
        let mapped =
          src_opers
          |> List.map (fun op ->
                 Operand.map
                   (fun op_ident ->
                     match List.assoc_opt op_ident args with
                     | Some id -> id
                     | None -> if op_ident = "0" then "v0" else op_ident)
                   op)
        in
        mapped @ acc
    | None -> acc
  in
  let analyze_pairs f opers =
    match FuncTable.find_opt data_graphs f with
    | Some data_g ->
        let op_srcs =
          List.map
            (fun op ->
              let op = Operand.get op in
              (op, Data_graph.find_sources data_g op))
            opers
        in
        (* find all unique sources *)
        op_srcs |> List.concat_map snd |> rm_duplicates
        (* for each source, find the dependent ones *)
        |> List.map (fun src ->
               let deps_regs =
                 List.filter_map
                   (fun (r, srcs) ->
                     if src <> r && List.mem src srcs then Some r else None)
                   op_srcs
               in
               (src, deps_regs))
        |> List.map (function
             | src, [ _; _ ] -> Operand.GPRPair src
             | src, _ -> Operand.GPR src)
    | None -> opers
  in
  let module Bfs = Graph.Traverse.Bfs (G) in
  Bfs.iter_component
    (fun cur_func ->
      let opers_before =
        match FuncTable.find_opt result cur_func with
        | Some opers -> opers
        | _ -> []
      in
      let opers =
        let is_gpr_pair = function Operand.GPRPair _ -> true | _ -> false in
        let opers = G.fold_pred_e on_edge g cur_func [] @ opers_before in
        let pairs = List.filter is_gpr_pair opers in
        let not_pairs = List.filter (fun op -> not (is_gpr_pair op)) opers in
        analyze_pairs cur_func not_pairs @ pairs
      in
      FuncTable.add result cur_func opers)
    g dummy_node;

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
  List.iter
    (fun func -> try dfs g ~start_v:func ~on_edge with _ -> ())
    start_nodes;
  result

let rec rev_dfs g ~start_v ~on_edge =
  G.iter_pred_e
    (fun e ->
      on_edge e;
      rev_dfs g
        ~start_v:
          (let v_src, _, _ = e in
           v_src)
        ~on_edge)
    g start_v

let analyze_csr g funcs =
  let open Libsail in
  let open Ast_util in
  let open Type_check in
  let open Myast_iterator in
  let csrs =
    let readCSR = F_usual "readCSR" in

    match FuncTable.find_opt funcs readCSR with
    | Some (_, readCSR_body) ->
        let csrs = ref [] in
        let add_csr id =
          if List.mem id !csrs then () else csrs := id :: !csrs
        in
        let it =
          {
            default_iterator with
            exp =
              (fun self (E_aux (e, annot) as exp) ->
                (match e with
                | E_id id
                  when match Env.lookup_id id (env_of_annot annot) with
                       | Register _ -> true
                       | _ -> false ->
                    add_csr id
                | _ -> ());
                default_iterator.exp self exp);
          }
        in
        it.exp it readCSR_body;

        let on_edge (v_src, _, _) =
          let _, body = FuncTable.find funcs v_src in
          it.exp it body
        in
        rev_dfs g ~start_v:readCSR ~on_edge;

        FuncTable.iter
          (fun func (_, body) ->
            if get_id func = "ext_read_CSR" then it.exp it body)
          funcs;

        !csrs
    | None -> []
  in

  let writers = FuncTable.create 500 in
  let readers = FuncTable.create 500 in
  FuncTable.iter
    (fun func (_, body) ->
      let it =
        {
          default_iterator with
          exp =
            (fun self (E_aux (e, annot) as exp) ->
              let is_csr_reg id =
                match Env.lookup_id id (env_of_annot annot) with
                | Register _
                  when List.exists (fun csr -> Id.compare csr id = 0) csrs ->
                    true
                | _ -> false
              in
              (match e with
              | E_assign (LE_aux ((LE_id id | LE_typ (_, id)), _), _)
              | E_ref id
                when is_csr_reg id ->
                  FuncTable.add writers func id
              | E_id id when is_csr_reg id -> FuncTable.add readers func id
              | _ -> ());
              default_iterator.exp self exp);
        }
      in
      it.exp it body)
    funcs;

  let res_writers = FuncTable.create 1000 in
  let res_readers = FuncTable.create 1000 in
  let f info res =
    FuncTable.iter
      (fun func reg ->
        let reachs =
          get_reachables_from_func_id g funcs ~start_id:(Func.get_id func)
            ~break_ids:[]
        in
        FuncTable.iter
          (fun func () ->
            match FuncTable.find_opt res func with
            | Some regs ->
                if List.exists (fun r -> Id.compare r reg = 0) regs then ()
                else FuncTable.replace res func (reg :: regs)
            | None -> FuncTable.add res func [ reg ])
          reachs)
      info
  in
  f readers res_readers;
  f writers res_writers;

  (res_writers, res_readers)
