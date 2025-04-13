(* open Core.Utils *)
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

let generate (funcs : (string list * 'a exp) FuncTable.t) ast =
  let open Libsail in
  let open Ast_util in
  let open Myast_iterator in
  let g = G.create () ~size:1500 in
  let rec helper src_func =
    let src_id = Func.get_id src_func in
    let classify_func_call id args =
      let speced =
        List.mapi (fun i e -> (i, e)) args
        |> List.filter (fun (_, e) -> Constant_fold.is_constant e)
      in
      if List.is_empty speced then F_usual id else F_specialized (id, speced)
    in
    let rec extract_arg = function
      | E_aux (E_app (Id_aux (Id f_id, _), args), _) -> (
          helper (classify_func_call f_id args);
          match args with
          | [ x ] -> extract_arg x
          (* write_single_element(... vd + to_bits(...), ...) *)
          | [
              E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
              E_aux (E_id (Id_aux (Id id, _)), _);
            ]
          | [
              E_aux (E_id (Id_aux (Id id, _)), _);
              E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
            ]
            when String.equal f_id "add_bits" ->
              id
          | _ -> "")
      | e -> string_of_exp e
    in
    let process src_args dst_func =
      match FuncTable.find_opt funcs dst_func with
      | Some (dst_args, _) ->
          let args2 =
            List.map2 (fun dst src -> (dst, extract_arg src)) dst_args src_args
          in
          G.add_edge_e g (dst_func, args2, src_func);
          helper dst_func
      | None -> (
          match FuncTable.find_opt funcs (F_usual (Func.get_id dst_func)) with
          | Some (dst_args, body) ->
              let () =
                let speced_body, _ =
                  let substs =
                    let speced_args =
                      match dst_func with
                      | F_specialized (_, speced) ->
                          (* printfn "%s %s" dst_id
                             (String.concat " "
                                (List.map
                                   (fun (a, b) ->
                                     string_of_int a ^ " : " ^ string_of_exp b)
                                   speced)); *)
                          List.map
                            (fun (i, e) -> (List.nth dst_args i |> mk_id, e))
                            speced
                      | _ -> assert false
                    in
                    Bindings.of_list speced_args
                  in
                  let ref_vars = Constant_propagation.referenced_vars body in
                  Myconstant_propagation.const_prop "" ast ref_vars
                    (substs, KBindings.empty) Bindings.empty body
                in
                FuncTable.add funcs dst_func (dst_args, speced_body)
              in
              let args2 =
                List.map2
                  (fun dst src -> (dst, extract_arg src))
                  dst_args src_args
              in
              G.add_edge_e g (dst_func, args2, src_func);
              helper dst_func
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
                process src_args (classify_func_call dst_id src_args)
            | E_app (Id_aux (Id dst_id, _), src_args) when dst_id <> src_id ->
                process src_args (classify_func_call dst_id src_args);
                default_iterator.exp_aux self e
            | _ -> default_iterator.exp_aux self e);
      }
    in
    match FuncTable.find_opt funcs src_func with
    | Some (_, body) -> it.exp it body
    | None -> ()
  in
  let () = FuncTable.iter (fun k _ -> helper k) funcs in
  (* let () =
       let on_edge : V.t * Edge.t * V.t -> unit =
        fun (v_src, args, v_dst) ->
         Format.printf "%s - " v_src;
         List.iter (fun (a, b) -> Format.printf "(%s, %s) " a b) args;
         Format.printf "- %s\n" v_dst
       in
       G.iter_edges_e on_edge g
     in *)
  g

let rec dfs g ~start_v ~on_edge =
  if G.mem_vertex g start_v then
    G.iter_succ_e
      (fun e ->
        on_edge e;
        dfs g
          ~start_v:
            (let _, _, v_dst = e in
             v_dst)
          ~on_edge)
      g start_v

let propogate_operands ~g ~aliases info =
  let result = Hashtbl.of_seq info in
  let on_edge : V.t * Edge.t * V.t -> unit =
   fun (v_src, args, v_dst) ->
    match Hashtbl.find_opt result v_src with
    | Some src_opers -> (
        let check_aliases func_id xs =
          match Hashtbl.find_opt aliases func_id with
          | Some aliases ->
              List.map
                (fun op ->
                  match List.assoc_opt op aliases with
                  | Some alias -> alias
                  | None -> op)
                xs
          | None -> xs
        in
        let mapped =
          args
          |> List.filter_map (fun (src, dst) ->
                 if List.mem src src_opers then
                   if String.equal dst "0b00000" then Some "v0" else Some dst
                 else None)
          |> check_aliases v_dst
        in
        match Hashtbl.find_opt result v_dst with
        | Some dst_opers -> Hashtbl.replace result v_dst (mapped @ dst_opers)
        | None -> Hashtbl.add result v_dst mapped
        (* if debug then (
           Format.printf "%s - " v_src;
           List.iter (fun (a, b) -> Format.printf "(%s, %s) " a b) args;
           Format.printf "- %s\n" v_dst;
           Format.printf "%s opers: %s\n" v_src (String.concat " " src_opers);
           Format.printf "mapped: %s\n" (String.concat " " mapped);
           Format.printf "%s opers: %s\n\n" v_dst
             (String.concat " " (Hashtbl.find result v_dst))) *))
    | None -> ()
  in
  let () = Seq.iter (fun (start_v, _) -> dfs g ~start_v ~on_edge) info in
  result
