module V = struct
  type t = string

  let make = Fun.id
  let compare = Stdlib.compare
  let equal = Stdlib.( = )
  let hash = Stdlib.Hashtbl.hash
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

    let vertex_name = Fun.id
    let get_subgraph _ = None
    let vertex_attributes _ = []
    let graph_attributes _ = []
    let edge_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
  end) in
  Out_channel.with_open_text outfile (fun ch -> Dot.output_graph ch g)

let generate funcs_tbl =
  let g = G.create () in
  let rec helper src_id =
    let open Myast in
    let open Myast_iterator in
    let rec extract_arg = function
      | E_aux (E_id (Id_aux (Id id, _)), _) -> id
      | E_aux (E_lit (L_aux (L_unit, _)), _) -> "()"
      | E_aux (E_lit (L_aux (L_num n, _)), _) -> Z.to_string n
      | E_aux (E_lit (L_aux (L_true, _)), _) -> "L_true"
      | E_aux (E_lit (L_aux (L_false, _)), _) -> "L_false"
      | E_aux (E_lit (L_aux (L_bin bn, _)), _) -> "0b" ^ bn
      | E_aux (E_lit (L_aux (L_string s, _)), _) -> s
      | E_aux (E_app (Id_aux (Id f_id, _), args), _) -> (
          helper f_id;
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
      | _ ->
          (* Format.printf "%a\n"
               (Myast.pp_exp Myast.pp_tannot)
               src;
             Format.printf
               "Arg's extraction not implemented for %s %s\n"
               cur_v id; *)
          ""
    in
    let f src_args dst_id =
      match Hashtbl.find_opt funcs_tbl dst_id with
      | Some (dst_args, _) ->
          let args =
            List.map2 (fun dst src -> (dst, extract_arg src)) dst_args src_args
          in

          G.add_vertex g src_id;
          G.add_vertex g dst_id;
          G.add_edge_e g (dst_id, args, src_id);

          helper dst_id
      | None -> ()
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
                f src_args dst_id
            | E_app (Id_aux (Id dst_id, _), src_args)
              when not @@ String.equal dst_id src_id ->
                f src_args dst_id;
                default_iterator.exp_aux self e
            | _ -> default_iterator.exp_aux self e);
      }
    in
    match Hashtbl.find_opt funcs_tbl src_id with
    | Some (_, body) -> it.exp it body
    | None -> ()
  in
  let () = Hashtbl.iter (fun id _ -> helper id) funcs_tbl in
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
        | Some dst_opers ->
            Hashtbl.replace result v_dst
              (List.sort_uniq compare (mapped @ dst_opers))
        | None -> Hashtbl.add result v_dst mapped
        (* Format.printf "%s - " v_src;
           List.iter (fun (a, b) -> Format.printf "(%s, %s) " a b) args;
           Format.printf "- %s\n" v_dst;
           Format.printf "%s QWE: %s\n" v_src (String.concat " " qwe);
           Format.printf "%s OPERS: %s\n" v_src (String.concat " " ins);
           Format.printf "mapped: %s\n" (String.concat " " mapped);
           Format.printf "%s OPERS: %s\n\n" v_dst
             (String.concat " " (Hashtbl.find result v_dst)) *))
    | None -> ()
  in
  let () = Seq.iter (fun (start_v, _) -> dfs g ~start_v ~on_edge) info in
  result
