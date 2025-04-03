open Core.Utils

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

open Myast
open Specialize

let generate1
    (funcs_tbl :
      ( string,
        (string * Myast.typ) list * Libsail.Type_check.tannot Myast.exp )
      Hashtbl.t) =
  let g = G.create () in
  let tabl :
      ( func,
        (string * Myast.typ) list * Libsail.Type_check.tannot Myast.exp )
      Hashtbl.t =
    Hashtbl.create 10
  in
  let () =
    Hashtbl.iter
      (fun id (args, body) ->
        let args_to_spec =
          (* need to specialize? e.g (ITYPE (imm, rs1, rd, op)) *)
          List.filter_map
            (fun (arg, typ) ->
              match typ with
              | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
              | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _)
                when Char.uppercase_ascii arg.[0] != arg.[0] -> (
                  match Enums.find_opt typ_id with
                  | Some vals -> Some (arg, typ, vals)
                  | None -> None)
              | _ -> None)
            args
        in
        if List.is_empty args_to_spec then
          Hashtbl.add tabl (F_usual id) (args, body)
        else
          (* let args_to_spec = List.map fst args_to_spec in
             let specialize_by_args args_to_spec =
               let rec specialize acc remaining_args current_body =
                 match remaining_args with
                 | [] -> [ (acc, current_body) ]
                 | arg :: rest_args -> (
                     match specialize_match_by_id arg current_body with
                     | None -> specialize acc rest_args current_body
                     | Some cases ->
                         List.concat_map
                           (fun (case_id, specialized_body) ->
                             specialize ((arg, case_id) :: acc) rest_args
                               specialized_body)
                           cases)
               in
               specialize [] args_to_spec body
             in
             let _ = specialize_by_args args_to_spec in *)
          let arg_to_spec, typ, vals_to_spec = List.hd args_to_spec in
          let specs =
            specialize_match_by_id arg_to_spec typ vals_to_spec body
          in
          List.iter
            (fun (spec_val, body) ->
              Hashtbl.add tabl
                (F_specialized (id, [ (1, spec_val) ]))
                (args, body))
            specs)
      funcs_tbl
  in

  (* Hashtbl.iter
     (fun k _ ->
       match k with
       | F_regular id -> printfn "Regular %s\n" id
       | F_specialized (id, args) ->
           printfn "Specialized %s" id;
           List.iter (fun (arg, value) -> printfn "%s -> %s" arg value) args;
           printfn "")
     tabl; *)
  let g = G.create () in
  let rec helper (src_id : func) =
    let open Myast in
    let open Myast_iterator in
    let extract_arg = function
      | E_aux (E_id (Id_aux (Id id, _)), _) -> id
      | E_aux (E_lit (L_aux (L_unit, _)), _) -> "()"
      | E_aux (E_lit (L_aux (L_num n, _)), _) -> Z.to_string n
      | E_aux (E_lit (L_aux (L_true, _)), _) -> "L_true"
      | E_aux (E_lit (L_aux (L_false, _)), _) -> "L_false"
      | E_aux (E_lit (L_aux (L_bin bn, _)), _) -> "0b" ^ bn
      | E_aux (E_lit (L_aux (L_string s, _)), _) -> s
      | E_aux (E_app (Id_aux (Id f_id, _), args), _) ->
          "123123123123"
          (* helper f_id;
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
             | _ -> "" *)
      | _ -> ""
    in
    let extract_typ (tannot : Libsail.Type_check.tannot) =
      match Obj.magic tannot with
      | Some { env; typ; _ }, _ -> typ
      | _ -> assert false
    in
    let extract_arg_with_typ = function
      | E_aux (E_id (Id_aux (Id id, _)), (_, tannot)) -> (id, extract_typ tannot)
      | E_aux (E_lit (L_aux (L_unit, _)), (_, tannot)) ->
          ("()", extract_typ tannot)
      | E_aux (E_lit (L_aux (L_num n, _)), (_, tannot)) ->
          (Z.to_string n, extract_typ tannot)
      | E_aux (E_lit (L_aux (L_true, _)), (_, tannot)) ->
          ("L_true", extract_typ tannot)
      | E_aux (E_lit (L_aux (L_false, _)), (_, tannot)) ->
          ("L_false", extract_typ tannot)
      | E_aux (E_lit (L_aux (L_bin bn, _)), (_, tannot)) ->
          ("0b" ^ bn, extract_typ tannot)
      | E_aux (E_lit (L_aux (L_string s, _)), (_, tannot)) ->
          (s, extract_typ tannot)
      | E_aux (E_app (Id_aux (Id f_id, _), args), (_, tannot)) ->
          ("123123123123", extract_typ tannot)
          (* helper f_id;
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
             | _ -> "" *)
      | _ -> ("", Libsail.Ast_util.unknown_typ)
    in
    let f src_args dst_id =
      match Hashtbl.find_opt funcs_tbl dst_id with
      | Some (dst_args, _) ->
          let src_args1 = List.map extract_arg src_args in
          let args =
            List.map2 (fun dst src -> (fst dst, src)) dst_args src_args1
          in

          let src_id =
            match src_id with F_usual id -> id | F_specialized (id, _) -> id
          in

          G.add_vertex g src_id;
          G.add_vertex g dst_id;
          G.add_edge_e g (dst_id, args, src_id);

          let src_args = List.map extract_arg_with_typ src_args in
          let speced_arg =
            (* need to specialize? e.g (ITYPE (imm, rs1, rd, op)) *)
            List.filter_map
              (fun (arg, typ) ->
                match typ with
                | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _)
                  when Char.uppercase_ascii arg.[0] != arg.[0] -> (
                    match Enums.find_opt typ_id with
                    | Some vals -> Some arg
                    | None -> None)
                | _ -> None)
              src_args
          in
          if List.is_empty speced_arg then helper (F_usual dst_id)
          else
            let speced = List.hd speced_arg in
            helper
              (F_specialized
                 ( dst_id,
                   [
                     ( (* List.find (fun (a, b) -> String.equal speced b) args
                          |> fst *)
                       1,
                       speced );
                   ] ))
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
            (* when not @@ String.equal dst_id src_id *) ->
                let qwe =
                  match src_id with
                  | F_usual id -> id
                  | F_specialized (id, _) -> id
                in
                if not @@ String.equal dst_id qwe then (
                  f src_args dst_id;
                  default_iterator.exp_aux self e)
            | _ -> default_iterator.exp_aux self e);
      }
    in
    match Hashtbl.find_opt tabl src_id with
    | Some (_, body) ->
        it.exp it body
        (* let args_to_spec =
             (* need to specialize? e.g (ITYPE (imm, rs1, rd, op)) *)
             List.filter_map
               (fun (arg, typ) ->
                 match typ with
                 | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                 | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) -> (
                     match Enums.find_opt typ_id with
                     | Some vals -> Some (arg, typ, vals)
                     | None -> None)
                 | _ -> assert false)
               args
           in
           if List.is_empty args_to_spec then it.exp it body
           else
             (* let args_to_spec = List.map fst args_to_spec in
                let specialize_by_args args_to_spec =
                  let rec specialize acc remaining_args current_body =
                    match remaining_args with
                    | [] -> [ (acc, current_body) ]
                    | arg :: rest_args -> (
                        match specialize_match_by_id arg current_body with
                        | None -> specialize acc rest_args current_body
                        | Some cases ->
                            List.concat_map
                              (fun (case_id, specialized_body) ->
                                specialize ((arg, case_id) :: acc) rest_args
                                  specialized_body)
                              cases)
                  in
                  specialize [] args_to_spec body
                in
                let _ = specialize_by_args args_to_spec in *)
             let arg_to_spec, typ, vals_to_spec = List.hd args_to_spec in
             let specs =
               specialize_match_by_id arg_to_spec typ vals_to_spec body
             in
             List.iter (fun (spec_val, body) -> it.exp it body) specs *)
    | None -> ()
  in
  let () = Hashtbl.iter (fun id _ -> helper id) tabl in
  g

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
      | _ -> ""
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
    | Some src_opers ->
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
        (match Hashtbl.find_opt result v_dst with
        | Some dst_opers ->
            Hashtbl.replace result v_dst
              (List.sort_uniq compare (mapped @ dst_opers))
        | None -> Hashtbl.add result v_dst mapped);

        if debug then (
          Format.printf "%s - " v_src;
          List.iter (fun (a, b) -> Format.printf "(%s, %s) " a b) args;
          Format.printf "- %s\n" v_dst;
          Format.printf "%s opers: %s\n" v_src (String.concat " " src_opers);
          Format.printf "mapped: %s\n" (String.concat " " mapped);
          Format.printf "%s opers: %s\n\n" v_dst
            (String.concat " " (Hashtbl.find result v_dst)))
    | None -> ()
  in
  let () = Seq.iter (fun (start_v, _) -> dfs g ~start_v ~on_edge) info in
  result
