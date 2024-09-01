[@@@ocaml.warnerror "-unused-extension"]

let failwithf fmt = Format.kasprintf failwith fmt

let log fmt =
  if true then Format.kasprintf print_endline fmt
  else Format.ikfprintf (fun _ -> ()) Format.std_formatter fmt

let loge fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt

type cfg = {
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config = { ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }
let printfn ppf = Format.kasprintf print_endline ppf

include struct
  open Myast_iterator
  open Myast

  exception Match_op_found of Libsail.Type_check.tannot pexp_funcl list

  let has_match =
    {
      default_iterator with
      exp_aux =
        (fun self e ->
          match e with
          | E_match (E_aux (E_id (Id_aux (Id "op", _)), _), xs) ->
              raise (Match_op_found xs)
          | _ -> default_iterator.exp_aux self e);
    }

  let has_right_match expr =
    try
      has_match.exp has_match expr;
      None
    with Match_op_found xs -> Some xs
end

module Collect_out_info = struct
  open Myast_iterator
  open Myast

  module V = struct
    type t = string

    let compare = Stdlib.compare
    let equal = Stdlib.( = )
    let hash = Stdlib.Hashtbl.hash
  end

  module Edge = struct
    type t = string option list

    let compare = Stdlib.compare
    let equal = Stdlib.( = )
    let hash = Stdlib.Hashtbl.hash
    let default = []
    let make = Fun.id
  end

  module G = Graph.Imperative.Digraph.ConcreteLabeled (V) (Edge)

  let g = G.create ()

  let make_iterator curV is_right_opnd register_assmt =
    {
      default_iterator with
      exp_aux =
        (fun self -> function
          | E_app
              ( Id_aux
                  ( Id
                      ( "wX_bits" | "wF_or_X_D" | "wF_or_X_H" | "wF_or_X_S"
                      | "wF_S" | "wF_D" ),
                    _ ),
                [ E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              register_assmt rd
          | E_app
              ( Id_aux (Id "write_vreg", _),
                [ _; _; _; E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              register_assmt rd
          | E_app
              ( Id_aux (Id "write_vmask", _),
                [ _; E_aux (E_id (Id_aux (Id rd, _)), _); _ ] )
            when is_right_opnd rd ->
              register_assmt rd
          | E_app
              ( Id_aux (Id "execute", _),
                [
                  E_aux
                    ( E_app (Id_aux (Id rtype, _), [ E_aux (E_tuple args, _) ]),
                      _ );
                ] ) as e ->
              (* log "%s %d rtype = %S, argc = %d" __FILE__ __LINE__ rtype
                 (List.length args); *)
              let spec_args =
                List.map
                  (function
                    | E_aux (E_id (Id_aux (Id id, _)), _) ->
                        log "%s %d arg.id = %S" __FILE__ __LINE__ id;
                        if Char.uppercase_ascii id.[0] = id.[0] then Some id
                        else None
                    | _ -> None)
                  args
              in
              G.add_vertex g curV;
              G.add_vertex g rtype;
              G.add_edge_e g (rtype, Edge.make spec_args, curV);
              default_iterator.exp_aux self e
          | e -> default_iterator.exp_aux self e);
    }

  let dump_graph () =
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
    Out_channel.with_open_text config.dot_file (fun ch -> Dot.output_graph ch g)
end

type collected_info =
  | CI_hacky of string * int (* Cname + index of 'op' constructor*)
  | CI_default of string

let dump_execute jfile =
  let ast =
    let j = In_channel.with_open_text jfile Yojson.Safe.from_channel in
    match Myast.def_of_yojson Myast.tannot_of_yojson j with
    | Result.Error err ->
        Format.eprintf "Error: %s\n%!" err;
        exit 1
    | Ok ast -> ast
  in

  let collected : (string, collected_info) Hashtbl.t = Hashtbl.create 100 in
  let weird_stuff : (string, unit) Hashtbl.t = Hashtbl.create 100 in
  let out_info : (string, string list) Hashtbl.t = Hashtbl.create 100 in
  let open Myast in
  let on_rtype key pargs body =
    let _ : Libsail.Type_check.tannot exp = body in
    (* printfn "%s %d" __FUNCTION__ __LINE__; *)
    let args_idents =
      match pargs with
      | [ P_aux (P_tuple ps, _) ] ->
          List.filter_map
            (function
              | P_aux (P_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
            ps
      | _ -> []
    in
    let () =
      let out_args = ref [] in
      let iterator =
        Collect_out_info.make_iterator key
          (fun _ -> true)
          (fun s -> out_args := s :: !out_args)
      in
      iterator.exp iterator body;
      Hashtbl.add out_info key !out_args
    in
    let argidx =
      let exception Arg_found of int in
      match pargs with
      | [ P_aux (P_tuple ps, _) ] -> (
          try
            List.iteri
              (fun i -> function
                | P_aux (P_id (Id_aux (Id "op", _)), _) -> raise (Arg_found i)
                | _ -> ())
              ps;
            -1
          with Arg_found n -> n)
      | _ -> -1
    in
    match has_right_match body with
    | None when argidx = -1 -> Hashtbl.add collected key (CI_default key)
    | None ->
        (* failwithf "can't decide what to do when key = %S" key *)
        Hashtbl.add weird_stuff key ()
    | Some args ->
        (* Format.printf "The clause for %S HAS right match\n%!" key; *)
        ListLabels.iter args ~f:(function
          | Pat_aux (Pat_exp (P_aux (P_id (Id_aux (Id name, _)), _), _), _) ->
              Hashtbl.add collected name (CI_hacky (key, argidx))
          (* collected_hacky := name :: !collected_hacky *)
          | _ -> ())
  in

  Out_channel.with_open_text "06159dump.txt" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      Format.pp_set_max_indent ppf 1000;

      match ast with
      | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, body), _)), _) ->
          printfn "body length = %d\n@," (List.length body);
          ListLabels.iter body ~f:(function
            | FCL_aux (FCL_funcl (Id_aux (Id _, _), fdecl), _) -> (
                match fdecl with
                | Pat_aux
                    ( Pat_exp
                        ( P_aux ((P_app (Id_aux (Id name, _), pargs) as aux), _),
                          exp ),
                      _ ) -> (
                    match (name, pargs) with
                    | ( "ZICOND_RTYPE",
                        [
                          P_aux
                            ( P_tuple
                                [
                                  P_aux (P_id _, _);
                                  P_aux (P_id _, _);
                                  P_aux (P_id _, _);
                                  P_aux (P_id (Id_aux (Id id, _)), _);
                                ],
                              _ );
                        ] ) ->
                        Hashtbl.add collected id (CI_hacky (name, 4))
                    | _ ->
                        printfn "@[%s: %a@]@," name
                          (pp_pat_aux Myast.pp_tannot)
                          aux;
                        on_rtype name pargs exp)
                | _ -> assert false)
            | _ -> assert false)
      | _ -> assert false);

  Collect_out_info.dump_graph ();
  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in
      (* let printfn fmt =
           Format.kasprintf (fun s -> Format.fprintf ppf "%s@ " s) fmt
         in *)
      printf "(* This file was auto generated *)@ ";
      printf "@[<v>@[type t =@]@ ";
      printf "@[ | CI_hacky of string * int@]@ ";
      printf "@[ | CI_default of string@]@ ";
      printf "@]";
      printf "@[type info = { out: string list }@]@ ";
      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";
      printf
        "@[let def k name info = Hashtbl.add ans (CI_default name) info in@]@ ";
      printf
        "@[let hacky k name arity info = Hashtbl.add ans \
         (CI_hacky(name,arity)) info in@]@ ";

      collected
      |> Hashtbl.iter (fun key ->
             let out =
               match Hashtbl.find out_info key with
               | xs -> xs
               | exception Not_found -> []
             in
             let out_str ppf out =
               Format.pp_print_list
                 ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
                 (fun ppf -> Format.fprintf ppf "%S")
                 ppf out
             in
             function
             | CI_default name ->
                 printf "@[def %S %S {out=[%a]};@]@," key name out_str out
             | CI_hacky (name, n) ->
                 printf "@[hacky %S %S %d {out=[%a]};@]@," key name n out_str
                   out);
      printf "@[ans@]";
      printf "@]@ ";
      Format.pp_print_cut ppf ();

      printf "let %s_hacky = [ " config.ocaml_ident;
      weird_stuff |> Hashtbl.iter (fun s () -> printf "%S; " s);
      printf "]\n\n%!")

let () =
  Arg.parse
    [
      ("-dump-execute", Arg.String (fun s -> dump_execute s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> failwithf "Bad argument: %S" s)
    ""
