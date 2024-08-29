[@@@ocaml.warnerror "-unused-extension"]

let failwithf fmt = Format.kasprintf failwith fmt

type cfg = { mutable ocaml_code : string; mutable ocaml_ident : string }

let config = { ocaml_code = ""; ocaml_ident = "" }
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
  let open Myast in
  let on_rtype key pargs body =
    let _ : Libsail.Type_check.tannot exp = body in
    (* printfn "%s %d" __FUNCTION__ __LINE__; *)
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

  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in
      (* let printfn fmt =
           Format.kasprintf (fun s -> Format.fprintf ppf "%s@ " s) fmt
         in *)
      printf "(* This file was auto generated *)@ ";
      printf "@[<v>@[type t =@]";
      printf "@[ | CI_hacky of string * int@]";
      printf "@[ | CI_default of string@]@]\n\n";

      printf "@[<v 0>";
      printf "@[let %s =@]@ " config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";
      collected
      |> Hashtbl.iter (fun key -> function
           | CI_default name ->
               printf "@[Hashtbl.add ans %S (CI_default %S);@]@," key name
           | CI_hacky (name, n) ->
               printf "@[Hashtbl.add ans %S (CI_hacky (%S, %d));@]@," key name n);
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
