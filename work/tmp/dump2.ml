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

let dump_execute jfile =
  let ast =
    let j = In_channel.with_open_text jfile Yojson.Safe.from_channel in
    match Myast.def_of_yojson Myast.tannot_of_yojson j with
    | Result.Error err ->
        Format.eprintf "Error: %s\n%!" err;
        exit 1
    | Ok ast -> ast
  in

  let collected = ref [] in
  let collected_hacky = ref [] in
  let open Myast in
  let on_rtype key body =
    let _ : Libsail.Type_check.tannot exp = body in
    (* printfn "%s %d" __FUNCTION__ __LINE__; *)
    match has_right_match body with
    | None ->
        (* Format.printf "The clause for %S has NOT right match\n%!" key; *)
        ()
    | Some args ->
        (* Format.printf "The clause for %S HAS right match\n%!" key; *)
        ListLabels.iter args ~f:(function
          | Pat_aux (Pat_exp (P_aux (P_id (Id_aux (Id name, _)), _), _), _) ->
              collected_hacky := name :: !collected_hacky
          | _ -> ())
    (* match body with
       | E_aux (E_block [ E_aux (E_let (LB_aux (LB_val (_, _), _), e2), _) ], _)
         -> (
           match e2 with
           | E_aux (E_block [ E_aux (E_let (LB_aux (LB_val (_, _), _), e2), _) ], _)
             -> (
               match e2 with
               | E_aux
                   (E_block [ E_aux (E_let (LB_aux (LB_val (_, e3), _), _), _) ], _)
                 -> (
                   printfn "@[%a@]@," (Myast.pp_exp Myast.pp_tannot) e3;
                   match e3 with
                   | E_aux (E_match (_, xs), _) ->
                       ListLabels.iter xs ~f:(function
                         | Pat_aux
                             (Pat_exp (P_aux (P_id (Id_aux (Id name, _)), _), _), _)
                           ->
                             collected_hacky := name :: !collected_hacky
                         | _ -> ())
                   | _ -> assert false)
               | _ -> assert false)
           | _ -> assert false)
       | _ -> assert false *)
    (* printfn "@[%a@]@," (Myast.pp_exp Myast.pp_tannot) body *)
  in

  Out_channel.with_open_text "06159dump.txt" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      Format.pp_set_max_indent ppf 1000;
      let printfn fmt = Format.kasprintf (Format.pp_print_string ppf) fmt in

      match ast with
      | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, body), _)), _) ->
          printfn "body length = %d\n@," (List.length body);
          List.iter
            (function
              | FCL_aux (FCL_funcl (Id_aux (Id _, _), fdecl), _) -> (
                  match fdecl with
                  | Pat_aux
                      ( Pat_exp
                          ( P_aux
                              ((P_app (Id_aux (Id name, _), pargs) as aux), _),
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
                          ] )
                      (* when String.starts_with ~prefix:"RISCV_C" id  *) ->
                          (* let _ = failwithf "From %S get instr %s" name id in *)
                          collected := id :: !collected
                      | _ ->
                          collected := name :: !collected;
                          printfn "@[%s: %a@]@," name
                            (pp_pat_aux Myast.pp_tannot)
                            aux;
                          on_rtype name exp)
                  | _ -> assert false)
              | _ -> assert false)
            body
      | _ -> assert false);

  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  Out_channel.with_open_text config.ocaml_code (fun ch ->
      Printf.fprintf ch "(* This file was auto generated *)\n\n";
      Printf.fprintf ch "let %s = [ " config.ocaml_ident;
      ListLabels.iter !collected ~f:(fun s -> Printf.fprintf ch "%S; " s);
      Printf.fprintf ch "]\n\n";

      Printf.fprintf ch "let %s_hacky = [ " config.ocaml_ident;
      ListLabels.iter !collected_hacky ~f:(fun s -> Printf.fprintf ch "%S; " s);
      Printf.fprintf ch "]\n\n")

let () =
  Arg.parse
    [
      ("-dump-execute", Arg.String (fun s -> dump_execute s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun _ -> assert false)
    ""
