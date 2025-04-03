open Core.Utils
open Myast_iterator
open Myast

type cfg = {
  mutable sail_json : string;
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
  mutable dot_file : string;
}

let config =
  { sail_json = ""; ocaml_code = ""; ocaml_ident = ""; dot_file = "graph.dot" }

let is_name_for_tracing = function "run" -> true | _ -> false

let sail_json =
  lazy
    (match
       In_channel.with_open_text config.sail_json Yojson.Safe.from_channel
     with
    | `List xs ->
        List.map
          (fun j ->
            match def_of_yojson tannot_of_yojson j with
            | Result.Ok def -> def
            | _ -> assert false)
          xs
    | _ -> assert false)

let funcs : (string, string list * Libsail.Type_check.tannot exp) Hashtbl.t =
  Hashtbl.create 2000

let aliases : (string, (string * string) list) Hashtbl.t = Hashtbl.create 500

let collect_aliases body add_alias =
  let it =
    {
      default_iterator with
      exp_aux =
        (fun self e ->
          (match e with
          | E_let
              ( LB_aux
                  ( LB_val
                      ( P_aux (P_id (Id_aux (Id alias, _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (Id "creg2reg_idx", _),
                                [ E_aux (E_id (Id_aux (Id arg, _)), _) ] ),
                            _ ) ),
                    _ ),
                _ )
          | E_let
              ( LB_aux
                  ( LB_val
                      ( P_aux
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                                [ _; E_aux (E_id (Id_aux (Id arg, _)), _) ] ),
                            _ ) ),
                    _ ),
                _ ) ->
              add_alias alias arg
          | E_let
              ( LB_aux
                  ( LB_val
                      ( P_aux
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (Id ("zero_extend" | "sign_extend"), _),
                                [
                                  _;
                                  E_aux
                                    ( E_app
                                        (Id_aux (Id "bitvector_concat", _), xs),
                                      _ );
                                ] ),
                            _ ) ),
                    _ ),
                _ ) -> (
              match
                List.find_map
                  (function
                    | E_aux (E_id (Id_aux (Id id, _)), _) -> Some id | _ -> None)
                  xs
              with
              | Some arg -> add_alias alias arg
              | None -> ())
          | E_let
              ( LB_aux
                  ( LB_val
                      ( P_aux
                          (P_typ (_, P_aux (P_id (Id_aux (Id alias, _)), _)), _),
                        E_aux
                          ( E_app
                              ( Id_aux (Id "add_bits", _),
                                [
                                  E_aux (E_id (Id_aux (Id arg, _)), _);
                                  E_aux (E_app (Id_aux (Id "to_bits", _), _), _);
                                ] ),
                            _ ) ),
                    _ ),
                _ ) ->
              add_alias alias arg
          | _ -> ());
          default_iterator.exp_aux self e);
    }
  in
  it.exp it body

let specialize_match_by_id target_id typ vals_to_spec body =
  let specialize_case pat_id =
    let open Libsail.Rewriter in
    let rewrite_match =
      {
        rewriters_base with
        rewrite_exp =
          (fun self e ->
            match e with
            | E_aux (E_match (E_aux (E_id (Id_aux (Id id, _)), _), pexps), _)
              when String.equal id target_id -> (
                match
                  List.find_map
                    (function
                      | Pat_aux
                          ( Pat_exp
                              (P_aux (P_id (Id_aux (Id id, _)), _), pat_body),
                            _ )
                        when String.equal pat_id id ->
                          Some pat_body
                      | _ -> None)
                    pexps
                with
                | Some exp -> exp
                | None ->
                    Option.get
                    @@ List.find_map
                         (function
                           | Pat_aux (Pat_exp (P_aux (P_wild, _), pat_body), _)
                             ->
                               Some pat_body
                           | _ -> None)
                         pexps)
            | _ -> rewriters_base.rewrite_exp self e);
      }
    in
    let rewrite_target_id =
      {
        rewriters_base with
        rewrite_exp =
          (fun self e ->
            match e with
            | E_aux (E_id (Id_aux (Id id, l)), _) when String.equal id target_id
              ->
                E_aux
                  ( E_id (Id_aux (Id pat_id, l)),
                    ( Unknown,
                      Libsail.Type_check.mk_tannot Libsail.Type_check.Env.empty
                        typ ) )
            | _ -> rewriters_base.rewrite_exp self e);
      }
    in
    (* rewrite_match.rewrite_exp rewrite_match body *)
    let rewrited_body = rewrite_match.rewrite_exp rewrite_match body in
    rewrite_target_id.rewrite_exp rewrite_target_id rewrited_body
  in
  List.map (fun id -> (id, specialize_case id)) vals_to_spec

open Specialize

let dump_execute () =
  let executes :
      ( Specialize.func,
        (string * Myast.typ) list * Libsail.Type_check.tannot Myast.exp )
      Hashtbl.t =
    Hashtbl.create 500
  in
  let () =
    List.iter
      (function
        | DEF_aux (DEF_fundef (FD_aux (FD_function (_, _, funcls), _)), _) ->
            let extract_args_with_typs pargs =
              let extract_typ (tannot : Libsail.Type_check.tannot) =
                match Obj.magic tannot with
                | Some { env; typ; _ }, _ -> typ
                | _ -> assert false
              in
              let rec helper = function
                | P_id (Id_aux (Id id, _)) -> id
                | P_wild -> "_"
                | P_lit (L_aux (L_hex n, _)) -> "0x" ^ n
                | P_lit (L_aux (L_unit, _)) -> "unit"
                | P_struct _ -> "P_struct"
                | P_var (P_aux (paux, _), _) -> helper paux
                | P_typ (_, P_aux (paux, _)) -> helper paux
                | _ ->
                    Format.printf "%a" (pp_pat pp_tannot) pargs;
                    failwithf "Arg's extraction not implemented\n"
              in
              match pargs with
              | P_aux (P_tuple args, _) ->
                  List.map
                    (function
                      | P_aux (paux, (_, tannot)) ->
                          (helper paux, extract_typ tannot))
                    args
              | P_aux (P_var (P_aux (paux, _), _), (_, tannot)) ->
                  [ (helper paux, extract_typ tannot) ]
              | P_aux (P_typ (typ, P_aux (paux, _)), _) ->
                  [ (helper paux, typ) ]
              | P_aux (p, (_, tannot)) -> [ (helper p, extract_typ tannot) ]
            in
            (* let extract_args pargs =
                 let args_with_typs = extract_args_with_typs pargs in
                 let args = List.map fst args_with_typs in
                 ( args,
                   fun arg ->
                     Option.get
                     @@ List.find_map
                          (fun (s, typ) ->
                            if String.equal arg s then Some typ else None)
                          args_with_typs )
               in *)
            List.iter
              (function
                | FCL_aux
                    ( FCL_funcl
                        ( Id_aux (Id "execute", _),
                          Pat_aux
                            ( Pat_exp
                                ( P_aux (P_app (Id_aux (Id id, _), [ parg ]), _),
                                  body ),
                              _ ) ),
                      _ ) ->
                    (* if is_name_for_tracing id then
                       printfn "@[%s: %a@]@," id (pp_funcl_aux pp_tannot) exec; *)
                    let args = extract_args_with_typs parg in

                    let argsi = List.mapi (fun i (a, b) -> (i, a, b)) args in

                    let speced_args =
                      List.filter_map
                        (fun (i, arg, typ) ->
                          match typ with
                          | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                          | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                              if
                                Char.uppercase_ascii arg.[0] = arg.[0]
                                && Enums.mem typ_id
                              then Some (i, arg)
                              else None
                          | _ -> None)
                        argsi
                    in

                    let args_to_spec =
                      List.filter_map
                        (fun (i, arg, typ) ->
                          match typ with
                          | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                          | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                              if
                                Char.uppercase_ascii arg.[0] != arg.[0]
                                && Enums.mem typ_id
                              then Some (i, arg, typ)
                              else None
                          | _ -> None)
                        argsi
                    in

                    (if List.is_empty args_to_spec then
                       if List.is_empty speced_args then
                         Hashtbl.add executes (F_usual id) (args, body)
                       else
                         Hashtbl.add executes
                           (F_specialized (id, speced_args))
                           (args, body)
                     else
                       let i, arg_to_spec, typ = List.hd args_to_spec in
                       let vals_to_spec =
                         match typ with
                         | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                         | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                             Enums.find_exn typ_id
                         | _ -> assert false
                       in
                       let specs =
                         specialize_match_by_id arg_to_spec typ vals_to_spec
                           body
                       in
                       List.iter
                         (fun (spec_val, body) ->
                           Hashtbl.add executes
                             (F_specialized (id, [ (i, spec_val) ]))
                             (args, body))
                         specs);

                    (*  *)
                    Hashtbl.add funcs id (List.map fst args, body)
                | FCL_aux
                    ( (FCL_funcl
                         ( Id_aux (Id id, Range ({ pos_fname = path; _ }, _)),
                           Pat_aux (Pat_exp (parg, body), _) ) as funcl),
                      _ )
                (* when String.starts_with path ~prefix:"../../sail-riscv" *) ->
                    if is_name_for_tracing id then (
                      (* printfn "@[%s: %a@]@," id (pp_funcl_aux pp_tannot) funcl; *)
                      let defs = Lazy.force sail_json in
                      let ast = { Libsail.Ast_defs.empty_ast with defs } in
                      let open Libsail in
                      let open Ast_util in
                      let trash1 =
                        Bindings.of_list
                          [
                            ( Id_aux (Id "x", Generated Unknown),
                              E_aux
                                ( E_id (mk_id "One"),
                                  ( Generated Unknown,
                                    Type_check.mk_tannot
                                      (Type_check.env_of body)
                                      (Typ_aux
                                         ( Typ_id
                                             (Id_aux
                                                (Id "int", Generated Unknown)),
                                           Generated Unknown )) ) ) );
                          ]
                      in
                      let trash =
                        Bindings.of_list
                          [
                            ( Id_aux (Id "x", Generated Unknown),
                              E_aux
                                ( E_lit
                                    (L_aux
                                       ( L_num (Nat_big_num.of_int 1),
                                         Generated Unknown )),
                                  ( Generated Unknown,
                                    Type_check.mk_tannot
                                      (Type_check.env_of body)
                                      (Typ_aux
                                         ( Typ_id
                                             (Id_aux
                                                (Id "int", Generated Unknown)),
                                           Generated Unknown )) ) ) );
                          ]
                      in
                      Bindings.iter
                        (fun id e ->
                          let id =
                            match id with
                            | Id_aux (Id id, _) -> id
                            | _ -> assert false
                          in
                          printfn "@[%s: %a@]@," id (pp_exp pp_tannot) e)
                        trash;
                      let ref_vars =
                        Libsail.Constant_propagation.referenced_vars body
                      in
                      printfn "@[len ref vars %d@]@," (IdSet.cardinal ref_vars);
                      IdSet.iter
                        (fun id ->
                          printfn "@[ref vars %s@]@," (Ast_util.string_of_id id))
                        ref_vars;

                      let new_body, binds =
                        let open Libsail.Constant_propagation in
                        const_prop "interpreter" ast (referenced_vars body)
                          (trash, KBindings.empty) Bindings.empty body
                      in
                      (* let new_body, binds =
                           let open Libsail.Constant_propagation in
                           const_prop "target" ast (referenced_vars body)
                             (trash, KBindings.empty) Bindings.empty new_body
                         in *)
                      printfn "@[%s: %a@]@," id (pp_exp pp_tannot) body;
                      Bindings.iter
                        (fun id e ->
                          let id =
                            match id with
                            | Id_aux (Id id, _) -> id
                            | _ -> assert false
                          in
                          printfn "@[%s: %a@]@," id (pp_exp pp_tannot) e)
                        binds;

                      printfn "len %d" (Bindings.cardinal binds);
                      printfn "@[new body %s: %a@]@," id (pp_exp pp_tannot)
                        new_body)
                    else
                      (* let args, get_typ = extract_args parg in

                         let _ =
                           List.find_opt
                             (fun arg ->
                               match get_typ arg with
                               | Typ_aux (Typ_app (Id_aux (Id typ_id, _), _), _)
                               | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                                   (* if Enums.mem typ_id then printfn "%s %s" id typ_id; *)
                                   Enums.mem typ_id
                               | _ -> assert false)
                             args
                         in

                         Hashtbl.add funcs id (args, body);

                         let args = extract_args_with_typs parg in
                         Hashtbl.add executes id (args, body) *)
                      ()
                | _ -> ())
              funcls
        | _ -> ())
      (Lazy.force sail_json)
  in

  let g = Call_graph.generate funcs in

  (* let g1 = Call_graph.generate1 executes in *)
  (* Call_graph.dump g1 "g_spec.dot"; *)
  let () =
    let cur_aliases = Hashtbl.create 10 in
    Hashtbl.iter
      (fun name (args, body) ->
        Hashtbl.clear cur_aliases;
        collect_aliases body (fun alias arg ->
            if List.mem arg args then Hashtbl.add cur_aliases alias arg
            else if Hashtbl.mem cur_aliases arg then
              Hashtbl.replace cur_aliases alias arg);

        Hashtbl.add aliases name (List.of_seq (Hashtbl.to_seq cur_aliases));

        if debug then (
          printfn "Aliases for %s" name;
          Hashtbl.iter (printfn "%s -> %s") cur_aliases))
      funcs
  in

  Call_graph.dump g config.dot_file;

  let outs =
    Call_graph.propogate_operands ~g ~aliases
      (List.to_seq [ ("wX", [ "r" ]); ("wF", [ "r" ]); ("wV", [ "r" ]) ])
  in
  let ins =
    Call_graph.propogate_operands ~g ~aliases
      (List.to_seq [ ("rX", [ "r" ]); ("rF", [ "r" ]); ("rV", [ "r" ]) ])
  in

  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@[open Core.Instruction@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 1000 in@]@ ";

      let lst_str ppf out =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          (fun ppf -> Format.fprintf ppf "%S")
          ppf out
      in

      let open Assembly_helper in
      Mnemonics.mnemonics
      |> Hashtbl.iter (fun mnemonic (sail_name, opers) ->
             let sail_id =
               match sail_name with
               | IK_straight s -> s
               | IK_singledef (a, b) -> Format.sprintf "%s %s" a b
             in
             let ins =
               let imm_ins =
                 match
                   List.find_all
                     (fun id ->
                       String.ends_with ~suffix:"imm" id
                       || List.mem id
                            [
                              "nzi";
                              "bs";
                              "shamt";
                              "rnum";
                              "rm";
                              "constantidx";
                              "vm";
                              "pred";
                              "succ";
                            ])
                     opers
                 with
                 | xs when not (String.equal "VMVRTYPE" sail_id) -> xs
                 | _ -> []
               in
               let in_regs =
                 match Hashtbl.find_opt ins sail_id with
                 | Some ins -> ins
                 | None -> []
               in
               in_regs @ imm_ins
             in
             let outs =
               match Hashtbl.find_opt outs sail_id with
               | Some outs -> outs
               | None -> []
             in
             printf
               "@[Hashtbl.add ans \"%s\" { mnemonic=\"%s\"; operands=[%a]; \
                ins=[%a]; outs=[%a] };@]@,"
               mnemonic mnemonic lst_str opers lst_str ins lst_str outs);

      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = Hashtbl.find %s @]@," config.ocaml_ident;
      printf "@[let find_opt = Hashtbl.find_opt %s @]@," config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@," config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-execute", Arg.String (fun s -> config.sail_json <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun s -> failwithf "Bad argument: %S" s)
    "";
  dump_execute ()
