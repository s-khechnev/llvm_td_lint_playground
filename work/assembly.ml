type cfg = {
  mutable input : string;
  mutable ocaml_code : string;
  mutable ocaml_ident : string;
}

let config = { input = ""; ocaml_code = ""; ocaml_ident = "" }

let main () =
  let open Myast in
  let asts =
    let json =
      In_channel.with_open_text config.input Yojson.Safe.from_channel
    in
    match json with
    | `List xs ->
        List.filter_map
          (fun j ->
            match Myast.def_of_yojson Myast.tannot_of_yojson j with
            | Result.Error err ->
                Format.eprintf "Error: %s\n%!" err;
                exit 1
            | Ok ast -> Some ast)
          xs
    | _ -> assert false
  in

  let mappings : (string, Libsail.Type_check.tannot mapcl list) Hashtbl.t =
    Hashtbl.create 100
  in
  List.iter
    (function
      | DEF_aux
          ( DEF_mapdef (MD_aux (MD_mapping (Id_aux (Id ident, _), _, maps), _)),
            _ ) ->
          Hashtbl.add mappings ident maps
      | _ -> ())
    asts;

  let assemblies =
    Option.get
    @@ List.find_map
         (function
           | DEF_aux
               ( DEF_mapdef
                   (MD_aux
                     (MD_mapping (Id_aux (Id "assembly", _), _, assemblies), _)),
                 _ ) ->
               Some assemblies
           | _ -> None)
         asts
  in

  let collected = Hashtbl.create 2000 in
  List.iter
    (function
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  (MPat_pat (MP_aux (MP_app (Id_aux (Id ident, _), args), _)), _),
                MPat_aux (MPat_pat (MP_aux (x, _)), _) ),
            _ )
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  ( MPat_when
                      (MP_aux (MP_app (Id_aux (Id ident, _), args), _), _),
                    _ ),
                MPat_aux (MPat_when (MP_aux (x, _), _), _) ),
            _ )
        when not
               (String.equal ident "FENCEI_RESERVED"
               || String.equal ident "FENCE_RESERVED") -> (
          let ident =
            let enum_arg =
              List.find_map
                (function
                  | MP_aux (MP_id (Id_aux (Id id, _)), _) ->
                      if Char.uppercase_ascii id.[0] = id.[0] then Some id
                      else None
                  | _ -> None)
                args
            in
            match enum_arg with
            | Some s -> Format.sprintf "%s____%s" ident s
            | None -> ident
          in
          match x with
          | MP_string_append xs ->
              (* each element from lst2 concatenates with all elements from lst1 *)
              let stupid_concat lst1 lst2 =
                if List.is_empty lst1 then lst2
                else
                  List.concat_map
                    (fun s1 -> List.map (fun s2 -> s1 ^ s2) lst2)
                    lst1
              in
              let mnemonics, opers =
                let exception
                  ReachSpc of
                    (string list * Libsail.Type_check.tannot mpat list)
                in
                (* also return tail after spc *)
                let rec get_mnemonics accu = function
                  | [] -> (accu, [])
                  | a :: l ->
                      get_mnemonics
                        (match a with
                        | MP_aux (MP_lit (L_aux (L_string str, _)), _) ->
                            stupid_concat accu [ str ]
                        | MP_aux
                            ( MP_app
                                ( Id_aux (Id map_id, _),
                                  [ MP_aux (MP_id (Id_aux (Id arg, _)), _) ] ),
                              _ )
                          when Char.uppercase_ascii arg.[0] = arg.[0] ->
                            let map = Hashtbl.find mappings map_id in
                            let str =
                              Option.get
                              @@ List.find_map
                                   (function
                                     | MCL_aux
                                         ( MCL_bidir
                                             ( MPat_aux
                                                 ( MPat_pat
                                                     (MP_aux
                                                       ( MP_id
                                                           (Id_aux
                                                             (Id enum_val_id, _)),
                                                         _ )),
                                                   _ ),
                                               MPat_aux
                                                 ( MPat_pat
                                                     (MP_aux
                                                       ( MP_lit
                                                           (L_aux
                                                             (L_string str, _)),
                                                         _ )),
                                                   _ ) ),
                                           _ )
                                       when String.equal enum_val_id arg ->
                                         Some str
                                     | _ -> None)
                                   map
                            in
                            stupid_concat accu [ str ]
                        | MP_aux (MP_app (Id_aux (Id id, _), _), _) ->
                            if String.equal id "spc" then
                              raise (ReachSpc (accu, l))
                            else
                              let map_strs =
                                let maps = Hashtbl.find mappings id in
                                List.map
                                  (function
                                    | MCL_aux
                                        ( MCL_bidir
                                            ( _,
                                              MPat_aux
                                                ( MPat_pat
                                                    (MP_aux
                                                      ( MP_lit
                                                          (L_aux
                                                            (L_string str, _)),
                                                        _ )),
                                                  _ ) ),
                                          _ ) ->
                                        str
                                    | _ -> "")
                                  maps
                              in
                              stupid_concat accu map_strs
                        | _ -> assert false)
                        l
                in
                try get_mnemonics [] xs with ReachSpc result -> result
              in
              let operands =
                List.fold_right
                  (fun x acc ->
                    match x with
                    | MP_aux
                        ( MP_app
                            ( Id_aux
                                ( Id
                                    ( "reg_name" | "creg_name" | "vreg_name"
                                    | "freg_or_reg_name" | "freg_name"
                                    | "csr_name_map" | "fence_bits" ),
                                  _ ),
                              [ MP_aux (MP_id (Id_aux (Id reg_name, _)), _) ] ),
                          _ ) ->
                        reg_name :: acc
                    | MP_aux
                        ( MP_app
                            ( Id_aux (Id m_id, _),
                              [ MP_aux (MP_id (Id_aux (Id imm, _)), _) ] ),
                          _ )
                      when String.starts_with ~prefix:"hex_bits" m_id
                           || String.equal "frm_mnemonic" m_id
                           || String.equal "maybe_vmask" m_id ->
                        imm :: acc
                    | MP_aux
                        ( MP_app
                            ( Id_aux (Id f_id, _),
                              [ MP_aux (MP_vector_concat xs, _) ] ),
                          _ )
                      when String.starts_with ~prefix:"hex_bits" f_id ->
                        let imm =
                          Option.get
                          @@ List.find_map
                               (function
                                 | MP_aux
                                     ( MP_typ
                                         ( MP_aux (MP_id (Id_aux (Id id, _)), _),
                                           _ ),
                                       _ ) ->
                                     Some id
                                 | _ -> None)
                               xs
                        in
                        imm :: acc
                    | MP_aux (MP_lit (L_aux (L_string "v0", _)), _) ->
                        "v0" :: acc
                    | _ -> acc)
                  opers []
              in
              List.iter
                (fun s -> Hashtbl.add collected s (ident, operands))
                mnemonics
          | MP_lit (L_aux (L_string mnemonic, _)) ->
              Hashtbl.add collected mnemonic (ident, [])
          | _ -> assert false)
      | _ -> ())
    assemblies;

  Out_channel.with_open_text config.ocaml_code (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let %s =@]@," config.ocaml_ident;
      printf "@[let ans = Hashtbl.create 2000 in@]@ ";

      let out_str ppf out =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          (fun ppf -> Format.fprintf ppf "%S")
          ppf out
      in

      collected
      |> Hashtbl.iter (fun key (v, opers) ->
             printf "@[Hashtbl.add ans \"%s\" (\"%s\", [%a]);@]@," key v out_str
               opers);
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find_opt = Hashtbl.find_opt %s@]@\n" config.ocaml_ident;
      printf "@[let find_exn = Hashtbl.find %s@]@\n" config.ocaml_ident;
      printf "@[let mem = Hashtbl.mem %s @]@\n%!" config.ocaml_ident)

let () =
  Arg.parse
    [
      ("-dump-assembly", Arg.String (fun s -> config.input <- s), "");
      ("-ocaml-code", Arg.String (fun s -> config.ocaml_code <- s), "");
      ("-ocaml-ident", Arg.String (fun s -> config.ocaml_ident <- s), "");
    ]
    (fun _ -> failwith "Bad argument: %S")
    "";
  assert (config.input <> "");
  assert (config.ocaml_ident <> "");
  assert (config.ocaml_code <> "");
  main ()
