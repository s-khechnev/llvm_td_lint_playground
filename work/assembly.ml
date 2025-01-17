let input = "./riscv.sail.json"

open Myast

let () =
  let asts =
    let json = In_channel.with_open_text input Yojson.Safe.from_channel in
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

  let hashtbl = Hashtbl.create 3000 in
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
          List.iter
            (function
              | MP_aux (MP_id (Id_aux (Id arg_id, _)), (_, tannot)) -> (
                  match Obj.magic tannot with
                  | Some { env; typ; _ }, _ -> (
                      match typ with
                      | Typ_aux (Typ_id (Id_aux (Id typ_id, _)), _) ->
                          Format.printf "%s %s -> %s\n" ident arg_id typ_id
                      | _ -> ())
                  | _ -> ())
              | _ -> ())
            args;
          match x with
          | MP_string_append xs ->
              let stupid_concat lst1 lst2 =
                if List.is_empty lst1 then lst2
                else
                  List.concat
                    (List.map
                       (fun s2 -> List.map (fun s1 -> s1 ^ s2) lst1)
                       lst2)
              in
              let exception
                ReachSpc of (string list * Libsail.Type_check.tannot mpat list)
              in
              let mnemonics, args =
                (* also returns tail after spc *)
                let rec get_mnemonics accu = function
                  | [] -> (accu, [])
                  | a :: l ->
                      get_mnemonics
                        (match a with
                        | MP_aux (MP_lit (L_aux (L_string str, _)), _) ->
                            stupid_concat accu [ str ]
                        | MP_aux (MP_app (Id_aux (Id map_id, _), _), _) ->
                            if String.equal map_id "spc" then
                              raise (ReachSpc (accu, l))
                            else
                              let maps_strs =
                                let maps = Hashtbl.find mappings map_id in
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
                              stupid_concat accu maps_strs
                        | _ -> assert false)
                        l
                in
                try get_mnemonics [] xs with
                | ReachSpc result -> result
                | _ ->
                    Format.printf "Not reach spc for %s\n" ident;
                    ([], [])
              in
              let regs =
                let regs =
                  List.fold_left
                    (fun acc -> function
                      | MP_aux
                          ( MP_app
                              ( Id_aux
                                  ( Id
                                      ( "reg_name" | "creg_name" | "vreg_name"
                                      | "freg_or_reg_name" | "freg_name" ),
                                    _ ),
                                [ MP_aux (MP_id (Id_aux (Id reg_name, _)), _) ]
                              ),
                            _ ) ->
                          (* Format.printf "%s -> %s\n" ident reg_name; *)
                          reg_name :: acc
                      | MP_aux
                          ( MP_app
                              ( Id_aux (Id f_id, _),
                                [ MP_aux (MP_id (Id_aux (Id reg_name, _)), _) ]
                              ),
                            _ )
                        when String.starts_with ~prefix:"hex_bits" f_id ->
                          (* Format.printf "%s -> %s\n" ident reg_name; *)
                          reg_name :: acc
                      | _ -> acc)
                    [] args
                in
                Array.of_list @@ List.rev regs
              in
              List.iter
                (fun s ->
                  Hashtbl.add hashtbl s (ident, regs)
                  (* Format.printf "%s -> %s\n" ident s *))
                mnemonics
          | MP_lit (L_aux (L_string mnemonic, _)) ->
              Hashtbl.add hashtbl mnemonic (ident, [||])
          (* Format.printf "%s -> %s\n" ident mnemonic *)
          | _ -> assert false)
      | _ -> ())
    assemblies;
  Out_channel.with_open_text "mnemonic_hashtbl.ml" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let ans = \n  let ans = Hashtbl.create 3000 in@]@ ";

      let out_str ppf out =
        Format.pp_print_array
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          (fun ppf -> Format.fprintf ppf "%S")
          ppf out
      in

      hashtbl
      |> Hashtbl.iter (fun key (v, regs) ->
             printf "@[Hashtbl.add ans \"%s\" (\"%s\", [|%a|]);@]@," key v
               out_str regs);
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = Hashtbl.find ans@]@\n%!")
