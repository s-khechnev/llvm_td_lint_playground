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

  (* Format.printf "Count mnemonics: %d\n" (List.length assemblies);
     Format.printf "Count mappings: %d\n" (Hashtbl.length mappings); *)
  (* Hashtbl.iter (fun id _ -> print_endline id) mappings; *)
  let hashtbl = Hashtbl.create 3000 in
  List.iter
    (function
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  (MPat_pat (MP_aux (MP_app (Id_aux (Id ident, _), _), _)), _),
                MPat_aux (MPat_pat (MP_aux (x, _)), _) ),
            _ )
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  ( MPat_when (MP_aux (MP_app (Id_aux (Id ident, _), _), _), _),
                    _ ),
                MPat_aux (MPat_when (MP_aux (x, _), _), _) ),
            _ ) -> (
          match x with
          | MP_string_append xs ->
              let mnemonic =
                let exception ReachSpc of string list in
                try
                  List.fold_left
                    (fun acc ->
                      let trash lst1 lst2 =
                        if List.is_empty lst1 then lst2
                        else
                          List.concat
                            (List.map
                               (fun s2 -> List.map (fun s1 -> s1 ^ s2) lst1)
                               lst2)
                      in
                      function
                      | MP_aux (MP_lit (L_aux (L_string str, _)), _) ->
                          trash acc [ str ]
                      | MP_aux (MP_app (Id_aux (Id map_id, _), _), _) ->
                          if String.equal map_id "spc" then raise (ReachSpc acc)
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
                            if List.is_empty acc then maps_strs
                            else trash acc maps_strs
                      | _ -> assert false)
                    [] xs
                with
                | ReachSpc result -> result
                | _ ->
                    Format.printf "Not reach spc for %s\n" ident;
                    []
              in

              List.iter
                (fun s ->
                  Hashtbl.add hashtbl s ident
                  (* Format.printf "%s -> %s\n" ident s *))
                mnemonic
          | MP_lit (L_aux (L_string mnemonic, _)) ->
              Hashtbl.add hashtbl mnemonic ident
          (* Format.printf "%s -> %s\n" ident mnemonic *)
          | _ -> assert false)
      | _ -> print_endline "not match")
    assemblies;
  Out_channel.with_open_text "mnemonic_hashtbl.ml" (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      let printf fmt = Format.fprintf ppf fmt in

      printf "@[<v>";
      printf "@[(* This file was auto generated *)@]@ ";
      printf "@]@ ";

      printf "@[<v 2>";
      printf "@[let ans = \n        let ans = Hashtbl.create 3000 in@]@ ";

      hashtbl
      |> Hashtbl.iter (fun key v ->
             printf "@[Hashtbl.add ans \"%s\" \"%s\";@]@," key v);
      printf "@[ans@]@ ";
      printf "@]@ ";
      Format.pp_print_cut ppf ();
      printf "@[let find = Hashtbl.find ans@]@\n%!")
