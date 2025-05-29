open Libsail
open Myast
open Spec
open Type_check
open Ast_util

(* analyze assembly clauses and returns mnemonics, operands and imms of instructions *)
let get_info (ast : 'a Ast_defs.ast) =
  let defs = ast.defs in
  let mappings : (string, typ * Type_check.tannot mapcl list) Hashtbl.t =
    Hashtbl.create 100
  in
  let () =
    List.iter
      (function
        | DEF_aux
            ( DEF_mapdef (MD_aux (MD_mapping (Id_aux (Id ident, _), t, maps), _)),
              _ ) ->
            let typ =
              match t with
              | Typ_annot_opt_aux (Typ_annot_opt_some (_, typ), _) -> typ
              | Typ_annot_opt_aux (Typ_annot_opt_none, _) ->
                  defs
                  |> List.find_map (function
                       | DEF_aux
                           ( DEF_val
                               (VS_aux
                                 ( VS_val_spec
                                     ( TypSchm_aux (TypSchm_ts (_, typ), _),
                                       Id_aux (Id val_spec_id, _),
                                       _ ),
                                   _ )),
                             _ )
                         when val_spec_id = ident ->
                           Some typ
                       | _ -> None)
                  |> Option.get
            in
            Hashtbl.add mappings ident (typ, maps)
        | _ -> ())
      defs
  in
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
         defs
  in
  (* analyze the instructions only from p ext *)
  let assemblies =
    List.filter
      (function
        | MCL_aux (_, ({ doc_comment = _; attrs = _; loc }, _)) ->
            let is_from_pext ({ pos_fname; _ } : Lexing.position) =
              let str_match rgx_str =
                Str.string_match (Str.regexp rgx_str) pos_fname 0
              in
              (* for tests *)
              str_match ".*sail.sail.*" || str_match ".*pext.*"
            in
            let rec f loc =
              match loc with
              | Range (pos1, pos2) -> is_from_pext pos1 || is_from_pext pos2
              | Unique (_, l) | Generated l -> f l
              | Hint (_, l1, l2) -> f l1 || f l2
              | _ -> false
            in
            f loc)
      assemblies
  in

  let collect_mnemonics substs =
    let handle_str str =
      (* typo *)
      if str = "KMABBT32" then "kmabt32"
      else
        String.map
          (fun c -> if c = '_' then '.' else Char.lowercase_ascii c)
          str
        |> String.trim
    in
    let eval_mapping map_id arg =
      match Hashtbl.find mappings map_id with
      | Typ_aux (Typ_bidir (ltyp, _), _), maps ->
          let arg_typ = typ_of arg in
          maps
          |> List.find_map (function
               | MCL_aux (MCL_bidir (lhs, rhs), _) ->
                   let mpat_eq_exp (MP_aux (p, _)) (E_aux (e, _)) =
                     match (p, e) with
                     | MP_id id1, E_id id2 -> Id.compare id1 id2 = 0
                     | MP_lit (L_aux (L_false, _)), E_lit (L_aux (L_false, _))
                     | MP_lit (L_aux (L_true, _)), E_lit (L_aux (L_true, _)) ->
                         true
                     | _ -> false
                   in
                   let eval_map_to_str (MPat_aux (pe1, _)) (MPat_aux (pe2, _)) =
                     match (pe1, pe2) with
                     | ( MPat_pat lhs,
                         MPat_pat (MP_aux (MP_lit (L_aux (L_string str, _)), _))
                       ) ->
                         if mpat_eq_exp lhs arg then Some (handle_str str)
                         else None
                     | _ -> None
                   in
                   if Typ.compare ltyp arg_typ = 0 then eval_map_to_str lhs rhs
                   else eval_map_to_str rhs lhs
               | _ -> None)
          |> Option.get
      | _ -> assert false
    in
    function
    | MP_string_append xs ->
        (* each element from lst2 concatenates with all elements from lst1 *)
        let my_concat lst1 lst2 =
          if List.is_empty lst1 then lst2
          else
            List.concat_map (fun s1 -> List.map (fun s2 -> s1 ^ s2) lst2) lst1
        in
        let mnemonics, tail =
          let exception ReachSpc of (string list * Type_check.tannot mpat list)
          in
          (* also returns tail after spc *)
          let rec get_mnemonics accu = function
            | [] -> (accu, [])
            | a :: tl ->
                let accu =
                  match a with
                  | MP_aux (MP_lit (L_aux (L_string str, _)), _) ->
                      my_concat accu [ handle_str str ]
                  | MP_aux
                      ( MP_app
                          ( Id_aux (Id map_id, _),
                            [ MP_aux (MP_id arg_id, annot) ] ),
                        _ ) -> (
                      match Env.lookup_id arg_id (env_of_annot annot) with
                      | Enum typ ->
                          my_concat accu
                            [
                              eval_mapping map_id
                                (E_aux
                                   ( E_id arg_id,
                                     ( Unknown,
                                       Type_check.mk_tannot (env_of_annot annot)
                                         typ ) ));
                            ]
                      | _ -> (
                          match
                            List.find_opt
                              (fun (id, _) -> Id.compare arg_id id = 0)
                              substs
                          with
                          | Some (_, arg_value) ->
                              my_concat accu [ eval_mapping map_id arg_value ]
                          | None -> (
                              try
                                let strs =
                                  let _, maps = Hashtbl.find mappings map_id in
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
                                          handle_str str
                                      | _ -> "")
                                    maps
                                in
                                my_concat accu strs
                              with Not_found -> accu)))
                  | MP_aux (MP_app (Id_aux (Id "spc", _), _), _) ->
                      raise (ReachSpc (accu, tl))
                  | _ -> accu
                in
                get_mnemonics accu tl
          in
          try get_mnemonics [] xs with ReachSpc result -> result
        in
        let imms = ref [] in
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
                              | "csr_name_map" ),
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
                     || String.equal "maybe_vmask" m_id
                     || String.equal "fence_bits" m_id ->
                  imms := imm :: !imms;
                  imm :: acc
              | MP_aux
                  ( MP_app
                      (Id_aux (Id f_id, _), [ MP_aux (MP_vector_concat xs, _) ]),
                    _ )
                when String.starts_with ~prefix:"hex_bits" f_id ->
                  let imm =
                    Option.get
                    @@ List.find_map
                         (function
                           | MP_aux
                               ( MP_typ
                                   (MP_aux (MP_id (Id_aux (Id id, _)), _), _),
                                 _ ) ->
                               Some id
                           | _ -> None)
                         xs
                  in
                  imms := imm :: !imms;
                  imm :: acc
              | MP_aux (MP_lit (L_aux (L_string "v0", _)), _) -> "v0" :: acc
              | _ -> acc)
            tail []
        in
        (mnemonics, operands, !imms)
    | MP_lit (L_aux (L_string s, _)) -> ([ handle_str s ], [], [])
    | _ -> assert false
  in
  let collected = FuncTable.create 1000 in
  List.iter
    (function
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  (MPat_pat (MP_aux (MP_app (Id_aux (Id ident, _), args), _)), _),
                MPat_aux (MPat_pat (MP_aux (body, _)), _) ),
            _ )
      | MCL_aux
          ( MCL_bidir
              ( MPat_aux
                  ( MPat_when
                      (MP_aux (MP_app (Id_aux (Id ident, _), args), _), _),
                    _ ),
                MPat_aux (MPat_when (MP_aux (body, _), _), _) ),
            _ )
        when ident <> "FENCEI_RESERVED" && ident <> "FENCE_RESERVED" -> (
          let args = List.mapi (fun i a -> (i, Ast_util.pat_of_mpat a)) args in
          match (get_speced_args args, get_args_to_spec args) with
          | [], [] ->
              let mnemonics = collect_mnemonics [] body in
              FuncTable.add collected (F_usual ident) mnemonics
          | speced, [] ->
              let mnemonics = collect_mnemonics [] body in
              FuncTable.add collected (F_specialized (ident, speced)) mnemonics
          | speced, to_spec ->
              product to_spec
              |> List.iter (fun xs ->
                     let substs = List.map (fun (_, id, e) -> (id, e)) xs in
                     let mnemonics = collect_mnemonics substs body in
                     FuncTable.add collected
                       (F_specialized
                          ( ident,
                            List.merge
                              (fun (i1, _) (i2, _) -> compare i1 i2)
                              speced
                              (List.map (fun (i, _, e) -> (i, e)) xs) ))
                       mnemonics))
      | _ -> ())
    assemblies;
  collected
