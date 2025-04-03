open Myast

type func = F_usual of string | F_specialized of string * (int * string) list

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
