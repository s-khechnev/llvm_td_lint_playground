open Libsail
open Type_check
open Ast_util
open Ast

let gensym, reset_anf_counter = Jib_util.symbol_generator "ga"

let rec anf (E_aux (e_aux, exp_annot) as exp : tannot exp) =
  let to_const_e (E_aux (exp_aux, _) as exp) =
    match exp_aux with
    | E_typ (typ, _) ->
        let id = gensym () in
        ( mk_exp (E_id id),
          fun x ->
            mk_exp
              (E_let (mk_letbind (mk_pat (P_typ (typ, mk_pat (P_id id)))) exp, x))
        )
    | E_app _ | E_let _ | E_return _ | E_exit _ | E_throw _ | E_if _ | E_field _
    | E_match _ | E_try _ | E_struct_update _ | E_block _ | E_assign _ | E_for _
    | E_loop _ ->
        let id = gensym () in
        ( mk_exp (E_id id),
          fun x -> mk_exp (E_let (mk_letbind (mk_pat (P_id id)) exp, x)) )
    | _ -> (exp, fun x -> x)
  in

  let strip_measure = function
    | Measure_aux (Measure_none, l) -> Measure_aux (Measure_none, l)
    | Measure_aux (Measure_some e, l) ->
        Measure_aux (Measure_some (strip_exp e), l)
  in

  match e_aux with
  | E_block exps ->
      let exps = List.map anf exps in
      mk_exp (E_block exps)
  | E_assign (lexp, assign_exp) ->
      let aexp = anf assign_exp in
      mk_exp (E_assign (strip_lexp lexp, aexp))
  | E_loop (loop_typ, m, cond, exp) ->
      mk_exp (E_loop (loop_typ, strip_measure m, anf cond, anf exp))
  | E_for (id, exp1, exp2, exp3, order, body) ->
      mk_exp (E_for (id, anf exp1, anf exp2, anf exp3, order, anf body))
  | E_if (cond, then_exp, else_exp) ->
      let cond_val, wrap = to_const_e (anf cond) in
      wrap (mk_exp (E_if (cond_val, anf then_exp, anf else_exp)))
  | E_app_infix (x, Id_aux (Id op, l), y) ->
      anf (E_aux (E_app (Id_aux (Operator op, l), [ x; y ]), exp_annot))
  | E_app_infix (x, Id_aux (Operator op, l), y) ->
      anf (E_aux (E_app (Id_aux (Id op, l), [ x; y ]), exp_annot))
  | E_vector exps ->
      let aexps = List.map anf exps in
      let avals = List.map to_const_e aexps in
      let wrap =
        List.fold_left (fun f g x -> f (g x)) (fun x -> x) (List.map snd avals)
      in
      wrap (mk_exp (E_vector (List.map fst avals)))
  | E_list exps ->
      let aexps = List.map anf exps in
      let avals = List.map to_const_e aexps in
      let wrap =
        List.fold_left (fun f g x -> f (g x)) (fun x -> x) (List.map snd avals)
      in
      wrap (mk_exp (E_list (List.map fst avals)))
  | E_field (field_exp, id) ->
      let aval, wrap = to_const_e (anf field_exp) in
      wrap (mk_exp (E_field (aval, id)))
  | E_var (l, binding, body) -> (
      match strip_lexp l with
      | LE_aux (LE_id id, _) ->
          mk_exp (E_var (mk_lexp (LE_id id), anf binding, anf body))
      | LE_aux (LE_typ (typ, id), _) ->
          mk_exp (E_var (mk_lexp (LE_typ (typ, id)), anf binding, anf body))
      | _ -> strip_exp exp)
  | E_app (id, _)
    when string_of_id id = "and_bool"
         || string_of_id id = "or_bool"
         || string_of_id id = "not_bool" ->
      strip_exp exp
  | E_app (id, exps) ->
      let aexps = List.map anf exps in
      let avals = List.map to_const_e aexps in
      let wrap =
        List.fold_left (fun f g x -> f (g x)) (fun x -> x) (List.map snd avals)
      in
      wrap (mk_exp (E_app (id, List.map fst avals)))
  | E_throw exn_exp ->
      let aexp = anf exn_exp in
      let aval, wrap = to_const_e aexp in
      wrap (mk_exp (E_throw aval))
  | E_let (LB_aux (LB_val (pat, binding), _), body) ->
      mk_exp (E_let (mk_letbind (strip_pat pat) (anf binding), anf body))
  | E_match (match_exp, pexps) ->
      let match_aval, match_wrap = to_const_e (anf match_exp) in
      let anf_pexp (Pat_aux (pat_aux, _)) =
        match pat_aux with
        | Pat_when (pat, guard, body) ->
            mk_pexp (Pat_when (strip_pat pat, anf guard, anf body))
        | Pat_exp (pat, body) -> mk_pexp (Pat_exp (strip_pat pat, anf body))
      in
      match_wrap
        (mk_exp
           (E_typ
              ( typ_of exp,
                mk_exp (E_match (match_aval, List.map anf_pexp pexps)) )))
  | E_typ (typ, exp) -> mk_exp (E_typ (typ, anf exp))
  | _ -> strip_exp exp
