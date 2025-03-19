open Myast

type 'a iterator = {
  pexp : 'a iterator -> 'a pexp -> unit;
  exp : 'a iterator -> 'a exp -> unit;
  fexp : 'a iterator -> 'a fexp -> unit;
  lexp : 'a iterator -> 'a lexp -> unit;
  exp_aux : 'a iterator -> 'a exp_aux -> unit;
  lexp_aux : 'a iterator -> 'a lexp_aux -> unit;
  typ : 'a iterator -> typ -> unit;
  let_bind : 'a iterator -> 'a letbind -> unit;
}

(* module PPSail = struct
     include Libsail.Format_sail.Make (struct
       let config = Libsail.Format_sail.default_config
     end)

     let no_def_annot = { doc_comment = None; attrs = []; loc = Unknown }
     let noannot : _ annot = (Unknown, 1)
     let eletbind p exp = LB_aux (LB_val (p, exp), noannot)
     let elet lb e = E_aux (E_let (lb, e), noannot)
     let ereturn e = E_aux (E_return e, noannot)
     let def_let x = DEF_aux (DEF_let x, no_def_annot)
     let pwild = P_aux (P_wild, noannot)

     type _1 = Libsail.Parse_ast.def

     let show_expr e : string = format_defs "" "" [] [ def_let (eletbind pwild e) ]
   end *)

module E = struct
  let iter self e =
    match e with
    | E_vector xs | E_list xs | E_tuple xs | E_app (_, xs) | E_block xs ->
        List.iter (self.exp self) xs
    | E_id _ | E_lit _ -> ()
    | E_return e | E_exit e | E_throw e | E_field (e, _) -> self.exp self e
    | E_typ (t, e) ->
        self.typ self t;
        self.exp self e
    | E_assert (l, r) | E_app_infix (l, _, r) ->
        self.exp self l;
        self.exp self r
    | E_if (e1, ethen, eelse) ->
        self.exp self e1;
        self.exp self ethen;
        self.exp self eelse
    | E_let (lb, e1) ->
        self.let_bind self lb;
        self.exp self e1
    | E_assign (l, r) ->
        self.lexp self l;
        self.exp self r
    | E_match (e, xs) ->
        self.exp self e;
        List.iter (self.pexp self) xs
    | E_struct fs -> List.iter (self.fexp self) fs
    | E_var (a, b, c) ->
        self.lexp self a;
        self.exp self b;
        self.exp self c
    | E_for (_, a, b, c, _, d) ->
        self.exp self a;
        self.exp self b;
        self.exp self c;
        self.exp self d
    | E_struct_update (e, fs) ->
        self.exp self e;
        List.iter (self.fexp self) fs
    | E_cons (a, b) ->
        self.exp self a;
        self.exp self b
    | E_loop (_, m, a, b) ->
        (match m with
        | Measure_aux (Measure_some s, _) -> self.exp self s
        | _ -> ());
        self.exp self a;
        self.exp self b
    | e ->
        let repr = Format.asprintf "%a" (pp_exp_aux pp_tannot) e in
        let repr =
          if String.length repr > 20 then String.sub repr 0 20 else repr
        in
        Format.eprintf "@[%s@]@ " repr;
        failwithf "Not implemented %s %d" __FILE__ __LINE__

  let iter_lexp self = function
    | LE_id _ -> ()
    | LE_deref e -> self.exp self e
    | LE_app (_, xs) -> List.iter (self.exp self) xs
    | LE_typ (t, _) -> self.typ self t
    | LE_tuple xs | LE_vector_concat xs -> List.iter (self.lexp self) xs
    | LE_vector (l, r) ->
        self.lexp self l;
        self.exp self r
    | LE_vector_range (l, r, r2) ->
        self.lexp self l;
        self.exp self r;
        self.exp self r2
    | LE_field (l, _) -> self.lexp self l
end

module T = struct
  let iter self (Typ_aux (t, _)) =
    match t with
    | Typ_app _ | Typ_internal_unknown | Typ_id _ | Typ_var _ -> ()
    | Typ_fn (xs, res) ->
        List.iter (self.typ self) xs;
        self.typ self res
    | Typ_bidir (l, r) ->
        self.typ self l;
        self.typ self r
    | Typ_tuple xs -> List.iter (self.typ self) xs
    | Typ_exist (_, _, r) -> self.typ self r
end

let default_iterator =
  {
    typ = T.iter;
    exp_aux = E.iter;
    exp = (fun self (E_aux (e, _)) -> self.exp_aux self e);
    lexp_aux = E.iter_lexp;
    (* lexp = (fun self (LE_aux (e, _)) -> self.lexp self e); *)
    lexp = (fun self (LE_aux (e, _)) -> self.lexp_aux self e);
    let_bind = (fun self (LB_aux (LB_val (_pat, e), _)) -> self.exp self e);
    pexp =
      (fun self (Pat_aux (p, _)) ->
        match p with
        | Pat_exp (_, e) -> self.exp self e
        | Pat_when (_, e1, e2) ->
            self.exp self e1;
            self.exp self e2);
    fexp =
      (fun self (FE_aux (p, _)) ->
        match p with FE_fexp (_, e) -> self.exp self e);
  }
