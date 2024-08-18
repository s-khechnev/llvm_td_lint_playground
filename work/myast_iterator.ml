open Myast

type 'a iterator = {
  expr : 'a iterator -> 'a exp -> unit;
  typ : 'a iterator -> typ -> unit;
}

module E = struct
  let iter self (E_aux (e, _)) =
    match e with
    | E_list xs | E_tuple xs | E_app (_, xs) | E_block xs ->
        List.iter (self.expr self) xs
    | E_id _ | E_lit _ -> ()
    | E_typ (t, e) ->
        self.typ self t;
        self.expr self e
    | E_app_infix (l, _, r) ->
        self.expr self l;
        self.expr self r
    | E_if (e1, ethen, eelse) ->
        self.expr self e1;
        self.expr self ethen;
        self.expr self eelse
    | _ -> failwithf "Not implemented %s %d" __FILE__ __LINE__
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

let default_iterator = { expr = E.iter; typ = T.iter }
