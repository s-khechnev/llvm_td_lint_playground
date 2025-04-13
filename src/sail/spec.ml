open Myast
open Libsail
open Ast_util
open Type_check

module Func = struct
  type t =
    | F_usual of string
    | F_specialized of string * (int * Libsail.Type_check.tannot exp) list

  let get_id = function F_usual id -> id | F_specialized (id, _) -> id

  let equal x y =
    let equal_id id1 id2 =
      match (id1, id2) with
      | Id_aux (Id x, _), Id_aux (Id y, _) -> String.equal x y
      | Id_aux (Operator x, _), Id_aux (Operator y, _) -> String.equal x y
      | _ -> false
    in
    let rec equal_exp e1 e2 =
      match (unaux_exp e1, unaux_exp e2) with
      | E_id id1, E_id id2 -> equal_id id1 id2
      | E_lit (L_aux (L_string s1, _)), E_lit (L_aux (L_string s2, _))
      | E_lit (L_aux (L_hex s1, _)), E_lit (L_aux (L_hex s2, _))
      | E_lit (L_aux (L_bin s1, _)), E_lit (L_aux (L_bin s2, _)) ->
          String.equal s1 s2
      | E_lit (L_aux (L_num n1, _)), E_lit (L_aux (L_num n2, _)) ->
          Nat_big_num.equal n1 n2
      | E_lit (L_aux (L_false, _)), E_lit (L_aux (L_false, _))
      | E_lit (L_aux (L_true, _)), E_lit (L_aux (L_true, _)) ->
          true
      | E_lit (L_aux (L_true, _)), _
      | _, E_lit (L_aux (L_true, _))
      | E_lit (L_aux (L_false, _)), _
      | _, E_lit (L_aux (L_false, _)) ->
          false
      | E_lit (L_aux (L_unit, _)), E_lit (L_aux (L_unit, _))
      | E_lit (L_aux (L_zero, _)), E_lit (L_aux (L_zero, _))
      | E_lit (L_aux (L_one, _)), E_lit (L_aux (L_one, _)) ->
          true
      | E_vector xs1, E_vector xs2 -> List.equal equal_exp xs1 xs2
      | E_struct xs1, E_struct xs2 ->
          List.equal
            (fun (FE_aux (FE_fexp (id1, e1), _)) (FE_aux (FE_fexp (id2, e2), _)) ->
              equal_id id1 id2 && equal_exp e1 e2)
            xs1 xs2
      | _ -> String.equal (string_of_exp e1) (string_of_exp e2)
    in
    match (x, y) with
    | F_usual s1, F_usual s2 -> String.equal s1 s2
    | F_specialized (s1, l1), F_specialized (s2, l2) ->
        String.equal s1 s2
        && List.equal
             (fun (i1, e1) (i2, e2) -> i1 = i2 && equal_exp e1 e2)
             l1 l2
    | _, _ -> false

  let hash e =
    let hash_speced lst =
      let hash_exp e = Hashtbl.hash (string_of_exp e) in
      List.fold_left (fun acc (i, x) -> acc + Hashtbl.hash i + hash_exp x) 0 lst
    in
    match e with
    | F_usual s -> Hashtbl.hash s
    | F_specialized (s, speced) -> Hashtbl.hash (s, hash_speced speced)

  let compare x y = String.compare (get_id x) (get_id y)
end

module FuncTable = Hashtbl.Make (Func)

let get_speced_args pargsi =
  List.filter_map
    (fun (i, p) ->
      let rec to_const_exp (P_aux (p, (_, tannot)) as pat) =
        match p with
        | P_var (p, _) -> to_const_exp p
        | P_typ (_, p) -> to_const_exp p
        | P_id id -> (
            match Env.lookup_id id (env_of_pat pat) with
            | Enum typ ->
                Some
                  (E_aux
                     ( E_id id,
                       (Unknown, Type_check.mk_tannot (env_of_pat pat) typ) ))
            | _ -> None)
        | P_lit l -> Some (E_aux (E_lit l, (Unknown, tannot)))
        | _ -> None
      in
      to_const_exp p |> Option.map (fun e -> (i, e)))
    pargsi

let get_args_to_spec pargsi =
  List.filter_map
    (fun (i, p) ->
      let rec need_spec (P_aux (p, _) as pat) =
        match p with
        | P_var (p, _) -> need_spec p
        | P_typ (_, p) -> need_spec p
        | P_id id
          when match Env.lookup_id id (env_of_pat pat) with
               | Enum _ -> false
               | _ -> true -> (
            match typ_of_pat pat with
            | Typ_aux (Typ_id t, _) | Typ_aux (Typ_app (t, _), _) -> (
                if string_of_id t = "bool" || string_of_id t = "atom_bool" then
                  let f l =
                    E_aux
                      ( E_lit (mk_lit l),
                        (Unknown, Type_check.mk_tannot (env_of_pat pat) bool_typ)
                      )
                  in
                  Some (i, id, [ f L_false; f L_true ])
                else
                  try
                    let ids = Env.get_enum t (env_of_pat pat) in
                    let values =
                      List.map
                        (fun enum_id ->
                          E_aux
                            ( E_id enum_id,
                              ( Unknown,
                                Type_check.mk_tannot (env_of_pat pat)
                                  (mk_typ (Typ_id t)) ) ))
                        ids
                    in
                    Some (i, id, values)
                  with _ -> None)
            | _ -> None)
        | _ -> None
      in
      need_spec p)
    pargsi
