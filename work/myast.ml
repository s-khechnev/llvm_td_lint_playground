let failwithf fmt = Format.kasprintf failwith fmt

open Libsail

type position = Lexing.position

let position_to_yojson : position -> Yojson.Safe.t =
 fun pos -> `List [ `String pos.Lexing.pos_fname; `Int pos.Lexing.pos_lnum ]

let position_of_yojson : Yojson.Safe.t -> (position, _) Result.t = function
  | `List [ `String pos_fname; `Int pos_lnum ] ->
      Result.ok @@ { Lexing.pos_fname; pos_lnum; pos_bol = 0; pos_cnum = 0 }
  | _ -> Result.Ok Lexing.dummy_pos

let pp_position ppf { Lexing.pos_fname; pos_lnum; _ } =
  Format.fprintf ppf "%s/%d" pos_fname pos_lnum

type l = Libsail.Parse_ast.l =
  | Unknown
  | Unique of int * l
  | Generated of l
  | Hint of string * l * l
  | Range of position * position
[@@deriving yojson, show { with_path = false }]

let pp_l ppf = function
  | Range (a, b) when a.pos_cnum = b.pos_cnum && a.pos_fname = b.pos_fname ->
      pp_position ppf a
  | x -> pp_l ppf x

type 'a string_map = 'a Value.StringMap.t

let pp_string_map _fa ppf _ = Format.fprintf ppf "???"
let string_map_to_yojson _ : _ -> Yojson.Safe.t = fun _ -> `List []

let string_map_of_yojson fv : Yojson.Safe.t -> ('v string_map, _) Result.t =
  let rec helper acc = function
    | `String name :: j :: tl -> (
        match fv j with
        | Result.Ok v -> helper (Value.StringMap.add name v acc) tl
        | Error s -> Error s)
    | [] -> Result.Ok acc
    | _ -> assert false
  in
  function `List xs -> helper Value.StringMap.empty xs | _ -> assert false

type bit = Sail_lib.bit = B0 | B1
[@@deriving yojson, show { with_path = false }]

type rational = Rational.t

let pp_rational ppf _ = Format.fprintf ppf "???"
let rational_to_yojson _r = `Int 42
let rational_of_yojson _ = Result.Ok (Rational.of_int 42)

type z = Z.t

let pp_z ppf _ = Format.fprintf ppf "???"
let z_to_yojson _ = `Int 42
let z_of_yojson _ = Result.Ok (Z.of_int 42)

type value = Libsail.Value.value =
  | V_vector of value list
  | V_list of value list
  | V_int of z
  | V_real of rational
  | V_bool of bool
  | V_bit of bit
  | V_tuple of value list
  | V_unit
  | V_string of string
  | V_ref of string
  | V_ctor of string * value list
  | V_record of value string_map
  | V_attempted_read of string
[@@deriving yojson, show { with_path = false }]

type loop = Ast.loop = While | Until
[@@deriving yojson, show { with_path = false }]

type 'a annot = l * 'a [@@deriving yojson, show { with_path = false }]

type extern = Ast.extern = { pure : bool; bindings : (string * string) list }
[@@deriving yojson, show { with_path = false }]

type def_annot = Ast.def_annot = {
  doc_comment : string option;
  attrs : (l * string * string) list;
  loc : l;
}
[@@deriving yojson, show { with_path = false }]

let pp_def_annot ppf = function
  | { doc_comment = None; attrs = []; loc } -> pp_l ppf loc
  | x -> pp_def_annot ppf x

type 'a clause_annot = def_annot * 'a
[@@deriving yojson, show { with_path = false }]

type x = string (* identifier *) [@@deriving yojson, show { with_path = false }]

type ix = string
(* infix identifier *) [@@deriving yojson, show { with_path = false }]

type kid_aux = Ast.kid_aux =
  (* kinded IDs: Type, Int, and Bool variables *)
  | Var of x
[@@deriving yojson, show { with_path = false }]

type kind_aux = Ast.kind_aux =
  (* base kind *)
  | K_type (* kind of types *)
  | K_int (* kind of natural number size expressions *)
  | K_bool (* kind of constraints *)
[@@deriving yojson, show { with_path = false }]

type id_aux = Ast.id_aux =
  (* Identifier *)
  | Id of x
  | Operator of x (* remove infix status *)
[@@deriving yojson, show { with_path = false }]

type kid = Ast.kid = Kid_aux of kid_aux * l
[@@deriving yojson, show { with_path = false }]

type kind = Ast.kind = K_aux of kind_aux * l
[@@deriving yojson, show { with_path = false }]

type id = Ast.id = Id_aux of id_aux * l
[@@deriving yojson, show { with_path = false }]

type kinded_id_aux = Ast.kinded_id_aux =
  (* optionally kind-annotated identifier *)
  | KOpt_kind of kind * kid (* kind-annotated variable *)
[@@deriving yojson, show { with_path = false }]

type num = Nat_big_num.num

let pp_num = Nat_big_num.pp_num

let num_to_yojson : num -> Yojson.Safe.t =
 fun n -> `String (Format.asprintf "%a" Nat_big_num.pp_num n)

let num_of_yojson : Yojson.Safe.t -> (num, _) Result.t =
 fun _ -> Result.Ok (Nat_big_num.of_int 42)

type nexp_aux = Ast.nexp_aux =
  (* numeric expression, of kind Int *)
  | Nexp_id of id (* abbreviation identifier *)
  | Nexp_var of kid (* variable *)
  | Nexp_constant of num (* constant *)
  | Nexp_app of id * nexp list (* app *)
  | Nexp_times of nexp * nexp (* product *)
  | Nexp_sum of nexp * nexp (* sum *)
  | Nexp_minus of nexp * nexp (* subtraction *)
  | Nexp_exp of nexp (* exponential *)
  | Nexp_neg of nexp (* unary negation *)

and nexp = Ast.nexp = Nexp_aux of nexp_aux * l
[@@deriving yojson, show { with_path = false }]

type kinded_id = Ast.kinded_id = KOpt_aux of kinded_id_aux * l
[@@deriving yojson, show { with_path = false }]

type lit_aux = Ast.lit_aux =
  (* literal constant *)
  | L_unit
  | L_zero
  | L_one
  | L_true
  | L_false
  | L_num of num (* natural number constant *)
  | L_hex of string (* bit vector constant, C-style *)
  | L_bin of string (* bit vector constant, C-style *)
  | L_string of string (* string constant *)
  | L_undef (* undefined-value constant *)
  | L_real of string
[@@deriving yojson, show { with_path = false }]

type typ_aux = Ast.typ_aux =
  (* type expressions, of kind Type *)
  | Typ_internal_unknown
  | Typ_id of id (* defined type *)
  | Typ_var of kid (* type variable *)
  | Typ_fn of typ list * typ (* Function (first-order only) *)
  | Typ_bidir of typ * typ (* Mapping *)
  | Typ_tuple of typ list (* Tuple *)
  | Typ_app of id * typ_arg list (* type constructor application *)
  | Typ_exist of kinded_id list * n_constraint * typ

and typ = Ast.typ = Typ_aux of typ_aux * l

and typ_arg_aux = Ast.typ_arg_aux =
  (* type constructor arguments of all kinds *)
  | A_nexp of nexp
  | A_typ of typ
  | A_bool of n_constraint

and typ_arg = Ast.typ_arg = A_aux of typ_arg_aux * l

and n_constraint_aux = Ast.n_constraint_aux =
  (* constraint over kind Int *)
  | NC_equal of nexp * nexp
  | NC_bounded_ge of nexp * nexp
  | NC_bounded_gt of nexp * nexp
  | NC_bounded_le of nexp * nexp
  | NC_bounded_lt of nexp * nexp
  | NC_not_equal of nexp * nexp
  | NC_set of kid * num list
  | NC_or of n_constraint * n_constraint
  | NC_and of n_constraint * n_constraint
  | NC_app of id * typ_arg list
  | NC_var of kid
  | NC_true
  | NC_false

and n_constraint = Ast.n_constraint = NC_aux of n_constraint_aux * l
[@@deriving yojson, show { with_path = false }]

type order_aux = Ast.order_aux =
  (* vector order specifications, of kind Order *)
  | Ord_inc (* increasing *)
  | Ord_dec
(* decreasing *) [@@deriving yojson, show { with_path = false }]

type lit = Ast.lit = L_aux of lit_aux * l
[@@deriving yojson, show { with_path = false }]

type typ_pat_aux = Ast.typ_pat_aux =
  (* type pattern *)
  | TP_wild
  | TP_var of kid
  | TP_app of id * typ_pat list

and typ_pat = Ast.typ_pat = TP_aux of typ_pat_aux * l
[@@deriving yojson, show { with_path = false }]

type field_pat_wildcard = Ast.field_pat_wildcard = FP_wild of l | FP_no_wild
[@@deriving yojson, show { with_path = false }]

type quant_item_aux = Ast.quant_item_aux =
  (* kinded identifier or Int constraint *)
  | QI_id of kinded_id (* optionally kinded identifier *)
  | QI_constraint of n_constraint
(* constraint *) [@@deriving yojson, show { with_path = false }]

type order = Ast.order = Ord_aux of order_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a pat_aux = 'a Ast.pat_aux =
  (* pattern *)
  | P_lit of lit (* literal constant pattern *)
  | P_wild (* wildcard *)
  | P_or of 'a pat * 'a pat (* pattern disjunction *)
  | P_not of 'a pat (* pattern negation *)
  | P_as of 'a pat * id (* named pattern *)
  | P_typ of typ * 'a pat (* typed pattern *)
  | P_id of id (* identifier *)
  | P_var of 'a pat * typ_pat (* bind pattern to type variable *)
  | P_app of id * 'a pat list (* union constructor pattern *)
  | P_vector of 'a pat list (* vector pattern *)
  | P_vector_concat of 'a pat list (* concatenated vector pattern *)
  | P_vector_subrange of id * num * num (* vector subrange pattern *)
  | P_tuple of 'a pat list (* tuple pattern *)
  | P_list of 'a pat list (* list pattern *)
  | P_cons of 'a pat * 'a pat (* Cons patterns *)
  | P_string_append of 'a pat list (* string append pattern, x ^^ y *)
  | P_struct of (id * 'a pat) list * field_pat_wildcard (* struct pattern *)

and 'a pat = 'a Ast.pat = P_aux of 'a pat_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type quant_item = Ast.quant_item = QI_aux of quant_item_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a internal_loop_measure_aux = 'a Ast.internal_loop_measure_aux =
  (* internal syntax for an optional termination measure for a loop *)
  | Measure_none
  | Measure_some of 'a exp

and 'a internal_loop_measure = 'a Ast.internal_loop_measure =
  | Measure_aux of 'a internal_loop_measure_aux * l

and 'a exp_aux = 'a Ast.exp_aux =
  (* expression *)
  | E_block of 'a exp list (* sequential block *)
  | E_id of id (* identifier *)
  | E_lit of lit (* literal constant *)
  | E_typ of typ * 'a exp (* cast *)
  | E_app of id * 'a exp list (* function application *)
  | E_app_infix of 'a exp * id * 'a exp (* infix function application *)
  | E_tuple of 'a exp list (* tuple *)
  | E_if of 'a exp * 'a exp * 'a exp (* conditional *)
  | E_loop of loop * 'a internal_loop_measure * 'a exp * 'a exp
  | E_for of id * 'a exp * 'a exp * 'a exp * order * 'a exp (* for loop *)
  | E_vector of 'a exp list (* vector (indexed from 0) *)
  | E_vector_access of 'a exp * 'a exp (* vector access *)
  | E_vector_subrange of 'a exp * 'a exp * 'a exp (* subvector extraction *)
  | E_vector_update of 'a exp * 'a exp * 'a exp (* vector functional update *)
  | E_vector_update_subrange of 'a exp * 'a exp * 'a exp * 'a exp
    (* vector subrange update, with vector *)
  | E_vector_append of 'a exp * 'a exp (* vector concatenation *)
  | E_list of 'a exp list (* list *)
  | E_cons of 'a exp * 'a exp (* cons *)
  | E_struct of 'a fexp list (* struct *)
  | E_struct_update of 'a exp * 'a fexp list (* functional update of struct *)
  | E_field of 'a exp * id (* field projection from struct *)
  | E_match of 'a exp * 'a pexp list (* pattern matching *)
  | E_let of 'a letbind * 'a exp (* let expression *)
  | E_assign of 'a lexp * 'a exp (* imperative assignment *)
  | E_sizeof of nexp (* the value of $nexp$ at run time *)
  | E_return of 'a exp (* return $(exp 'a)$ from current function *)
  | E_exit of 'a exp (* halt all current execution *)
  | E_ref of id
  | E_throw of 'a exp
  | E_try of 'a exp * 'a pexp list
  | E_assert of 'a exp * 'a exp
    (* halt with error message $(exp 'a)$ when not $(exp 'a)$. exp' is optional. *)
  | E_var of 'a lexp * 'a exp * 'a exp
    (* This is an internal node for compilation that demonstrates the scope of a local mutable variable *)
  | E_internal_plet of 'a pat * 'a exp * 'a exp
    (* This is an internal node, used to distinguish some introduced lets during processing from original ones *)
  | E_internal_return of 'a exp
    (* For internal use to embed into monad definition *)
  | E_internal_value of value
    (* For internal use in interpreter to wrap pre-evaluated values when returning an action *)
  | E_internal_assume of n_constraint * 'a exp
    (* Internal node for additional type checker assumptions *)
  | E_constraint of n_constraint

and 'a exp = 'a Ast.exp = E_aux of 'a exp_aux * 'a annot

and 'a lexp_aux = 'a Ast.lexp_aux =
  (* lvalue expression *)
  | LE_id of id (* identifier *)
  | LE_deref of 'a exp
  | LE_app of id * 'a exp list (* memory or register write via function call *)
  | LE_typ of typ * id
  | LE_tuple of 'a lexp list (* multiple (non-memory) assignment *)
  | LE_vector_concat of 'a lexp list (* vector concatenation L-exp *)
  | LE_vector of 'a lexp * 'a exp (* vector element *)
  | LE_vector_range of 'a lexp * 'a exp * 'a exp (* subvector *)
  | LE_field of 'a lexp * id (* struct field *)

and 'a lexp = 'a Ast.lexp = LE_aux of 'a lexp_aux * 'a annot

and 'a fexp_aux = 'a Ast.fexp_aux =
  (* field expression *)
  | FE_fexp of id * 'a exp

and 'a fexp = 'a Ast.fexp = FE_aux of 'a fexp_aux * 'a annot

and 'a pexp_aux = 'a Ast.pexp_aux =
  (* pattern match *)
  | Pat_exp of 'a pat * 'a exp
  | Pat_when of 'a pat * 'a exp * 'a exp

and 'a pexp = 'a Ast.pexp = Pat_aux of 'a pexp_aux * 'a annot

and 'a letbind_aux = 'a Ast.letbind_aux =
  (* let binding *)
  | LB_val of 'a pat * 'a exp
(* let, implicit type ($(pat 'a)$ must be total) *)

and 'a letbind = 'a Ast.letbind = LB_aux of 'a letbind_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a mpat_aux = 'a Ast.mpat_aux =
  (* Mapping pattern. Mostly the same as normal patterns but only constructible parts *)
  | MP_lit of lit
  | MP_id of id
  | MP_app of id * 'a mpat list
  | MP_vector of 'a mpat list
  | MP_vector_concat of 'a mpat list
  | MP_vector_subrange of id * num * num
  | MP_tuple of 'a mpat list
  | MP_list of 'a mpat list
  | MP_cons of 'a mpat * 'a mpat
  | MP_string_append of 'a mpat list
  | MP_typ of 'a mpat * typ
  | MP_as of 'a mpat * id
  | MP_struct of (id * 'a mpat) list

and 'a mpat = 'a Ast.mpat = MP_aux of 'a mpat_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type typquant_aux = Ast.typquant_aux =
  (* type quantifiers and constraints *)
  | TypQ_tq of quant_item list
  | TypQ_no_forall
(* empty *) [@@deriving yojson, show { with_path = false }]

type 'a mpexp_aux = 'a Ast.mpexp_aux =
  | MPat_pat of 'a mpat
  | MPat_when of 'a mpat * 'a exp
[@@deriving yojson, show { with_path = false }]

type typquant = Ast.typquant = TypQ_aux of typquant_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a pexp_funcl = 'a pexp [@@deriving yojson, show { with_path = false }]

type 'a mpexp = 'a Ast.mpexp = MPat_aux of 'a mpexp_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type type_union_aux = Ast.type_union_aux =
  (* type union constructors *)
  | Tu_ty_id of typ * id
[@@deriving yojson, show { with_path = false }]

type tannot_opt_aux = Ast.tannot_opt_aux =
  (* optional type annotation for functions *)
  | Typ_annot_opt_none
  | Typ_annot_opt_some of typquant * typ
[@@deriving yojson, show { with_path = false }]

type 'a rec_opt_aux = 'a Ast.rec_opt_aux =
  (* optional recursive annotation for functions *)
  | Rec_nonrec (* non-recursive *)
  | Rec_rec (* recursive without termination measure *)
  | Rec_measure of 'a pat * 'a exp
(* recursive with termination measure *)
[@@deriving yojson, show { with_path = false }]

type 'a funcl_aux = 'a Ast.funcl_aux =
  (* function clause *)
  | FCL_funcl of id * 'a pexp_funcl
[@@deriving yojson, show { with_path = false }]

type 'a mapcl_aux = 'a Ast.mapcl_aux =
  (* mapping clause (bidirectional pattern-match) *)
  | MCL_bidir of 'a mpexp * 'a mpexp
  | MCL_forwards of 'a mpexp * 'a exp
  | MCL_backwards of 'a mpexp * 'a exp
[@@deriving yojson, show { with_path = false }]

type typschm_aux = Ast.typschm_aux =
  (* type scheme *)
  | TypSchm_ts of typquant * typ
[@@deriving yojson, show { with_path = false }]

type index_range_aux = Ast.index_range_aux =
  (* index specification, for bitfields in register types *)
  | BF_single of nexp (* single index *)
  | BF_range of nexp * nexp (* index range *)
  | BF_concat of index_range * index_range (* concatenation of index ranges *)

and index_range = Ast.index_range = BF_aux of index_range_aux * l
[@@deriving yojson, show { with_path = false }]

type type_union = Ast.type_union = Tu_aux of type_union_aux * def_annot
[@@deriving yojson, show { with_path = false }]

type tannot_opt = Ast.tannot_opt = Typ_annot_opt_aux of tannot_opt_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a rec_opt = 'a Ast.rec_opt = Rec_aux of 'a rec_opt_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a funcl = 'a Ast.funcl = FCL_aux of 'a funcl_aux * 'a clause_annot
[@@deriving yojson, show { with_path = false }]

type 'a mapcl = 'a Ast.mapcl = MCL_aux of 'a mapcl_aux * 'a clause_annot
[@@deriving yojson, show { with_path = false }]

type typschm = Ast.typschm = TypSchm_aux of typschm_aux * l
[@@deriving yojson, show { with_path = false }]

type type_def_aux = Ast.type_def_aux =
  (* type definition body *)
  | TD_abbrev of id * typquant * typ_arg (* type abbreviation *)
  | TD_record of
      id * typquant * (typ * id) list * bool (* struct type definition *)
  | TD_variant of
      id * typquant * type_union list * bool (* tagged union type definition *)
  | TD_enum of id * id list * bool (* enumeration type definition *)
  | TD_bitfield of
      id
      * typ
      * (id * index_range) list (* register mutable bitfield type definition *)
[@@deriving yojson, show { with_path = false }]

type 'a fundef_aux = 'a Ast.fundef_aux =
  (* function definition *)
  | FD_function of 'a rec_opt * tannot_opt * 'a funcl list
[@@deriving yojson, show { with_path = false }]

type 'a mapdef_aux = 'a Ast.mapdef_aux =
  (* mapping definition (bidirectional pattern-match function) *)
  | MD_mapping of id * tannot_opt * 'a mapcl list
[@@deriving yojson, show { with_path = false }]

type subst_aux = Ast.subst_aux =
  (* instantiation substitution *)
  | IS_typ of kid * typ (* instantiate a type variable with a type *)
  | IS_id of id * id
(* instantiate an identifier with another identifier *)
[@@deriving yojson, show { with_path = false }]

type outcome_spec_aux = Ast.outcome_spec_aux =
  (* outcome declaration *)
  | OV_outcome of id * typschm * kinded_id list
[@@deriving yojson, show { with_path = false }]

type 'a instantiation_spec_aux = 'a Ast.instantiation_spec_aux = IN_id of id
[@@deriving yojson, show { with_path = false }]

type val_spec_aux = Ast.val_spec_aux =
  | VS_val_spec of typschm * id * extern option
[@@deriving yojson, show { with_path = false }]

type default_spec_aux = Ast.default_spec_aux =
  (* default kinding or typing assumption *)
  | DT_order of order
[@@deriving yojson, show { with_path = false }]

type 'a scattered_def_aux = 'a Ast.scattered_def_aux =
  (* scattered function and union type definitions *)
  | SD_function of
      'a rec_opt * tannot_opt * id (* scattered function definition header *)
  | SD_funcl of 'a funcl (* scattered function definition clause *)
  | SD_variant of id * typquant (* scattered union definition header *)
  | SD_unioncl of id * type_union (* scattered union definition member *)
  | SD_internal_unioncl_record of id * id * typquant * (typ * id) list
  | SD_mapping of id * tannot_opt
  | SD_mapcl of id * 'a mapcl
  | SD_enum of id
  | SD_enumcl of id * id
  | SD_end of id
(* scattered definition end *) [@@deriving yojson, show { with_path = false }]

type 'a dec_spec_aux = 'a Ast.dec_spec_aux =
  (* register declarations *)
  | DEC_reg of typ * id * 'a exp option
[@@deriving yojson, show { with_path = false }]

type 'a opt_default_aux = 'a Ast.opt_default_aux =
  (* optional default value for indexed vector expressions *)
  | Def_val_empty
  | Def_val_dec of 'a exp
[@@deriving yojson, show { with_path = false }]

type 'a impldef_aux = 'a Ast.impldef_aux =
  (* impl for target *)
  | Impl_impl of 'a funcl
[@@deriving yojson, show { with_path = false }]

type 'a type_def = 'a Ast.type_def = TD_aux of type_def_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a fundef = 'a Ast.fundef = FD_aux of 'a fundef_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a mapdef = 'a Ast.mapdef = MD_aux of 'a mapdef_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type subst = Ast.subst = IS_aux of subst_aux * l
[@@deriving yojson, show { with_path = false }]

type outcome_spec = Ast.outcome_spec = OV_aux of outcome_spec_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a instantiation_spec = 'a Ast.instantiation_spec =
  | IN_aux of 'a instantiation_spec_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a val_spec = 'a Ast.val_spec = VS_aux of val_spec_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type default_spec = Ast.default_spec = DT_aux of default_spec_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a scattered_def = 'a Ast.scattered_def =
  | SD_aux of 'a scattered_def_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a dec_spec = 'a Ast.dec_spec = DEC_aux of 'a dec_spec_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type prec = Ast.prec = Infix | InfixL | InfixR
[@@deriving yojson, show { with_path = false }]

type 'a loop_measure = 'a Ast.loop_measure = Loop of loop * 'a exp
[@@deriving yojson, show { with_path = false }]

type 'a opt_default = 'a Ast.opt_default =
  | Def_val_aux of 'a opt_default_aux * 'a annot
[@@deriving yojson, show { with_path = false }]

type 'a impldef = 'a Ast.impldef = Impl_aux of 'a impldef_aux * l
[@@deriving yojson, show { with_path = false }]

type 'a def_aux = 'a Ast.def_aux =
  (* top-level definition *)
  | DEF_type of 'a type_def (* type definition *)
  | DEF_fundef of 'a fundef (* function definition *)
  | DEF_mapdef of 'a mapdef (* mapping definition *)
  | DEF_impl of 'a funcl (* impl definition *)
  | DEF_let of 'a letbind (* value definition *)
  | DEF_val of 'a val_spec (* top-level type constraint *)
  | DEF_outcome of outcome_spec * 'a def list (* top-level outcome definition *)
  | DEF_instantiation of 'a instantiation_spec * subst list (* instantiation *)
  | DEF_fixity of prec * num * id (* fixity declaration *)
  | DEF_overload of id * id list (* operator overload specification *)
  | DEF_default of default_spec (* default kind and type assumptions *)
  | DEF_scattered of 'a scattered_def
    (* scattered function and type definition *)
  | DEF_measure of id * 'a pat * 'a exp
    (* separate termination measure declaration *)
  | DEF_loop_measures of
      id * 'a loop_measure list (* separate termination measure declaration *)
  | DEF_register of 'a dec_spec (* register declaration *)
  | DEF_internal_mutrec of 'a fundef list
    (* internal representation of mutually recursive functions *)
  | DEF_pragma of string * string * l (* compiler directive *)

and 'a def = 'a Ast.def = DEF_aux of 'a def_aux * def_annot
[@@deriving yojson, show { with_path = false }]

let pp_tannot ppf _ = Format.fprintf ppf "???"
let tannot_to_yojson _ = `Int 42

let tannot_of_yojson _ : Type_check.tannot Ppx_deriving_yojson_runtime.error_or
    =
  Result.Ok Type_check.empty_tannot

let save filename ast =
  print_endline __FUNCTION__;

  let _ : Type_check.tannot Ast_defs.ast = ast in

  Out_channel.with_open_bin filename (fun ch ->
      let j = `List (List.map (def_to_yojson tannot_to_yojson) ast.defs) in
      Yojson.Safe.pretty_to_channel ch j)
