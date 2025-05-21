module Arch = struct
  type t = RV32 | RV64 | RV32_RV64

  let equal a b =
    match (a, b) with
    | RV32, RV32
    | RV32, RV32_RV64
    | RV32_RV64, RV32
    | RV64, RV64
    | RV64, RV32_RV64
    | RV32_RV64, RV64
    | RV32_RV64, RV32_RV64 ->
        true
    | _ -> false

  let compare a1 a2 =
    if equal a1 a2 then 0
    else if Hashtbl.hash a1 < Hashtbl.hash a2 then -1
    else 1

  let to_string = function
    | RV32 -> "RV32"
    | RV64 -> "RV64"
    | RV32_RV64 -> "RV32_RV64"
end

module Operand = struct
  type t = Imm of string | GPR of string | GPRPair of string

  let equal_t a b =
    match (a, b) with
    | Imm _, Imm _ | GPR _, GPR _ | GPRPair _, GPRPair _ -> true
    | _ -> false

  let equal a b =
    match (a, b) with
    | Imm s1, Imm s2 | GPR s1, GPR s2 | GPRPair s1, GPRPair s2 ->
        String.equal s1 s2
    | _ -> false

  let to_string = function
    | Imm s | GPR s -> s
    | GPRPair s -> Format.sprintf "%s : PairGPR" s

  let get = function Imm s | GPR s | GPRPair s -> s
end

type t = {
  mnemonic : string;
  arch : Arch.t;
  operands : string list;
  ins : Operand.t list;
  outs : Operand.t list;
  mayLoad : bool;
  mayStore : bool;
  ins_csr : string list;
  outs_csr : string list;
}

let equal i1 i2 =
  i1.mnemonic = i2.mnemonic
  && List.equal String.equal i1.operands i2.operands
  && List.equal Operand.equal i1.ins i2.ins
  && List.equal Operand.equal i1.outs i2.outs
  && i1.mayLoad = i2.mayLoad && i1.mayStore = i2.mayStore
  && List.equal String.equal i1.ins_csr i2.ins_csr
  && List.equal String.equal i1.outs_csr i2.outs_csr

module InstrTable = Hashtbl.Make (struct
  type t = Arch.t * string

  let equal (a1, m1) (a2, m2) =
    if Arch.equal a1 a2 then String.equal m1 m2 else false

  let hash (_, mnemonic) = Hashtbl.hash mnemonic
end)
