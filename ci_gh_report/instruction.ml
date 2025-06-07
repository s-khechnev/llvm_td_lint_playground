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

type t = {
  mnemonic : string;
  arch : Arch.t;
  operands : string list;
  ins : string list;
  outs : string list;
  mayLoad : bool;
  mayStore : bool;
}

let equal i1 i2 =
  i1.mnemonic = i2.mnemonic
  && List.equal String.equal i1.operands i2.operands
  && List.equal String.equal i1.ins i2.ins
  && List.equal String.equal i1.outs i2.outs

module InstrTable = Hashtbl.Make (struct
  type t = Arch.t * string

  let equal (a1, m1) (a2, m2) =
    if Arch.equal a1 a2 then String.equal m1 m2 else false

  let hash (_, mnemonic) = Hashtbl.hash mnemonic
end)
