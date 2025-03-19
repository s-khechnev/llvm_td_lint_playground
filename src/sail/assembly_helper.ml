type implementation_kind =
  | IK_straight of string  (** [C_MUL(rsdc, rs2c)] *)
  | IK_singledef of string * string
      (** [ZICOND_RTYPE(rs2, rs1, rd, RISCV_CZERO_EQZ)] *)

let add_straight ans mnemonic sail_name opers =
  Hashtbl.add ans mnemonic (IK_straight sail_name, opers)

let add_singledef ans mnemonic sail_name arg opers =
  Hashtbl.add ans mnemonic (IK_singledef (sail_name, arg), opers)
