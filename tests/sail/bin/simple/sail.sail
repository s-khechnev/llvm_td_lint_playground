enum clause extension = Test

scattered union ast

val execute : ast -> Retired
scattered function execute

val assembly : ast <-> string
scattered mapping assembly

val encdec : ast <-> bits(32)
scattered mapping encdec

val encdec_compressed : ast <-> bits(16)
scattered mapping encdec_compressed

///

union clause ast = SHIFTIOP : (bits(6), regidx, regidx, sop)

mapping encdec_sop : sop <-> bits(3) = {
  RISCV_SLLI <-> 0b001,
  RISCV_SRLI <-> 0b101,
  RISCV_SRAI <-> 0b101
}

mapping clause encdec = SHIFTIOP(shamt, rs1, rd, RISCV_SLLI) <-> 0b000000 @ shamt @ rs1 @ 0b001 @ rd @ 0b0010011 if sizeof(xlen) == 64 | shamt[5] == bitzero
mapping clause encdec = SHIFTIOP(shamt, rs1, rd, RISCV_SRLI) <-> 0b000000 @ shamt @ rs1 @ 0b101 @ rd @ 0b0010011 if sizeof(xlen) == 64 | shamt[5] == bitzero
mapping clause encdec = SHIFTIOP(shamt, rs1, rd, RISCV_SRAI) <-> 0b010000 @ shamt @ rs1 @ 0b101 @ rd @ 0b0010011 if sizeof(xlen) == 64 | shamt[5] == bitzero

function clause execute (SHIFTIOP(shamt, rs1, rd, op)) = {
  let rs1_val = X(rs1);
  /* the decoder guard should ensure that shamt[5] = 0 for RV32 */
  let result : xlenbits = match op {
    RISCV_SLLI => if   sizeof(xlen) == 32
                  then rs1_val << shamt[4..0]
                  else rs1_val << shamt,
    RISCV_SRLI => if   sizeof(xlen) == 32
                  then rs1_val >> shamt[4..0]
                  else rs1_val >> shamt,
    RISCV_SRAI => if   sizeof(xlen) == 32
                  then shift_right_arith32(rs1_val, shamt[4..0])
                  else shift_right_arith64(rs1_val, shamt)
  };
  X(rd) = result;
  RETIRE_SUCCESS
}

mapping shiftiop_mnemonic : sop <-> string = {
  RISCV_SLLI <-> "slli",
  RISCV_SRLI <-> "srli",
  RISCV_SRAI <-> "srai"
}

mapping clause assembly = SHIFTIOP(shamt, rs1, rd, op)
  <-> shiftiop_mnemonic(op) ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1) ^ sep() ^ hex_bits_6(shamt)

///

end ast
end execute
end assembly
end encdec
end encdec_compressed
