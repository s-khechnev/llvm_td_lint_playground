enum my_enum = { E_value1, E_value2 }

enum already_speced = { S_value1, S_value2 }

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

union clause ast = TRIVIAL : unit

mapping clause assembly = TRIVIAL()
  <-> "trivial_asm_inst"

///

union clause ast = SPEC_BY_BOOL : (bool, regidx)

val map_mnemonic : bool <-> string
mapping map_mnemonic = {
  true   <-> "true_map",
  false  <-> "false_map"
}

mapping clause assembly = SPEC_BY_BOOL(flag, rs)
  <-> "with_bool_map." ^ map_mnemonic(flag) ^ spc() ^ reg_name(rs)

///

union clause ast = SPEC_BY_ENUM : (my_enum, bits(6))

mapping my_enum_mnemonic : string <-> my_enum = {
  "enum_mnemonic1" <-> E_value1,
  "enum_mnemonic2" <-> E_value2
}

mapping clause assembly = SPEC_BY_ENUM(op, simm6)
  <-> my_enum_mnemonic(op) ^ spc() ^ hex_bits_signed_6(simm6)

///

union clause ast = SPECED_BY_ENUM : (already_speced, bits(6))

mapping speced_mnemonic : string <-> already_speced = {
  "enum_speced_mnemonic1" <-> S_value1,
  "enum_speced_mnemonic2" <-> S_value2
}

mapping clause assembly = SPECED_BY_ENUM(S_value1, simm6)
  <-> speced_mnemonic(S_value1) ^ spc() ^ hex_bits_signed_6(simm6)

mapping clause assembly = SPECED_BY_ENUM(S_value2, simm6)
  <-> speced_mnemonic(S_value2) ^ spc() ^ hex_bits_signed_6(simm6)

///

union clause ast = ENUMERATE_MAP : (bits(3), bits(6))

mapping some_bits_map : bits(3) <-> string = {
  0b000 <-> "q0",
  0b001 <-> "c1",
  0b010 <-> "b1",
  0b100 <-> "a1"
}

mapping clause assembly = ENUMERATE_MAP(triplet, simm6)
  <-> some_bits_map(triplet) ^ spc() ^ hex_bits_signed_8(simm6 @ 0b01)

///

end ast
end execute
end assembly
end encdec
end encdec_compressed
