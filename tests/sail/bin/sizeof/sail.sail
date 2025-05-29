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

union clause ast = ONLY_RV32 : unit

mapping clause encdec = ONLY_RV32()
      if sizeof(xlen) == 32
  <-> 0b00000000000000000000000000000000
      if sizeof(xlen) == 32


function clause execute ONLY_RV32() =
  RETIRE_SUCCESS

mapping clause assembly = ONLY_RV32()
  <-> "only_rv_32"

///

union clause ast = ONLY_RV64 : unit

mapping clause encdec = ONLY_RV64()
      if sizeof(xlen) == 64
  <-> 0b00000000000000000000000000000000
      if sizeof(xlen) == 64


function clause execute ONLY_RV64() =
  RETIRE_SUCCESS

mapping clause assembly = ONLY_RV64()
  <-> "only_rv_64"

///

union clause ast = RV32_RV64 : unit

mapping clause encdec = RV32_RV64()
      if sizeof(xlen) == 64
  <-> 0b00000000000000000000000000000001
      if sizeof(xlen) == 64

mapping clause encdec = RV32_RV64()
      if sizeof(xlen) == 32
  <-> 0b00000000000000000000000000000001
      if sizeof(xlen) == 32


function clause execute RV32_RV64() =
  RETIRE_SUCCESS

mapping clause assembly = RV32_RV64()
  <-> "first_rv_64_rv_32"

///

union clause ast = RV32_RV64_ANOTHER : unit

mapping clause encdec = RV32_RV64_ANOTHER()
  <-> 0b00000000000000000000000000000000

function clause execute RV32_RV64_ANOTHER() =
  RETIRE_SUCCESS

mapping clause assembly = RV32_RV64_ANOTHER()
  <-> "another_rv_64_rv_32"

///

end ast
end execute
end assembly
end encdec
end encdec_compressed
