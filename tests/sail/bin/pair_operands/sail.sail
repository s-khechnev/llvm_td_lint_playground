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

val smul : forall 'n, 'n == 8 | 'n == 16 | 'n == 32.
  (bits('n), bits('n)) -> bits(2 * 'n)
function smul (v1, v2) =
  to_bits (2 * 'n, signed(v1) * signed(v2))

///

union clause ast = PEXT_SMAL : (regidx, regidx, regidx)

function clause execute (PEXT_SMAL(rs2, rs1, rd)) = {
    let rs1_val = X(rs1);
    let rs2_val = X(rs2);
    if sizeof(xlen) == 32
    then {
        let rs10 = (rs1 >> 1) << 1 ;
        let rs11 = rs10 + 1 ;
        let rd0 = (rd >> 1) << 1 ;
        let rd1 = rd0 + 1 ;
        let result : bits(64) = (X(rs11) @ X(rs10)) + EXTS(smul(slice(rs2_val, 16, 16), slice(rs2_val, 0, 16))); 
        X(rd1) = result[63..32];
        X(rd0) = result[31..0];
    } else
        X(rd)  = rs1_val + EXTS(smul(slice(rs2_val, 48, 16), slice(rs2_val, 32, 16))) + EXTS(smul(slice(rs2_val, 16, 16), slice(rs2_val, 0, 16)));
    RETIRE_SUCCESS
}

mapping clause assembly = PEXT_SMAL(rs2, rs1, rd)
  <-> "SMAL" ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1) ^ sep() ^ reg_name(rs2)

///

union clause ast = PEXT_64BITS_ADD_SUB : (regidx, regidx, regidx)

function clause execute (PEXT_64BITS_ADD_SUB(rs2, rs1, rd)) = {
  results : bits(64) = zeros();
  rs1_val : bits(64) = zeros();
  rs2_val : bits(64) = zeros();
  if sizeof(xlen) == 32
  then {
    rs1_val = X(rs1 | 0b00001) @ X(rs1 & 0b11110);
    rs2_val = X(rs2 | 0b00001) @ X(rs2 & 0b11110);
  }
  else {
    rs1_val = X(rs1);
    rs2_val = X(rs2);
  }; 
  results = rs1_val + rs2_val;
  if sizeof(xlen) == 32
  then {
    X(rd | 0b00001) = results[63..32];
    X(rd & 0b11110) = results[31..0];
  }  
  else X(rd) = results;
  RETIRE_SUCCESS
}

mapping clause assembly = PEXT_64BITS_ADD_SUB(rs2, rs1, rd)
  <-> "ADD64" ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1) ^ sep() ^ reg_name(rs2)

///

union clause ast = DEEP_PAIR : (regidx, regidx)

function f(rs : regidx) -> bits(64) = {
  if sizeof(xlen) == 32
  then {
    let rs10 = (rs >> 1) << 1;
    let rs11 = rs10 + 1;
    X(rs10) @ X(rs11)
  } else {
    X(rs)
  }
}

function clause execute (DEEP_PAIR(rs1, rd)) = {
  results : bits(64) = zeros();
  rs1_val : bits(64) = zeros();
  if sizeof(xlen) == 32
  then {
    rs1_val = f(rs1);
  }
  else {
    rs1_val = X(rs1);
  }; 
  results = rs1_val + rs1_val;
  if sizeof(xlen) == 32
  then {
    X(rd | 0b00001) = results[63..32];
    X(rd & 0b11110) = results[31..0];
  }  
  else X(rd) = results;
  RETIRE_SUCCESS
}

mapping clause assembly = DEEP_PAIR(rs1, rd)
  <-> "deep_pair" ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1)

///

union clause ast = DEEP_PAIR_1 : (regidx, regidx)

function readGPR1(rs : regidx) -> xlenbits = {
    X(rs)
}

function readGPR2(rs : regidx) -> xlenbits = {
    X(rs)
}

function clause execute (DEEP_PAIR_1(rs1, rd)) = {
  results : bits(64) = zeros();
  rs1_val : bits(64) = zeros();
  if sizeof(xlen) == 32
  then {
    rs1_val = readGPR1(rs1 | 0b00001) @ readGPR2(rs1 & 0b11110);
  }
  else {
    rs1_val = readGPR1(rs1);
  }; 
  results = rs1_val + rs1_val;
  if sizeof(xlen) == 32
  then {
    X(rd | 0b00001) = results[63..32];
    X(rd & 0b11110) = results[31..0];
  }  
  else X(rd) = results;
  RETIRE_SUCCESS
}

mapping clause assembly = DEEP_PAIR_1(rs1, rd)
  <-> "deep_pair_1" ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1)


end ast
end execute
end assembly
end encdec
end encdec_compressed
