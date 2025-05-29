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

union clause ast = UNUSED_READ : (regidx)

function clause execute UNUSED_READ(rs) = {
  let rs1_val = X(rs);

  RETIRE_SUCCESS
}

mapping clause assembly = UNUSED_READ(rs)
  <-> "unused_read" ^ spc() ^ reg_name(rs)

///

union clause ast = UNUSED_READ_SPEC : (regidx, bool)

function clause execute UNUSED_READ_SPEC(rs, flag) = {
  X(rs) = if flag then X(rs)
          else zeros();

  RETIRE_SUCCESS
}

mapping can_read : bool <-> string = {
  true <-> "read",
  false <-> "NOT_READ_RS"
}

mapping clause assembly = UNUSED_READ_SPEC(rs, flag)
  <-> "spec_" ^ can_read(flag) ^ spc() ^ reg_name(rs)

///

union clause ast = UNUSED_READ_SPEC_DEEP : (regidx, bool)

function f(flag : bool, rs : regidx) -> xlenbits = {
  let rs_val = X(rs);
  if flag then rs_val else zeros()
}

function clause execute UNUSED_READ_SPEC_DEEP(rs, flag) = {
  X(rs) = f(flag, rs);

  RETIRE_SUCCESS
}

mapping clause assembly = UNUSED_READ_SPEC_DEEP(rs, flag)
  <-> "deep_spec_" ^ can_read(flag) ^ spc() ^ reg_name(rs)


end ast
end execute
end assembly
end encdec
end encdec_compressed
