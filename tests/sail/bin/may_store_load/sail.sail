enum store_load_op = { E_Store, E_Load }

///

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

union clause ast = NOT_STORE_LOAD : (bits(12), regidx, regidx)

function clause execute NOT_STORE_LOAD(imm, rs1, rd) = {
  let rs1_val = X(rs1);
  let immext : xlenbits = sign_extend(imm);
  let result : xlenbits = rs1_val + immext;
  X(rd) = result;
  RETIRE_SUCCESS
}

mapping clause assembly = NOT_STORE_LOAD(imm, rs1, rd)
  <-> "not_store_load" ^ spc() ^ reg_name(rd) ^ sep() ^ reg_name(rs1) ^ sep() ^ hex_bits_signed_12(imm)

///

function load() -> unit = {
  let _ = read_ram(Read_plain, zeros(), 1, false);
  ()
}

function store() -> unit = {
  let _ = write_ram(Write_plain, zeros(), 1, zeros(), ());
  ()
}

function translateAddr() -> unit = {
  load();
  store()
}

mapping map_load : bool <-> string = {
  true  <-> "load",
  false <-> "NOT_LOAD"
}

mapping map_store : bool <-> string = {
  true  <-> "store",
  false <-> "NOT_STORE"
}

union clause ast = MAY_IF : (bool, bool)

function clause execute MAY_IF(load_flag, store_flag) = {
  if load_flag then load()
  else ();

  if store_flag then store()
  else (); 

  RETIRE_SUCCESS
}

mapping clause assembly = MAY_IF(flag1, flag2)
  <-> map_load(flag1) ^ "_" ^ map_store(flag2)

//

union clause ast = TRANSLATE : unit

function clause execute TRANSLATE() = {
  translateAddr();

  RETIRE_SUCCESS
}

mapping clause assembly = TRANSLATE()
  <-> "translate"

//

union clause ast = TRANSLATE_LOAD : unit

function clause execute TRANSLATE_LOAD() = {
  translateAddr();
  load();

  RETIRE_SUCCESS
}

mapping clause assembly = TRANSLATE_LOAD()
  <-> "translate_and_load"

///

union clause ast = TRANSLATE_STORE : unit

function clause execute TRANSLATE_STORE() = {
  translateAddr();
  store();

  RETIRE_SUCCESS
}

mapping clause assembly = TRANSLATE_STORE()
  <-> "translate_and_store"

///

union clause ast = DEEP_LOAD : unit

function g() -> unit = load()

function f() -> unit = g()

function clause execute DEEP_LOAD() = {
  let _ = f();

  RETIRE_SUCCESS
}

mapping clause assembly = DEEP_LOAD()
  <-> "deep_load"

///

union clause ast = DEEP_STORE : unit

function k() -> unit = store()

function h() -> unit = k()

function clause execute DEEP_STORE() = {
  let _ = h();

  RETIRE_SUCCESS
}

mapping clause assembly = DEEP_STORE()
  <-> "deep_store"

///

union clause ast = ENUM_STORE_LOAD : store_load_op

function clause execute ENUM_STORE_LOAD(store_load_op) = {
  match store_load_op {
    E_Store => store(),
    E_Load => load()
  };

  RETIRE_SUCCESS
}

mapping enum_map : store_load_op <-> string = {
  E_Store <-> "store",
  E_Load <-> "load"
}

mapping clause assembly = ENUM_STORE_LOAD(store_load_op)
  <-> "enum_" ^ enum_map(store_load_op)

///

union clause ast = DEEP_ENUM_STORE_LOAD : store_load_op

function foo(store_load_op) : store_load_op -> unit = {
  match store_load_op {
      E_Store => store(),
      E_Load => load()
  };
}

function clause execute DEEP_ENUM_STORE_LOAD(store_load_op) = {
  foo(store_load_op);

  RETIRE_SUCCESS
}

mapping clause assembly = DEEP_ENUM_STORE_LOAD(store_load_op)
  <-> "deep_enum_" ^ enum_map(store_load_op)

///


end ast
end execute
end assembly
end encdec
end encdec_compressed
