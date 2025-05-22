enum op_enum = { OP_A, OP_B, OP_C }

function spec_by_bool(flag : bool) -> unit = ()

function spec_by_enum (rs : bitvector(5), op : op_enum) -> unit = ()

function f_speced_true (rs : bitvector(5), true) -> unit = ()

function f_speced_false (rs : bitvector(5), false) -> unit = ()

function complicated (n : int, false, true, false, flag : bool, op : op_enum) -> unit = ()
