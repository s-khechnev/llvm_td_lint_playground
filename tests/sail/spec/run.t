  $ cat funcs.sail
  enum op_enum = { OP_A, OP_B, OP_C }
  
  function spec_by_bool(flag : bool) -> unit = ()
  
  function spec_by_enum (rs : bitvector(5), op : op_enum) -> unit = ()
  
  function f_speced_true (rs : bitvector(5), true) -> unit = ()
  
  function f_speced_false (rs : bitvector(5), false) -> unit = ()
  
  function complicated (n : int, false, true, false, flag : bool, op : op_enum) -> unit = ()
  $ ./spec_tester.exe funcs.sail
  op_enum_of_num args to spec: 
  op_enum_of_num speced args: 
  num_of_op_enum args to spec: (0, arg#, [ OP_A; OP_B; OP_C ])
  num_of_op_enum speced args: 
  spec_by_bool args to spec: (0, flag, [ false; true ])
  spec_by_bool speced args: 
  spec_by_enum args to spec: (1, op, [ OP_A; OP_B; OP_C ])
  spec_by_enum speced args: 
  f_speced_true args to spec: 
  f_speced_true speced args: (1, true)
  f_speced_false args to spec: 
  f_speced_false speced args: (1, false)
  complicated args to spec: (4, flag, [ false; true ]) (5, op, [ OP_A; OP_B; OP_C ])
  complicated speced args: (1, false) (2, true) (3, false)
