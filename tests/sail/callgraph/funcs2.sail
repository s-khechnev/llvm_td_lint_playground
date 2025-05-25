$include <arith.sail>

function h(flag : bool) -> int = 
    if flag then 1 else 0

function add1(i : int) -> int = i + 1

function my_print_int(i : int) -> unit = ()

function f() -> unit = 
    let foo = add1(h(true)) in
    let _ = add1(10) in
    let _ = add1(5) in
    my_print_int(foo)
