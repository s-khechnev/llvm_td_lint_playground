function h1() -> unit = ()
function h2() -> unit = ()

function g() -> unit = let _ = h1() in h2()

function f() -> unit = g()
