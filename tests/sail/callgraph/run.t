  $ cat funcs1.sail
  function h1() -> unit = ()
  function h2() -> unit = ()
  
  function g() -> unit = let _ = h1() in h2()
  
  function f() -> unit = g()
  $ dune exec ./callgraph_tester.exe funcs1.sail
  $ cat graph.dot
  digraph G {
    f;
    <h1 (0:())>;
    <g (0:())>;
    g;
    <h2 (0:())>;
    
    
    <h1 (0:())> -> g;
    <g (0:())> -> f;
    <h2 (0:())> -> g;
    
    }
  $ cat funcs2.sail
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
  $ dune exec ./callgraph_tester.exe funcs2.sail
  $ cat graph.dot
  digraph G {
    <add1 (0:5)>;
    <h (0:true)>;
    f;
    fmod_int;
    fdiv_int;
    my_print_int;
    <add1 (0:10)>;
    add1;
    
    
    <add1 (0:5)> -> f;
    <h (0:true)> -> f;
    fdiv_int -> fmod_int;
    my_print_int -> f;
    <add1 (0:10)> -> f;
    add1 -> f;
    
    }
