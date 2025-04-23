(* generate z3_problems and pretty print sail's ast *)
let () =
  let ast, _, _ = Mysail.main Sys.argv in
  Myast.save "pp_ast.txt" ast
