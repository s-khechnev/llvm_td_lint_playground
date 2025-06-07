let gen_rule arch =
  Printf.printf
    {|
(rule
 (targets sail_info_%s.ml graph_%s.dot)
 (deps dump.exe z3_problems sail_files_%s.txt)
 (mode
  (promote (until-clean)))
 (action
  (setenv
   ARCH
   %s
   (run
    ./dump.exe
    -ocaml-code
    sail_info_%s.ml
    -ocaml-ident
    sail_info
    -dot
    graph_%s.dot
    --
    -memo_z3
    %%{read-lines:sail_files_%s.txt}))))
|}
    arch arch arch arch arch arch arch

let () = List.iter gen_rule [ "RV32"; "RV64" ]
