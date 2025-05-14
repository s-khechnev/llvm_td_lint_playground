
(rule
 (targets sail_info_RV32.ml graph_RV32.dot)
 (deps dump.exe z3_problems sail_files_RV32.txt)
 (mode
  (promote (until-clean)))
 (action
  (setenv
   ARCH
   RV32
   (run
    ./dump.exe
    -ocaml-code
    sail_info_RV32.ml
    -ocaml-ident
    sail_info
    -dot
    graph_RV32.dot
    --
    -memo_z3
    %{read-lines:sail_files_RV32.txt}))))

(rule
 (targets sail_info_RV64.ml graph_RV64.dot)
 (deps dump.exe z3_problems sail_files_RV64.txt)
 (mode
  (promote (until-clean)))
 (action
  (setenv
   ARCH
   RV64
   (run
    ./dump.exe
    -ocaml-code
    sail_info_RV64.ml
    -ocaml-ident
    sail_info
    -dot
    graph_RV64.dot
    --
    -memo_z3
    %{read-lines:sail_files_RV64.txt}))))
