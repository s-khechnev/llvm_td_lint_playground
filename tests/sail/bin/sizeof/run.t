  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "only_rv_32") { mnemonic="only_rv_32"; arch=RV32; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "first_rv_64_rv_32") { mnemonic="first_rv_64_rv_32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "another_rv_64_rv_32") { mnemonic="another_rv_64_rv_32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "first_rv_64_rv_32") { mnemonic="first_rv_64_rv_32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "another_rv_64_rv_32") { mnemonic="another_rv_64_rv_32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV64, "only_rv_64") { mnemonic="only_rv_64"; arch=RV64; operands=[]; ins=[]; outs=[] };

