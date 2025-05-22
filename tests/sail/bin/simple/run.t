  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "auipc") { mnemonic="auipc"; arch=RV32; operands=["rd"; "imm"]; ins=["imm"]; outs=["rd"] };
    InstrTable.add ans (RV32, "lui") { mnemonic="lui"; arch=RV32; operands=["rd"; "imm"]; ins=["imm"]; outs=["rd"] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV64, "auipc") { mnemonic="auipc"; arch=RV64; operands=["rd"; "imm"]; ins=["imm"]; outs=["rd"] };
    InstrTable.add ans (RV64, "lui") { mnemonic="lui"; arch=RV64; operands=["rd"; "imm"]; ins=["imm"]; outs=["rd"] };

