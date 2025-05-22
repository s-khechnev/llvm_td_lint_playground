  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "q0") { mnemonic="q0"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "c1") { mnemonic="c1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "b1") { mnemonic="b1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "a1") { mnemonic="a1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "with_bool_map.true_map") { mnemonic="with_bool_map.true_map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_mnemonic1") { mnemonic="enum_mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_mnemonic2") { mnemonic="enum_mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "trivial_asm_inst") { mnemonic="trivial_asm_inst"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_speced_mnemonic2") { mnemonic="enum_speced_mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "with_bool_map.false_map") { mnemonic="with_bool_map.false_map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_speced_mnemonic1") { mnemonic="enum_speced_mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "q0") { mnemonic="q0"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "c1") { mnemonic="c1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "b1") { mnemonic="b1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "a1") { mnemonic="a1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "with_bool_map.true_map") { mnemonic="with_bool_map.true_map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_mnemonic1") { mnemonic="enum_mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_mnemonic2") { mnemonic="enum_mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "trivial_asm_inst") { mnemonic="trivial_asm_inst"; arch=RV32_RV64; operands=[]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_speced_mnemonic2") { mnemonic="enum_speced_mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
    InstrTable.add ans (RV32_RV64, "with_bool_map.false_map") { mnemonic="with_bool_map.false_map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[] };
    InstrTable.add ans (RV32_RV64, "enum_speced_mnemonic1") { mnemonic="enum_speced_mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=["simm6"]; outs=[] };
