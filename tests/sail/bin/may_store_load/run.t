  $ ARCH=RV32 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "enum_store") { mnemonic="enum_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "enum_load") { mnemonic="enum_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_enum_load") { mnemonic="deep_enum_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "NOT_LOAD_NOT_STORE") { mnemonic="NOT_LOAD_NOT_STORE"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_load") { mnemonic="deep_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "translate_and_load") { mnemonic="translate_and_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_enum_store") { mnemonic="deep_enum_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "translate_and_store") { mnemonic="translate_and_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "load_store") { mnemonic="load_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=true };
    InstrTable.add ans (RV32_RV64, "deep_store") { mnemonic="deep_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "load_NOT_STORE") { mnemonic="load_NOT_STORE"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "NOT_LOAD_store") { mnemonic="NOT_LOAD_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32, "not_store_load") { mnemonic="not_store_load"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=["rs1"; "imm"]; outs=["rd"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "translate") { mnemonic="translate"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
  $ ARCH=RV64 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "enum_store") { mnemonic="enum_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "enum_load") { mnemonic="enum_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_enum_load") { mnemonic="deep_enum_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "NOT_LOAD_NOT_STORE") { mnemonic="NOT_LOAD_NOT_STORE"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_load") { mnemonic="deep_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "translate_and_load") { mnemonic="translate_and_load"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_enum_store") { mnemonic="deep_enum_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "translate_and_store") { mnemonic="translate_and_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "load_store") { mnemonic="load_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=true };
    InstrTable.add ans (RV32_RV64, "deep_store") { mnemonic="deep_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV32_RV64, "load_NOT_STORE") { mnemonic="load_NOT_STORE"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false };
    InstrTable.add ans (RV32_RV64, "NOT_LOAD_store") { mnemonic="NOT_LOAD_store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true };
    InstrTable.add ans (RV64, "not_store_load") { mnemonic="not_store_load"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=["rs1"; "imm"]; outs=["rd"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "translate") { mnemonic="translate"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
