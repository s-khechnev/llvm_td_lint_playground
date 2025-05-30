  $ ARCH=RV32 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "spec_read") { mnemonic="spec_read"; arch=RV32_RV64; operands=["rs"]; ins=["rs"]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32, "spec_NOT_READ_RS") { mnemonic="spec_NOT_READ_RS"; arch=RV32; operands=["rs"]; ins=[]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_spec_read") { mnemonic="deep_spec_read"; arch=RV32_RV64; operands=["rs"]; ins=["rs"]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "unused_read") { mnemonic="unused_read"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_spec_NOT_READ_RS") { mnemonic="deep_spec_NOT_READ_RS"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=["rs"]; mayLoad=false; mayStore=false };
  $ ARCH=RV64 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "spec_read") { mnemonic="spec_read"; arch=RV32_RV64; operands=["rs"]; ins=["rs"]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV64, "spec_NOT_READ_RS") { mnemonic="spec_NOT_READ_RS"; arch=RV64; operands=["rs"]; ins=[]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_spec_read") { mnemonic="deep_spec_read"; arch=RV32_RV64; operands=["rs"]; ins=["rs"]; outs=["rs"]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "unused_read") { mnemonic="unused_read"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false };
    InstrTable.add ans (RV32_RV64, "deep_spec_NOT_READ_RS") { mnemonic="deep_spec_NOT_READ_RS"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=["rs"]; mayLoad=false; mayStore=false };
