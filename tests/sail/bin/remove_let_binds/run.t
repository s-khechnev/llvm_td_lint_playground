  $ ARCH=RV32 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "spec.read") { mnemonic="spec.read"; arch=RV32_RV64; operands=["rs"]; ins=[GPR "rs"]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "spec.not.read.rs") { mnemonic="spec.not.read.rs"; arch=RV32; operands=["rs"]; ins=[]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "deep.spec.read") { mnemonic="deep.spec.read"; arch=RV32_RV64; operands=["rs"]; ins=[GPR "rs"]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "unused.read") { mnemonic="unused.read"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "deep.spec.not.read.rs") { mnemonic="deep.spec.not.read.rs"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "spec.read") { mnemonic="spec.read"; arch=RV32_RV64; operands=["rs"]; ins=[GPR "rs"]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "spec.not.read.rs") { mnemonic="spec.not.read.rs"; arch=RV64; operands=["rs"]; ins=[]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "deep.spec.read") { mnemonic="deep.spec.read"; arch=RV32_RV64; operands=["rs"]; ins=[GPR "rs"]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "unused.read") { mnemonic="unused.read"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "deep.spec.not.read.rs") { mnemonic="deep.spec.not.read.rs"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[GPR "rs"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
