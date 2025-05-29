  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "add64") { mnemonic="add64"; arch=RV32; operands=["rd"; "rs1"; "rs2"]; ins=[GPRPair "rs1"; GPRPair "rs2"]; outs=[GPRPair "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.pair") { mnemonic="deep.pair"; arch=RV32; operands=["rd"; "rs1"]; ins=[GPRPair "rs1"]; outs=[GPRPair "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "smal") { mnemonic="smal"; arch=RV32; operands=["rd"; "rs1"; "rs2"]; ins=[GPRPair "rs1"; GPR "rs2"]; outs=[GPRPair "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.pair.1") { mnemonic="deep.pair.1"; arch=RV32; operands=["rd"; "rs1"]; ins=[GPRPair "rs1"]; outs=[GPRPair "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV64, "add64") { mnemonic="add64"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=[GPR "rs1"; GPR "rs2"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.pair") { mnemonic="deep.pair"; arch=RV64; operands=["rd"; "rs1"]; ins=[GPR "rs1"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "smal") { mnemonic="smal"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=[GPR "rs1"; GPR "rs2"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.pair.1") { mnemonic="deep.pair.1"; arch=RV64; operands=["rd"; "rs1"]; ins=[GPR "rs1"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
