  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "slli") { mnemonic="slli"; arch=RV32; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "srai") { mnemonic="srai"; arch=RV32; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "srli") { mnemonic="srli"; arch=RV32; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV64, "slli") { mnemonic="slli"; arch=RV64; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "srai") { mnemonic="srai"; arch=RV64; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "srli") { mnemonic="srli"; arch=RV64; operands=["rd"; "rs1"; "shamt"]; ins=[GPR "rs1"; Imm "shamt"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };

