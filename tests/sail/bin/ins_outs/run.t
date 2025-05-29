  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "andi") { mnemonic="andi"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "sltiu") { mnemonic="sltiu"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "addi") { mnemonic="addi"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "xori") { mnemonic="xori"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "ori") { mnemonic="ori"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "slti") { mnemonic="slti"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "addi") { mnemonic="addi"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV64, "andi") { mnemonic="andi"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "sltiu") { mnemonic="sltiu"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "addi") { mnemonic="addi"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "xori") { mnemonic="xori"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "ori") { mnemonic="ori"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "slti") { mnemonic="slti"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "addi") { mnemonic="addi"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
