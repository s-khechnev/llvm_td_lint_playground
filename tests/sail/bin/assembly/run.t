  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "q0") { mnemonic="q0"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "c1") { mnemonic="c1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "b1") { mnemonic="b1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "a1") { mnemonic="a1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "with.bool.map.true.map") { mnemonic="with.bool.map.true.map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.mnemonic1") { mnemonic="enum.mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.mnemonic2") { mnemonic="enum.mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "trivial.asm.inst") { mnemonic="trivial.asm.inst"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.speced.mnemonic2") { mnemonic="enum.speced.mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "with.bool.map.false.map") { mnemonic="with.bool.map.false.map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.speced.mnemonic1") { mnemonic="enum.speced.mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "q0") { mnemonic="q0"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "c1") { mnemonic="c1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "b1") { mnemonic="b1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "a1") { mnemonic="a1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "with.bool.map.true.map") { mnemonic="with.bool.map.true.map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.mnemonic1") { mnemonic="enum.mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.mnemonic2") { mnemonic="enum.mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "trivial.asm.inst") { mnemonic="trivial.asm.inst"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.speced.mnemonic2") { mnemonic="enum.speced.mnemonic2"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "with.bool.map.false.map") { mnemonic="with.bool.map.false.map"; arch=RV32_RV64; operands=["rs"]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "enum.speced.mnemonic1") { mnemonic="enum.speced.mnemonic1"; arch=RV32_RV64; operands=["simm6"]; ins=[Imm "simm6"]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
