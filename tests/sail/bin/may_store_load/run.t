  $ ARCH=RV32 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "enum.store") { mnemonic="enum.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "enum.load") { mnemonic="enum.load"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.enum.load") { mnemonic="deep.enum.load"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "not.load.not.store") { mnemonic="not.load.not.store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.load") { mnemonic="deep.load"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "translate.and.load") { mnemonic="translate.and.load"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.enum.store") { mnemonic="deep.enum.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "translate.and.store") { mnemonic="translate.and.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "load.store") { mnemonic="load.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "deep.store") { mnemonic="deep.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "load.not.store") { mnemonic="load.not.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "not.load.store") { mnemonic="not.load.store"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "not.store.load") { mnemonic="not.store.load"; arch=RV32; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32, "translate") { mnemonic="translate"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64 dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV64, "enum.store") { mnemonic="enum.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "enum.load") { mnemonic="enum.load"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.enum.load") { mnemonic="deep.enum.load"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "not.load.not.store") { mnemonic="not.load.not.store"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.load") { mnemonic="deep.load"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "translate.and.load") { mnemonic="translate.and.load"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.enum.store") { mnemonic="deep.enum.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "translate.and.store") { mnemonic="translate.and.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "load.store") { mnemonic="load.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "deep.store") { mnemonic="deep.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "load.not.store") { mnemonic="load.not.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=true; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "not.load.store") { mnemonic="not.load.store"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=true; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "not.store.load") { mnemonic="not.store.load"; arch=RV64; operands=["rd"; "rs1"; "imm"]; ins=[GPR "rs1"; Imm "imm"]; outs=[GPR "rd"]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "translate") { mnemonic="translate"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
