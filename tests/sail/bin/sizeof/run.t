  $ ARCH=RV32  dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV32.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32, "only.rv.32") { mnemonic="only.rv.32"; arch=RV32; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "first.rv.64.rv.32") { mnemonic="first.rv.64.rv.32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "another.rv.64.rv.32") { mnemonic="another.rv.64.rv.32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
  $ ARCH=RV64 OCAMLRUNPARAM=b dump_sail -ocaml-code sail_info.ml -ocaml-ident sail_info -dot graph.dot -- -no_warn $(cat ../sail_base_files_RV64.txt) ./sail.sail
  $ cat sail_info.ml | tail -n +6 | head -n -3
    InstrTable.add ans (RV32_RV64, "first.rv.64.rv.32") { mnemonic="first.rv.64.rv.32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV32_RV64, "another.rv.64.rv.32") { mnemonic="another.rv.64.rv.32"; arch=RV32_RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };
    InstrTable.add ans (RV64, "only.rv.64") { mnemonic="only.rv.64"; arch=RV64; operands=[]; ins=[]; outs=[]; mayLoad=false; mayStore=false; ins_csr=[]; outs_csr=[] };

