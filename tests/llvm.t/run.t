  $ ls
  llvm_data.json
  $ dump_llvm -ocaml-code llvm_info.ml -ocaml-ident llvm_info -dump-llvm llvm_data.json -o-filtered filtered.json
  $ cat llvm_info.ml
  (* This file was auto generated *)
  open Checker_core.Instruction
  
  let llvm_info =
    let ans = InstrTable.create 1000 in
    InstrTable.add ans (RV32_RV64, "epmty_ins") { mnemonic="epmty_ins"; arch=RV32_RV64; operands=[]; ins=["sp"]; outs=["rd"; "sp"] };
    InstrTable.add ans (RV32_RV64, "add") { mnemonic="add"; arch=RV32_RV64; operands=["rd"; "rs1"; "rs2"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    InstrTable.add ans (RV32_RV64, "addi") { mnemonic="addi"; arch=RV32_RV64; operands=["rd"; "rs1"; "imm12"]; ins=["rs1"; "imm12"]; outs=["rd"] };
    InstrTable.add ans (RV64, "addiw") { mnemonic="addiw"; arch=RV64; operands=["rd"; "rs1"; "imm12"]; ins=["rs1"; "imm12"]; outs=["rd"] };
    InstrTable.add ans (RV64, "addw") { mnemonic="addw"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    InstrTable.add ans (RV64, "add.uw") { mnemonic="add.uw"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    InstrTable.add ans (RV32, "aes32dsi") { mnemonic="aes32dsi"; arch=RV32; operands=["rd"; "rs1"; "rs2"; "bs"]; ins=["rs1"; "rs2"; "bs"]; outs=["rd"] };
    InstrTable.add ans (RV32, "aes32dsmi") { mnemonic="aes32dsmi"; arch=RV32; operands=["rd"; "rs1"; "rs2"; "bs"]; ins=["rs1"; "rs2"; "bs"]; outs=["rd"] };
    InstrTable.add ans (RV32, "aes32esi") { mnemonic="aes32esi"; arch=RV32; operands=["rd"; "rs1"; "rs2"; "bs"]; ins=["rs1"; "rs2"; "bs"]; outs=["rd"] };
    InstrTable.add ans (RV64, "aes64ds") { mnemonic="aes64ds"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    InstrTable.add ans (RV64, "aes64dsm") { mnemonic="aes64dsm"; arch=RV64; operands=["rd"; "rs1"; "rs2"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    InstrTable.add ans (RV32_RV64, "amoadd.b") { mnemonic="amoadd.b"; arch=RV32_RV64; operands=["rd"; "rs2"; "rs1"]; ins=["rs1"; "rs2"]; outs=["rd"] };
    ans
     let lookup_exn = InstrTable.find llvm_info 
