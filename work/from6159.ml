(* This file was auto generated *)
include From6159_helper

let from6159 =
  let ans = Hash_info.create 1000 in
  hacky ans "RISCV_PACK" "ZBKB_RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_BCLR" "ZBS_RTYPE" 3 {out=["rd"]};
  def ans  "F_UN_TYPE_D" {out=["rd"]};
  def ans  "F_UN_TYPE_D" {out=["rd"]};
  def ans  "F_UN_TYPE_D" {out=["rd"]};
  def ans  "C_LH" {out=["rd"]};
  def ans  "RISCV_REV8" {out=["rd"]};
  hacky ans "RISCV_ROLW" "ZBB_RTYPEW" 3 {out=["rd"]};
  def ans  "C_NOP_HINT" {out=[]};
  def ans  "WXTYPE" {out=["vd"]};
  def ans  "VFIRST_M" {out=["rd"]};
  def ans  "C_SRLI_HINT" {out=[]};
  def ans  "MVVMATYPE" {out=["vd"]};
  def ans  "AES64DS" {out=["rd"]};
  def ans  "FENCEI_RESERVED" {out=[]};
  def ans  "C_ADDI" {out=["rsd"]};
  def ans  "VMSBF_M" {out=["vd"]};
  def ans  "VLOXSEGTYPE" {out=["vd"]};
  def ans  "VFMERGE" {out=["vd"]};
  def ans  "RIVVTYPE" {out=["vd"]};
  hacky ans "RISCV_SRAIW" "SHIFTIWOP" 3 {out=["rd"]};
  def ans  "MUL" {out=["rd"]};
  def ans  "C_ZEXT_W" {out=["rsd"]};
  def ans  "SFENCE_INVAL_IR" {out=[]};
  def ans  "RISCV_XPERM4" {out=["rd"]};
  def ans  "VMSOF_M" {out=["vd"]};
  def ans  "C_AND" {out=["rsd"]};
  def ans  "AES32DSI" {out=["rd"]};
  def ans  "RISCV_FLEQ_D" {out=["rd"]};
  def ans  "RISCV_JALR" {out=["rd"]};
  def ans  "RISCV_RORI" {out=["rd"]};
  hacky ans "AMOSWAP" "AMO" 0 {out=["rd"]};
  def ans  "VSSSEGTYPE" {out=[]};
  def ans  "VXTYPE" {out=["vd"]};
  def ans  "VMSIF_M" {out=["vd"]};
  hacky ans "FNMADD_S" "F_MADD_TYPE_S" 5 {out=["rd"]};
  def ans  "LOAD" {out=["rd"]};
  hacky ans "RISCV_ORI" "ITYPE" 3 {out=["rd"]};
  def ans  "VLSSEGTYPE" {out=["vd"]};
  def ans  "VIOTA_M" {out=["vd"]};
  def ans  "SHA512SIG1H" {out=["rd"]};
  def ans  "RISCV_CLMULR" {out=["rd"]};
  def ans  "RISCV_FLTQ_S" {out=["rd"]};
  hacky ans "RISCV_SLTIU" "ITYPE" 3 {out=["rd"]};
  def ans  "VXMSTYPE" {out=["vd"]};
  def ans  "RISCV_CLZ" {out=["rd"]};
  hacky ans "RISCV_MAX" "ZBB_RTYPE" 3 {out=["rd"]};
  def ans  "VXSG" {out=["vd"]};
  def ans  "RISCV_CLMULH" {out=["rd"]};
  def ans  "RISCV_FLI_H" {out=["rd"]};
  hacky ans "FMADD_S" "F_MADD_TYPE_S" 5 {out=["rd"]};
  def ans  "VXCMPTYPE" {out=["vd"]};
  def ans  "VSETVLI" {out=["rd"; "rd"]};
  def ans  "C_LHU" {out=["rd"]};
  def ans  "RISCV_JAL" {out=["rd"]};
  def ans  "C_SEXT_B" {out=["rsd"]};
  def ans  "ECALL" {out=[]};
  hacky ans "RISCV_XORI" "ITYPE" 3 {out=["rd"]};
  def ans  "VLUXSEGTYPE" {out=["vd"]};
  def ans  "SHA512SUM1R" {out=["rd"]};
  hacky ans "RISCV_BEXT" "ZBS_RTYPE" 3 {out=["rd"]};
  def ans  "C_FSWSP" {out=[]};
  hacky ans "RISCV_XOR" "RTYPE" 3 {out=["rd"]};
  def ans  "VITYPE" {out=["vd"]};
  def ans  "STORECON" {out=["rd"; "rd"; "rd"; "rd"]};
  hacky ans "RISCV_SLLIW" "SHIFTIWOP" 3 {out=["rd"]};
  def ans  "VMVRTYPE" {out=["vd"]};
  def ans  "VMVXS" {out=["rd"]};
  def ans  "VISG" {out=["vd"]};
  hacky ans "RISCV_ANDN" "ZBB_RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_SH2ADDUW" "ZBA_RTYPEUW" 3 {out=["rd"]};
  def ans  "C_FLD" {out=["rd"]};
  hacky ans "RISCV_SLLI" "SHIFTIOP" 3 {out=["rd"]};
  def ans  "RISCV_UNZIP" {out=[]};
  hacky ans "FADD_S" "F_BIN_RM_TYPE_S" 4 {out=["rd"]};
  hacky ans "RISCV_SLLW" "RTYPEW" 3 {out=["rd"]};
  def ans  "SHA512SIG1" {out=["rd"]};
  hacky ans "RISCV_BLTU" "BTYPE" 3 {out=[]};
  hacky ans "RISCV_SH1ADDUW" "ZBA_RTYPEUW" 3 {out=["rd"]};
  def ans  "NITYPE" {out=["vd"]};
  hacky ans "RISCV_BLT" "BTYPE" 3 {out=[]};
  def ans  "C_ADD" {out=["rsd"]};
  hacky ans "RISCV_LUI" "UTYPE" 2 {out=["rd"]};
  def ans  "FVFTYPE" {out=["vd"]};
  hacky ans "RISCV_SLL" "RTYPE" 3 {out=["rd"]};
  def ans  "WFI" {out=[]};
  def ans  "VVMTYPE" {out=["vd"]};
  def ans  "MVXMATYPE" {out=["vd"]};
  def ans  "RISCV_FLI_D" {out=["rd"]};
  def ans  "C_ADDI_HINT" {out=[]};
  hacky ans "RISCV_BNE" "BTYPE" 3 {out=[]};
  def ans  "MASKTYPEX" {out=["vd"]};
  hacky ans "RISCV_BINVI" "ZBS_IOP" 3 {out=["rd"]};
  hacky ans "RISCV_BCLRI" "ZBS_IOP" 3 {out=["rd"]};
  def ans  "RISCV_FROUNDNX_D" {out=["rd"]};
  def ans  "RISCV_FROUND_D" {out=["rd"]};
  hacky ans "FNMADD_D" "F_MADD_TYPE_D" 5 {out=["rd"]};
  def ans  "FENCE_RESERVED" {out=[]};
  def ans  "MASKTYPEI" {out=["vd"]};
  def ans  "VSETIVLI" {out=["rd"]};
  def ans  "FVVTYPE" {out=["vd"]};
  def ans  "RISCV_CPOPW" {out=["rd"]};
  def ans  "RISCV_FMAXM_D" {out=["rd"]};
  def ans  "C_SD" {out=[]};
  hacky ans "RISCV_ADDI" "ITYPE" 3 {out=["rd"]};
  def ans  "C_LI_HINT" {out=[]};
  hacky ans "RISCV_RORW" "ZBB_RTYPEW" 3 {out=["rd"]};
  hacky ans "FMUL_D" "F_BIN_RM_TYPE_D" 4 {out=["rd"]};
  hacky ans "RISCV_SRAW" "RTYPEW" 3 {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  def ans  "F_BIN_TYPE_H" {out=["rd"]};
  hacky ans "FMSUB_D" "F_MADD_TYPE_D" 5 {out=["rd"]};
  def ans  "SHA256SUM1" {out=["rd"]};
  def ans  "VSUXSEGTYPE" {out=[]};
  def ans  "ZBKB_PACKW" {out=["rd"]};
  hacky ans "RISCV_SH3ADDUW" "ZBA_RTYPEUW" 3 {out=["rd"]};
  def ans  "FVVMTYPE" {out=["vd"]};
  def ans  "VMVSX" {out=["vd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_D" {out=["rd"]};
  def ans  "RISCV_ORCB" {out=["rd"]};
  def ans  "VIMCTYPE" {out=["vd"]};
  hacky ans "RISCV_SUBW" "RTYPEW" 3 {out=["rd"]};
  def ans  "VIMSTYPE" {out=["vd"]};
  def ans  "MASKTYPEV" {out=["vd"]};
  def ans  "C_MUL" {out=["rd"]};
  hacky ans "AMOXOR" "AMO" 0 {out=["rd"]};
  def ans  "FVFMTYPE" {out=["vd"]};
  def ans  "SM3P1" {out=["rd"]};
  def ans  "RISCV_CLMUL" {out=["rd"]};
  def ans  "ADDIW" {out=["rd"]};
  def ans  "RISCV_FLEQ_S" {out=["rd"]};
  hacky ans "RISCV_SH3ADD" "ZBA_RTYPE" 3 {out=["rd"]};
  def ans  "MRET" {out=[]};
  def ans  "WVXTYPE" {out=["vd"]};
  def ans  "RISCV_FMAXM_S" {out=["rd"]};
  hacky ans "AMOMAXU" "AMO" 0 {out=["rd"]};
  def ans  "C_ILLEGAL" {out=[]};
  def ans  "VLSEGFFTYPE" {out=["vd"]};
  def ans  "C_ANDI" {out=["rsd"]};
  hacky ans "FMUL_H" "F_BIN_RM_TYPE_H" 4 {out=["rd"]};
  hacky ans "RISCV_SEXTB" "ZBB_EXTOP" 2 {out=["rd"]};
  def ans  "NXSTYPE" {out=["vd"]};
  hacky ans "RISCV_SH1ADD" "ZBA_RTYPE" 3 {out=["rd"]};
  def ans  "VSOXSEGTYPE" {out=[]};
  def ans  "WVTYPE" {out=["vd"]};
  def ans  "C_SDSP" {out=[]};
  def ans  "C_NOP" {out=[]};
  def ans  "VXMCTYPE" {out=["vd"]};
  def ans  "MMTYPE" {out=["vd"]};
  def ans  "C_SUBW" {out=["rsd"]};
  def ans  "VEXT4TYPE" {out=["vd"]};
  def ans  "VSETVL" {out=["rd"; "rd"]};
  def ans  "C_SH" {out=[]};
  hacky ans "FNMSUB_S" "F_MADD_TYPE_S" 5 {out=["rd"]};
  def ans  "MVVCOMPRESS" {out=["vd"]};
  def ans  "F_UN_TYPE_S" {out=["rd"]};
  def ans  "F_UN_TYPE_S" {out=["rd"]};
  def ans  "F_UN_TYPE_S" {out=["rd"]};
  hacky ans "RISCV_SLT" "RTYPE" 3 {out=["rd"]};
  def ans  "FWVVTYPE" {out=["vd"]};
  def ans  "RISCV_FROUND_H" {out=["rd"]};
  hacky ans "RISCV_ADD" "RTYPE" 3 {out=["rd"]};
  def ans  "C_JAL" {out=["ra"]};
  def ans  "SFENCE_VMA" {out=[]};
  def ans  "NVTYPE" {out=["vd"]};
  hacky ans "RISCV_ANDI" "ITYPE" 3 {out=["rd"]};
  def ans  "AES64KS2" {out=["rd"]};
  hacky ans "RISCV_SRA" "RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_BEQ" "BTYPE" 3 {out=[]};
  def ans  "NVSTYPE" {out=["vd"]};
  hacky ans "RISCV_MINU" "ZBB_RTYPE" 3 {out=["rd"]};
  hacky ans "FMUL_S" "F_BIN_RM_TYPE_S" 4 {out=["rd"]};
  hacky ans "RISCV_BINV" "ZBS_RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_AND" "RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_BGE" "BTYPE" 3 {out=[]};
  def ans  "RISCV_FROUND_S" {out=["rd"]};
  hacky ans "RISCV_MAXU" "ZBB_RTYPE" 3 {out=["rd"]};
  def ans  "NISTYPE" {out=["vd"]};
  def ans  "C_SLLI" {out=["rsd"]};
  def ans  "AES32ESMI" {out=["rd"]};
  def ans  "VXMTYPE" {out=["vd"]};
  hacky ans "RISCV_SRAI" "SHIFTIOP" 3 {out=["rd"]};
  hacky ans "FMADD_H" "F_MADD_TYPE_H" 5 {out=["rd"]};
  def ans  "FENCEI" {out=[]};
  def ans  "RISCV_FROUNDNX_H" {out=["rd"]};
  def ans  "VFMVSF" {out=["vd"]};
  def ans  "MOVETYPEI" {out=["vd"]};
  hacky ans "RISCV_OR" "RTYPE" 3 {out=["rd"]};
  def ans  "RISCV_FLTQ_H" {out=["rd"]};
  def ans  "VEXT8TYPE" {out=["vd"]};
  def ans  "C_OR" {out=["rsd"]};
  def ans  "C_LW" {out=["rd"]};
  def ans  "FWVFMATYPE" {out=["vd"]};
  def ans  "C_LWSP" {out=["rd"]};
  def ans  "C_ADDI16SP" {out=["sp"]};
  def ans  "SHA512SIG0L" {out=["rd"]};
  def ans  "SM3P0" {out=["rd"]};
  def ans  "SM4ED" {out=["rd"]};
  def ans  "RISCV_FMINM_D" {out=["rd"]};
  def ans  "AES64IM" {out=["rd"]};
  def ans  "DIVW" {out=["rd"]};
  hacky ans "RISCV_SLTU" "RTYPE" 3 {out=["rd"]};
  def ans  "VLRETYPE" {out=["vd"]};
  def ans  "C_ZEXT_B" {out=["rsd"]};
  def ans  "C_MV" {out=["rd"]};
  def ans  "VIMTYPE" {out=["vd"]};
  def ans  "VFMVFS" {out=["rd"; "rd"; "rd"]};
  def ans  "LOADRES" {out=["rd"]};
  hacky ans "RISCV_SRL" "RTYPE" 3 {out=["rd"]};
  def ans  "RISCV_CTZ" {out=["rd"]};
  def ans  "C_J" {out=["zreg"]};
  hacky ans "AMOMINU" "AMO" 0 {out=["rd"]};
  def ans  "AES32ESI" {out=["rd"]};
  def ans  "RISCV_FMVH_X_D" {out=["rd"]};
  def ans  "RISCV_SLLIUW" {out=["rd"]};
  def ans  "C_BEQZ" {out=[]};
  hacky ans "FDIV_H" "F_BIN_RM_TYPE_H" 4 {out=["rd"]};
  hacky ans "AMOMAX" "AMO" 0 {out=["rd"]};
  hacky ans "RISCV_SRLW" "RTYPEW" 3 {out=["rd"]};
  def ans  "SHA512SUM0" {out=["rd"]};
  def ans  "RISCV_FMINM_S" {out=["rd"]};
  def ans  "SHA512SUM0R" {out=["rd"]};
  def ans  "C_ADD_HINT" {out=[]};
  def ans  "REMW" {out=["rd"]};
  def ans  "VFMV" {out=["vd"]};
  def ans  "RISCV_ZIP" {out=[]};
  def ans  "C_SEXT_H" {out=["rsd"]};
  def ans  "SHA512SUM1" {out=["rd"]};
  hacky ans "FDIV_S" "F_BIN_RM_TYPE_S" 4 {out=["rd"]};
  hacky ans "CSRRW" "CSR" 4 {out=["rd"]};
  def ans  "WMVXTYPE" {out=["vd"]};
  hacky ans "FNMSUB_H" "F_MADD_TYPE_H" 5 {out=["rd"]};
  def ans  "C_LDSP" {out=["rd"]};
  hacky ans "RISCV_BGEU" "BTYPE" 3 {out=[]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_H" {out=["rd"]};
  def ans  "RISCV_CPOP" {out=["rd"]};
  def ans  "C_FSW" {out=[]};
  def ans  "C_SW" {out=[]};
  hacky ans "FADD_H" "F_BIN_RM_TYPE_H" 4 {out=["rd"]};
  def ans  "FWFTYPE" {out=["vd"]};
  hacky ans "FMADD_D" "F_MADD_TYPE_D" 5 {out=["rd"]};
  def ans  "C_SUB" {out=["rsd"]};
  def ans  "VFUNARY0" {out=["vd"]};
  def ans  "FWVTYPE" {out=["vd"]};
  hacky ans "FMSUB_S" "F_MADD_TYPE_S" 5 {out=["rd"]};
  def ans  "RISCV_FROUNDNX_S" {out=["rd"]};
  def ans  "SM4KS" {out=["rd"]};
  def ans  "RISCV_RORIW" {out=["rd"]};
  def ans  "SFENCE_W_INVAL" {out=[]};
  def ans  "C_JR" {out=["zreg"]};
  def ans  "NXTYPE" {out=["vd"]};
  def ans  "C_NOT" {out=["r"]};
  hacky ans "FNMSUB_D" "F_MADD_TYPE_D" 5 {out=["rd"]};
  def ans  "MVVTYPE" {out=["vd"]};
  def ans  "C_ADDIW" {out=["rsd"]};
  def ans  "C_LD" {out=["rd"]};
  def ans  "FVFMATYPE" {out=["vd"]};
  def ans  "RISCV_CTZW" {out=["rd"]};
  hacky ans "RISCV_SRLIW" "SHIFTIWOP" 3 {out=["rd"]};
  def ans  "RISCV_XPERM8" {out=["rd"]};
  def ans  "RISCV_FMAXM_H" {out=["rd"]};
  hacky ans "FSUB_H" "F_BIN_RM_TYPE_H" 4 {out=["rd"]};
  hacky ans "RISCV_ADDUW" "ZBA_RTYPEUW" 3 {out=["rd"]};
  def ans  "SHA256SUM0" {out=["rd"]};
  def ans  "C_XOR" {out=["rsd"]};
  def ans  "VID_V" {out=["vd"]};
  def ans  "FENCE" {out=[]};
  def ans  "C_FLWSP" {out=["rd"]};
  def ans  "STORE" {out=[]};
  def ans  "RISCV_FMINM_H" {out=["rd"]};
  def ans  "C_LUI_HINT" {out=[]};
  def ans  "VVMCTYPE" {out=["vd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "F_BIN_TYPE_S" {out=["rd"]};
  def ans  "VSSEGTYPE" {out=[]};
  def ans  "SINVAL_VMA" {out=[]};
  def ans  "MOVETYPEX" {out=["vd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "F_BIN_TYPE_D" {out=["rd"]};
  def ans  "ZICOND_RTYPE" {out=["rd"]};
  def ans  "ZICOND_RTYPE" {out=["rd"]};
  hacky ans "RISCV_ROL" "ZBB_RTYPE" 3 {out=["rd"]};
  hacky ans "AMOAND" "AMO" 0 {out=["rd"]};
  def ans  "C_FSDSP" {out=[]};
  def ans  "C_BNEZ" {out=[]};
  def ans  "SRET" {out=[]};
  def ans  "FWVVMATYPE" {out=["vd"]};
  def ans  "AES64KS1I" {out=["rd"]};
  def ans  "STORE_FP" {out=[]};
  def ans  "C_JALR" {out=["ra"]};
  def ans  "RMVVTYPE" {out=["vd"]};
  hacky ans "AMOOR" "AMO" 0 {out=["rd"]};
  def ans  "RISCV_CLZW" {out=["rd"]};
  def ans  "REM" {out=["rd"]};
  def ans  "C_EBREAK" {out=[]};
  def ans  "FENCE_TSO" {out=[]};
  hacky ans "RISCV_SLTI" "ITYPE" 3 {out=["rd"]};
  def ans  "SHA512SIG0" {out=["rd"]};
  def ans  "AES64ESM" {out=["rd"]};
  def ans  "RISCV_FLI_S" {out=["rd"]};
  def ans  "C_SB" {out=[]};
  def ans  "C_FLDSP" {out=["rd"]};
  def ans  "C_MV_HINT" {out=[]};
  def ans  "VFNUNARY0" {out=["vd"]};
  hacky ans "RISCV_AUIPC" "UTYPE" 2 {out=["rd"]};
  def ans  "VFWUNARY0" {out=["vd"]};
  def ans  "MOVETYPEV" {out=["vd"]};
  hacky ans "RISCV_SEXTH" "ZBB_EXTOP" 2 {out=["rd"]};
  hacky ans "RISCV_SH2ADD" "ZBA_RTYPE" 3 {out=["rd"]};
  def ans  "VFUNARY1" {out=["vd"]};
  def ans  "FWVFTYPE" {out=["vd"]};
  def ans  "RISCV_FCVTMOD_W_D" {out=["rd"]};
  def ans  "C_SRLI" {out=["rsd"]};
  hacky ans "RISCV_BSET" "ZBS_RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_ADDW" "RTYPEW" 3 {out=["rd"]};
  def ans  "RFVVTYPE" {out=["vd"; "vd"]};
  def ans  "VSRETYPE" {out=[]};
  hacky ans "RISCV_ORN" "ZBB_RTYPE" 3 {out=["rd"]};
  def ans  "C_SLLI_HINT" {out=[]};
  def ans  "SHA512SIG0H" {out=["rd"]};
  def ans  "WVVTYPE" {out=["vd"]};
  def ans  "LOAD_FP" {out=["rd"; "rd"; "rd"]};
  def ans  "VVMSTYPE" {out=["vd"]};
  def ans  "FVVMATYPE" {out=["vd"]};
  def ans  "AES64DSM" {out=["rd"]};
  hacky ans "FDIV_D" "F_BIN_RM_TYPE_D" 4 {out=["rd"]};
  def ans  "VEXT2TYPE" {out=["vd"]};
  def ans  "URET" {out=[]};
  def ans  "C_LI" {out=["rd"]};
  def ans  "EBREAK" {out=[]};
  hacky ans "FADD_D" "F_BIN_RM_TYPE_D" 4 {out=["rd"]};
  def ans  "C_SRAI" {out=["rsd"]};
  hacky ans "RISCV_SRLI" "SHIFTIOP" 3 {out=["rd"]};
  def ans  "C_LUI" {out=["rd"]};
  def ans  "RISCV_FMVP_D_X" {out=["rd"]};
  def ans  "C_LBU" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  def ans  "F_UN_RM_TYPE_S" {out=["rd"]};
  hacky ans "RISCV_ZEXTH" "ZBB_EXTOP" 2 {out=["rd"]};
  hacky ans "RISCV_XNOR" "ZBB_RTYPE" 3 {out=["rd"]};
  hacky ans "RISCV_MIN" "ZBB_RTYPE" 3 {out=["rd"]};
  def ans  "WMVVTYPE" {out=["vd"]};
  def ans  "C_ZEXT_H" {out=["rsd"]};
  def ans  "MULW" {out=["rd"]};
  def ans  "SHA512SIG1L" {out=["rd"]};
  def ans  "VVCMPTYPE" {out=["vd"]};
  def ans  "VLSEGTYPE" {out=["vd"]};
  def ans  "ILLEGAL" {out=[]};
  def ans  "SHA256SIG0" {out=["rd"]};
  hacky ans "FSUB_D" "F_BIN_RM_TYPE_D" 4 {out=["rd"]};
  def ans  "RISCV_BREV8" {out=["rd"]};
  hacky ans "RISCV_PACKH" "ZBKB_RTYPE" 3 {out=["rd"]};
  def ans  "AES32DSMI" {out=["rd"]};
  def ans  "F_UN_TYPE_H" {out=["rd"]};
  def ans  "F_UN_TYPE_H" {out=["rd"]};
  def ans  "F_UN_TYPE_H" {out=["rd"]};
  hacky ans "RISCV_SUB" "RTYPE" 3 {out=["rd"]};
  def ans  "C_FSD" {out=[]};
  hacky ans "FSUB_S" "F_BIN_RM_TYPE_S" 4 {out=["rd"]};
  def ans  "C_ADDI4SPN" {out=["rd"]};
  def ans  "C_ADDW" {out=["rsd"]};
  hacky ans "RISCV_BEXTI" "ZBS_IOP" 3 {out=["rd"]};
  def ans  "VVTYPE" {out=["vd"]};
  hacky ans "FNMADD_H" "F_MADD_TYPE_H" 5 {out=["rd"]};
  hacky ans "RISCV_BSETI" "ZBS_IOP" 3 {out=["rd"]};
  def ans  "VCPOP_M" {out=["rd"]};
  hacky ans "FMSUB_H" "F_MADD_TYPE_H" 5 {out=["rd"]};
  hacky ans "AMOADD" "AMO" 0 {out=["rd"]};
  def ans  "SHA256SIG1" {out=["rd"]};
  def ans  "RISCV_FLEQ_H" {out=["rd"]};
  hacky ans "RISCV_ROR" "ZBB_RTYPE" 3 {out=["rd"]};
  def ans  "VICMPTYPE" {out=["vd"]};
  def ans  "MVXTYPE" {out=["vd"]};
  def ans  "C_FLW" {out=["rd"]};
  def ans  "C_SWSP" {out=[]};
  def ans  "RISCV_FLTQ_D" {out=["rd"]};
  hacky ans "AMOMIN" "AMO" 0 {out=["rd"]};
  def ans  "AES64ES" {out=["rd"]};
  def ans  "C_SRAI_HINT" {out=[]};
  def ans  "DIV" {out=["rd"]};
  ans
   let lookup_exn = Hash_info.find from6159 let mem = Hash_info.mem from6159 
let from6159_hacky = [ "VMTYPE"; ]

