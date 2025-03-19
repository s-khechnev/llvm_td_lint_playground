# filter llvm td json
.
| with_entries
( select(.value | type == "object")
| select(.value."Pseudo" == null)
| select(.value."!anonymous" != true)
| select(any(.value."!superclasses"[]; . != "Instruction"))
| select(all(.value."!superclasses"[]; . != "Pseudo" and . != "StandardPseudoInstruction"))
| select(.value."isCodeGenOnly" != 1)
| select(.value."AsmString" != null)
| select(.value."AsmString" != "")
| .key = (.value."AsmString" | split("\t") | .[0])
| del(.value.SoftFail, .value.TSFlags, .value.SchedRW, .value.Inst)
)
