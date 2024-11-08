.
| with_entries
( select(.value | type == "object")
| select(.value."Pseudo" == null)
| select(.value."!superclasses" != null)
| select(.value."!superclasses"[] | contains("Instruction") )
| select(.value."!superclasses"[] | contains("Pseudo") | not)
| select(.value."!superclasses"[] | contains("StandardPseudoInstruction") | not)
| select(.value | del(.TSFlags) )
)
# a comment
| with_entries(
    select(.value | type == "object" )
    | del(.value.SoftFail, .value.TSFlags, .value.SchedRW, .value.Inst) )
| with_entries(
    select(.value | type == "object") 
    | select(.value."AsmString" != null)
    | select(.value."AsmString" != "")
    | .key = (.value."AsmString" | split("\t") | .[0])
)
| .
