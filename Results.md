LLVM - Version 0.9.11-draft-20211209

[sail-riscv-p](https://github.com/umcann123/sail-riscv-p)

## Расхождения

### В Sail пишут в `vxsat`:
kabs16 kabs32 kabs8 kabsw kadd16 kadd32 kadd64 kadd8 kaddh kaddw kcras16 kcras32 kcrsa16 kcrsa32 kdmabb kdmabb16 kdmabt kdmabt16 kdmatt kdmatt16 kdmbb kdmbb16 kdmbt kdmbt16 kdmtt kdmtt16 khm16 khm8 khmbb khmbb16 khmbt khmbt16 khmtt khmtt16 khmx16 khmx8 kmabb kmabb32 kmabt kmabt32 kmada kmadrs kmadrs32 kmads kmads32 kmar64 kmatt kmatt32 kmaxda kmaxda32 kmaxds kmaxds32 kmda kmda32 kmmac kmmac.u kmmawb kmmawb2 kmmawb2.u kmmawb.u kmmawt kmmawt2 kmmawt2.u kmmawt.u kmmsb kmmsb.u kmmwb2 kmmwb2.u kmmwt2 kmmwt2.u kmsda kmsda32 kmsr64 kmsxda kmsxda32 kmxda kmxda32 ksll16 ksll32 ksll8 kslli16 kslli32 kslli8 kslliw ksllw kslra16 kslra16.u kslra32 kslra32.u kslra8 kslra8.u kslraw kslraw.u kstas16 kstas32 kstsa16 kstsa32 ksub16 ksub32 ksub64 ksub8 ksubh ksubw kwmmul kwmmul.u sclip16 sclip32 sclip8 uclip16 uclip32 uclip8 ukadd16 ukadd32 ukadd64 ukadd8 ukaddh ukaddw ukcras16 ukcras32 ukcrsa16 ukcrsa32 ukmar64 ukmsr64 ukstas16 ukstas32 ukstsa16 ukstsa32 uksub16 uksub32 uksub64 uksub8 uksubh uksubw

### В LLVM читают `vxsat`:
kdmabb kdmabb16 kdmabt kdmabt16 kdmatt kdmatt16 kdmbb kdmbb16 kdmbt kdmbt16 kdmtt kdmtt16 khm16 khm8 khmbb khmbb16 khmbt khmbt16 khmtt khmtt16 khmx16 khmx8
