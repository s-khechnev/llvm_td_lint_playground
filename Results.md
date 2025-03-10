## Расхождения

### SP не является явным аргументом в SAIL для инструкций:

1. [c.addi16sp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-addi16sp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/5ddf40fa78705384966c22da78e12134df7bd723/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L440), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L201)

2. [c.addi4spn](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-addi4spn) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L303), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L43)

3. [c.fldsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-fldsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L740), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zcd.sail#L23)

4. [c.flwsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-flwsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L733), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zcf.sail#L32)

5. [c.fsdsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-fsdsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L741), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zcd.sail#L39)

6. [c.fswsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-fswsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L734), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zcf.sail#L48)

7. [c.ldsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-ldsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/bc35510725e5d55f7798cc6eb3be7e5f19c38d59/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L726), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L455)

8. [c.lwsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-lwsp) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b356a3085be43fda14a9f34f9e81bdf36b73e915/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L515), [LLVM2](https://github.com/llvm/llvm-project/blob/b356a3085be43fda14a9f34f9e81bdf36b73e915/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L241), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L437)

9. [c.sdsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-sdsp) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b356a3085be43fda14a9f34f9e81bdf36b73e915/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L727), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L489)

10. [c.swsp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-swsp) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b356a3085be43fda14a9f34f9e81bdf36b73e915/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L575), [LLVM2](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L247), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L472)

### Zreg является неявным in регистром в SAIL для инструкций:

1. [c.beqz](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-beqz) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L498), [LLVM2](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L263), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L389)

2. [c.bnez](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-bnez) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L499), [LLVM2](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L263), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L401)

3. [c.li](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-li) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L435), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L178)

4. [c.mv](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-mv) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L549), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L533)

### Zreg является неявным out регистром в SAIL для инструкций:

1. [c.j](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-j) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L491), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L377), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_base.sail#L61)

2. [c.jr](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jr) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L540), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L501), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_jalr_rmem.sail#L11)

### RA является неявным out регистром в SAIL для инструкций:

1. [c.jal](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jal) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L422), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L145), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_base.sail#L61)

2. [c.jalr](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jalr) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L558), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L517), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_jalr_rmem.sail#L11)

### RVV 

#### В Sail vd - in регистр:

vadc.vim vadc.vvm vadc.vxm vcompress.vm vfmerge.vfm vmadc.vi vmadc.vim vmadc.vv vmadc.vvm vmadc.vx vmadc.vxm vmand.mm vmandn.mm vmerge.vim vmerge.vvm vmerge.vxm vmnand.mm vmnor.mm vmor.mm vmorn.mm vmsbc.vv vmsbc.vvm vmsbc.vx vmsbc.vxm vmxnor.mm vmxor.mm vsbc.vvm vsbc.vxm

#### В Sail vd и (неявно) v0 - in регистры

vaaddu.vv vaaddu.vx vaadd.vv vaadd.vx vadd.vi vadd.vv vadd.vx vand.vi vand.vv vand.vx vasubu.vv vasubu.vx vasub.vv vasub.vx vdivu.vv vdivu.vx vdiv.vv vdiv.vx vfadd.vf vfadd.vv vfclass.v vfcvt.f.xu.v vfcvt.f.x.v vfcvt.rtz.x.f.v vfcvt.rtz.xu.f.v vfcvt.x.f.v vfcvt.xu.f.v vfdiv.vf vfdiv.vv vfirst.m vfmacc.vf vfmacc.vv vfmadd.vf vfmadd.vv vfmax.vf vfmax.vv vfmin.vf vfmin.vv vfmsac.vf vfmsac.vv vfmsub.vf vfmsub.vv vfmul.vf vfmul.vv vfmv.s.f vfmv.v.f vfncvt.f.f.w vfncvt.f.xu.w vfncvt.f.x.w vfncvt.rod.f.f.w vfncvt.rtz.x.f.w vfncvt.rtz.xu.f.w vfncvt.x.f.w vfncvt.xu.f.w vfnmacc.vf vfnmacc.vv vfnmadd.vf vfnmadd.vv vfnmsac.vf vfnmsac.vv vfnmsub.vf vfnmsub.vv vfrdiv.vf vfrec7.v vfrsqrt7.v vfrsub.vf vfsgnjn.vf vfsgnjn.vv vfsgnj.vf vfsgnj.vv vfsgnjx.vf vfsgnjx.vv vfslide1down.vf vfslide1up.vf vfsqrt.v vfsub.vf vfsub.vv vfwadd.vf vfwadd.vv vfwadd.wf vfwadd.wv vfwcvt.f.f.v vfwcvt.f.xu.v vfwcvt.f.x.v vfwcvt.rtz.x.f.v vfwcvt.rtz.xu.f.v vfwcvt.x.f.v vfwcvt.xu.f.v vfwmacc.vf vfwmacc.vv vfwmsac.vf vfwmsac.vv vfwmul.vf vfwmul.vv vfwnmacc.vf vfwnmacc.vv vfwnmsac.vf vfwnmsac.vv vfwsub.vf vfwsub.vv vfwsub.wf vfwsub.wv vid.v viota.m vmacc.vv vmacc.vx vmadd.vv vmadd.vx vmaxu.vv vmaxu.vx vmax.vv vmax.vx vmfeq.vf vmfeq.vv vmfge.vf vmfgt.vf vmfle.vf vmfle.vv vmflt.vf vmflt.vv vmfne.vf vmfne.vv vminu.vv vminu.vx vmin.vv vmin.vx vmsbf.m vmseq.vi vmseq.vv vmseq.vx vmsgtu.vi vmsgtu.vx vmsgt.vi vmsgt.vx vmsif.m vmsleu.vi vmsleu.vv vmsleu.vx vmsle.vi vmsle.vv vmsle.vx vmsltu.vv vmsltu.vx vmslt.vv vmslt.vx vmsne.vi vmsne.vv vmsne.vx vmsof.m vmulhsu.vv vmulhsu.vx vmulhu.vv vmulhu.vx vmulh.vv vmulh.vx vmul.vv vmul.vx vmv1r.v vmv2r.v vmv4r.v vmv8r.v vmv.s.x vmv.v.i vmv.v.v vmv.v.x vnclipu.wi vnclipu.wv vnclipu.wx vnclip.wi vnclip.wv vnclip.wx vnmsac.vv vnmsac.vx vnmsub.vv vnmsub.vx vnsra.wi vnsra.wv vnsra.wx vnsrl.wi vnsrl.wv vnsrl.wx vor.vi vor.vv vor.vx vredand.vs vredmaxu.vs vredmax.vs vredminu.vs vredmin.vs vredor.vs vredsum.vs vredxor.vs vremu.vv vremu.vx vrem.vv vrem.vx vrgatherei16.vv vrgather.vi vrgather.vv vrgather.vx vrsub.vi vrsub.vx vsaddu.vi vsaddu.vv vsaddu.vx vsadd.vi vsadd.vv vsadd.vx vsext.vf2 vsext.vf4 vsext.vf8 vslide1down.vx vslide1up.vx vslidedown.vi vslidedown.vx vslideup.vi vslideup.vx vsll.vi vsll.vv vsll.vx vsmul.vv vsmul.vx vsra.vi vsra.vv vsra.vx vsrl.vi vsrl.vv vsrl.vx vssra.vi vssra.vv vssra.vx vssrl.vi vssrl.vv vssrl.vx vssubu.vv vssubu.vx vssub.vv vssub.vx vsub.vv vsub.vx vwaddu.vv vwaddu.vx vwaddu.wv vwaddu.wx vwadd.vv vwadd.vx vwadd.wv vwadd.wx vwmaccsu.vv vwmaccsu.vx vwmaccus.vx vwmaccu.vv vwmaccu.vx vwmacc.vv vwmacc.vx vwmulsu.vv vwmulsu.vx vwmulu.vv vwmulu.vx vwmul.vv vwmul.vx vwredsumu.vs vwredsum.vs vwsubu.vv vwsubu.vx vwsubu.wv vwsubu.wx vwsub.vv vwsub.vx vwsub.wv vwsub.wx vxor.vi vxor.vv vxor.vx vzext.vf2 vzext.vf4 vzext.vf8
