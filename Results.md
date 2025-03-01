### Расхождения

#### SP не является явным аргументом в SAIL для инструкций:

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

#### Zreg является неявным in регистром в SAIL для инструкций:

1. [c.beqz](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-beqz) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L498), [LLVM2](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L263), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L389)

2. [c.bnez](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-bnez) &mdash; [LLVM1](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L499), [LLVM2](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L263), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L401)

3. [c.li](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-li) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L435), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L178)

4. [c.mv](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-mv) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L549), [sail](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L533)

#### Zreg является неявным out регистром в SAIL для инструкций:

1. [c.j](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-j) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L491), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L377), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_base.sail#L61)

2. [c.jr](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jr) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L540), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L501), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_jalr_rmem.sail#L11)

#### RA является неявным out регистром в SAIL для инструкций:

1. [c.jal](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jal) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L422), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L145), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_base.sail#L61)

2. [c.jalr](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-jalr) &mdash; [LLVM](https://github.com/llvm/llvm-project/blob/b65e0947cade9bd39036a7700b54c1df4ec00756/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L558), [sail1](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_insts_zca.sail#L517), [sail2](https://github.com/riscv/sail-riscv/blob/2dfc4ff9f2bed3dcd0a3e8748211c99099e70ab7/model/riscv_jalr_rmem.sail#L11)
