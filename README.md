# llvm_td_lint_playground


````
function jsonify_tblgen {
        (cd /home/kakadu/work/llvm/llvm-19-build && \
        ./bin/llvm-tblgen  -I ../llvm-git/llvm/lib/Target/$1 -I../build-llvm18/include -I../llvm-git/llvm/include -I ../llvm-git/llvm/lib/Target ../llvm-git/llvm/lib/Target/$1/$1.td --dump-json | python -m json.tool  > $2)
}

````


### вопросики

#### [c.addi16sp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-addi16sp)

В [LLVM18](https://github.com/llvm/llvm-project/blob/release/18.x/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L435) у него мнемоника "c.addi16sp\t$rd, $imm", а на деле только 1 аргумент.
Наверное, нужно как-то понимать, что если там in/OUT агументы, то они не обязательно передаются явно, а могут быть глобальные эффекты