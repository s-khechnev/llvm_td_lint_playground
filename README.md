# llvm_td_lint_playground





### TODO

1) Нужно написать анализ алиасов, иначе детекция выходных/выходных нормально работать не будет. (пример -- исполнение F_BIN_RM_TYPE_H в SAIL)
2) Некоторые out регистры (SP) не являются явными аргументами в SAIL. (Пример --- [c.addi16sp](https://msyksphinz-self.github.io/riscv-isadoc/html/rvc.html#c-addi16sp) в [SAIL](https://github.com/riscv/sail-riscv/blob/05b845c91d1c1db7b361fc8d06e815b54ca0b07a/model/riscv_insts_zca.sail#L196) и [LLVM](https://github.com/llvm/llvm-project/blob/release/18.x/llvm/lib/Target/RISCV/RISCVInstrInfoC.td#L435)). Если это только c SP такое будет -- можно накостылять, что-нибудь, но в общем случае мне не очень понятно.
3) В SAIL не нашел реализаций некоторых инструкций: amoadd.d, amocas, divu (неожиданно), cbo*, [XCV](https://github.com/llvm/llvm-project/blob/release/18.x/llvm/lib/Target/RISCV/RISCVInstrInfoXCV.td) (что-то вендор-специфичное?).

    Это не полный список. Поделка пока жевала инструкции по алфавиту только от A до F.
4) Могут возникнуть некоторые проблемы, если в LLVM и SAIL названия аргументов не соответствуют друг другу.

    Явного расхождения пока не найдено, есть нечто похожее, когда в дампе LLVM отдельно разные in и out аргументы, с констрейнтом `$rs1 = $rs1_wb`. Всегда ли они будут соблюдать такие же имена как в SAIL? Можно ли считать, что искать в SAIL стоит левый операнд (здесь `$rs1`) в общем случае --- должно показать будущее.

### misc

````
function jsonify_tblgen {
        (cd /home/kakadu/work/llvm/llvm-19-build && \
        ./bin/llvm-tblgen  -I ../llvm-git/llvm/lib/Target/$1 -I../build-llvm18/include -I../llvm-git/llvm/include -I ../llvm-git/llvm/lib/Target ../llvm-git/llvm/lib/Target/$1/$1.td --dump-json | python -m json.tool  > $2)
}

````