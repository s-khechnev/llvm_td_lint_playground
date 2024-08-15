# llvm_td_lint_playground


````
function jsonify_tblgen {
        (cd /home/kakadu/work/llvm/llvm-19-build && \
        ./bin/llvm-tblgen  -I ../llvm-git/llvm/lib/Target/$1 -I../build-llvm18/include -I../llvm-git/llvm/include -I ../llvm-git/llvm/lib/Target ../llvm-git/llvm/lib/Target/$1/$1.td --dump-json | python -m json.tool  > $2)
}

````