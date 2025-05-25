[![Build and Test Master in docker](https://github.com/Kakadu/llvm_vs_sail/actions/workflows/master.yml/badge.svg)](https://github.com/Kakadu/llvm_vs_sail/actions/workflows/master.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Coverage Status](https://coveralls.io/repos/github/s-khechnev/llvm_td_lint_playground/badge.svg?branch=master)](https://coveralls.io/github/s-khechnev/llvm_td_lint_playground?branch=master)

# LLVM vs Sail


## Overview

This project is a static analyzer designed to compare RISC-V ISA specification in [LLVM](https://github.com/llvm/llvm-project) and [sail-riscv](https://github.com/riscv/sail-riscv).

### The analyzer compares
1. inputs/outs operands of instructions
2. mayLoad/mayStore

### Getting started
1. Required packages
```
libgmp-dev zlib1g-dev z3 jq
```

2. Dependencies
```
opam pin add ./ -n
opam install . --deps-only --with-test --with-doc
```

3. Build and test
```
dune build
dune runtest
```
This will generate a report in [report](src/report) containing all the properties of the matched instructions.

4. Test coverage
```
make coverage
xdg-open _coverage/index.html
```
