[![Build and Test Master in docker](https://github.com/Kakadu/llvm_vs_sail/actions/workflows/master.yml/badge.svg)](https://github.com/Kakadu/llvm_vs_sail/actions/workflows/master.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Coverage Status](https://coveralls.io/repos/github/Kakadu/llvm_vs_sail/badge.svg?branch=p_ext)](https://coveralls.io/github/Kakadu/llvm_vs_sail?branch=p_ext)

# LLVM vs Sail


## Overview

This project is a static analyzer that compares RISC-V ISA specifications in [LLVM](https://github.com/llvm/llvm-project) and [sail-riscv](https://github.com/riscv/sail-riscv).

### The static analyzer can compare:
1. inputs/outputs operands of instructions
2. [mayLoad](https://github.com/llvm/llvm-project/blob/24bd4e59b9e2901f8797484e7a231178d91807aa/llvm/include/llvm/Target/Target.td#L647)/[mayStore](https://github.com/llvm/llvm-project/blob/24bd4e59b9e2901f8797484e7a231178d91807aa/llvm/include/llvm/Target/Target.td#L648)

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

3. Building
```
dune build
```
This will generate a `src/report.txt` file containing all the properties of the matched instructions.

Reports are available for:  
- **LLVM v19** and [sail-riscv](https://github.com/riscv/sail-riscv/tree/05b845c91d1c1db7b361fc8d06e815b54ca0b07a): [view here](https://kakadu.github.io/llvm_vs_sail/master/report.txt)
- **P-extension (v0.9.11-draft-20211209)** in LLVM and [sail-riscv-p](https://github.com/umcann123/sail-riscv-p): [view here](https://kakadu.github.io/llvm_vs_sail/p_ext/report.txt)

4. Running tests
```
dune runtest
```

5. Test coverage
```
make coverage
xdg-open _coverage/index.html
```
