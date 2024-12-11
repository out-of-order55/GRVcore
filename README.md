# Structure

GRVCore
├── build
├── include
├── src
│   ├── main
│   ├── test
│   │   ├── vcs
│   │   └── verilator
│   └── vsrc
└── vsrc
    └── vcs

# TODO

* [X] refactor code and Makefile
* [X] create chisel env
* [X] add DPIC test
* [X] add wave test
* [ ] add vcs test
* [ ] add verdi test
* [ ] add dc test

# 开发记录

之前的看commit

2024/12/11：今天决定把跨行取值的特性取消了，采用mask表示指令有效性，ICache主体逻辑减少一半

# Problem

1. /home/gg/GRVCore//include/init.h:7:10: fatal error: Vtop__Dpi.h: No such file or directory: no verilog files use DPIC(must has at least one file uses DPIC)
2. Vtop_ALL.cpp:(.text+0x71): undefined reference to `ifu': add int in verilog file->void ifu(input int cnt)
