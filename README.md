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

2024/12/12：添加简易的br_test

2024/12/13:继续添加br_test,对ubtb，bim，btb和RAS测试，尽量找出性能bug，最终在简易的add程序实现93%的预测正确率

2024/12/14:添加FTQ，并且编写FTQ测试，以及前端简易测试

2024/12/15：添加IQueue，通过读写测试

# Problem

1. /home/gg/GRVCore//include/init.h:7:10: fatal error: Vtop__Dpi.h: No such file or directory: no verilog files use DPIC(must has at least one file uses DPIC)
2. Vtop_ALL.cpp:(.text+0x71): undefined reference to `ifu': add int in verilog file->void ifu(input int cnt)
