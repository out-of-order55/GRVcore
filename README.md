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
* [X] add vcs test
* [X] add verdi test
* [ ] add dc test

# 开发记录

之前的看commit

2024/12/11：今天决定把跨行取值的特性取消了，采用mask表示指令有效性，ICache主体逻辑减少一半

2024/12/12：添加简易的br_test

2024/12/13：继续添加br_test,对ubtb，bim，btb和RAS测试，尽量找出性能bug，最终在简易的add程序实现93%的预测正确率

2024/12/14：添加FTQ，并且编写FTQ测试，以及前端简易测试

2024/12/15：添加IQueue，通过读写测试

2024/12/16：前端完成连续指令流测试，可以稳定的取出指令并且送入后端，但由于冲刷前端的一些异常不好设置激励，所以会在译码模块或者执行模块做完联调，译码模块是改造的boom的decode

2024/12/17：完成译码，对gtkwave进行配置，并且加入了translate file（针对inst，uop等信号),完成rename，不过RAT写的一坨，需要优化逻辑

2024/12/18：完成lrename测试，在C端写了软件的rename，通过difftest来对比重命名错误，目前软件端的重命名不支持配置,对RAT进行优化，将面积从13000降为9100

2024/12/21：完成ROB代码编写，前两天firesim运行有问题（目前还没调通)

2024/12/22：调试firesim，准备一个命令一个命令的试，今天issue开了个头

2024/12/23：写完了issue，然后搭建了测试框架，目前解决了单个entry的uop会重复发送到不同的EXU

# Problem

1. /home/gg/GRVCore//include/init.h:7:10: fatal error: Vtop__Dpi.h: No such file or directory: no verilog files use DPIC(must has at least one file uses DPIC)
2. Vtop_ALL.cpp:(.text+0x71): undefined reference to `ifu': add int in verilog file->void ifu(input int cnt)
