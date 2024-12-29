package grvcore
/* 
STQ需要的内容：
1.地址
2.数据
3.状态
4.数据mask
已经commit的数据会被写入store buffer

Stage 0
计算地址|写入STQ数据
Stage 1
开始进行访存依赖检查，如果发生sw/ld违例，需要通知ROB，然后重新从这个load指令
执行
Stage 2
完成访存依赖检查
通知 ROB 可以提交指令



 */
