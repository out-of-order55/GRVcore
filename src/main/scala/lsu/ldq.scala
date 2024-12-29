package grvcore

/* 
LDQ流水线
stage0
计算地址，写入LDQ，向DCache发送访存请求
stage1
DCache对比tag，地址送到STQ和store buffer，得到数据
stage2
DCache返回命中信息，如果命中，将从STQ store buffer 和DCache选择数据（STQ优先级最高），写回数据
如果发生miss，会将miss信息送入issue unit，重新发送数据，DCache分配MSHR，LDQ监听数据


1.在dispatch写入
2.当load指令得到hit信息，更新LDQ，当指令提交时，释放表项
3.如果load指令发生miss，需要进行replay（不需要冲刷流水线：因为他miss，不能对其issue unit的指令进行wake up）
miss期间，LDQ需要侦听DCache的refill数据，如果数据到来，写入LDQ，然后准备写入rf，如果发生端口冲突，阻塞LDQ

LDQ需要存：
1.地址
2.数据
3.状态：
idle：初始状态
valid：disptach写入
miss：当发生cache_miss

需要设置的端口
1.来自dispatch的端口
2.来自LD流水线的端口
3.来自commit的端口
4.来自DCache的refill数据端口
5.写回端口


可能发生的异常：
1.bank冲突：进行replay
2.sw/ld违例：进行redirect

 */