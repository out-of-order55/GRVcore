package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
/* 
ROB实现的功能：
1.接受来自dispatch给出的寄存器旧映射关系，以及入队的Microop，然后返回ROB_idx

2.提交时更新处理器状态，通知FTQ，更新RAS，RAT，（ghist）
3.如果在执行发生异常，会发出锁存信号，并且，之后的指令执行实际无效，然后当ROB等到指令变为最旧的指令，将流水线冲刷，从该指令开始取指令
3.目前采用的是boom的提交方案，也就是只有一个corewidth所有的指令都执行完毕了，这时候才可以对指令进行提交，还有就是香山或者玄铁的方案，每个周期退休尽量多的指令
这时候一种设计方法就是FTQ和指令记录mask数值，提交时对比值，只有mask全部为1 FTQ才可以退队，注意这里不包含无效指令，
 */
class CommitMsg(implicit p: Parameters) extends GRVBundle{
    val valid       = Vec(coreWidth,Bool())
    val commit_uops = Vec(coreWidth,new MicroOp)
}
/* 
 */
class CommitExcMsg(implicit p: Parameters) extends GRVBundle{

    val ftq_idx    = UInt(log2Ceil(ftqentries).W)
    val pc_lob     = UInt(log2Ceil(blockBytes).W)
    val epc        = UInt(XLEN.W)//for mispred
    val flush_typ  = UInt(FLUSH_SZ.W)
}
class robEnqueue (implicit p: Parameters) extends GRVBundle{
    val uops  = Vec(coreWidth,new MicroOp)
    val valid = Vec(coreWidth,Bool())
}
class ROBIO(implicit p: Parameters) extends GRVBundle{

    val enq         = Flipped(Decoupled(new robEnqueue))
    val wb_resp     = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, DecoupledIO(new ExuResp))))

    val commit      = Valid(new CommitMsg)
    val flush       = Valid(new CommitExcMsg)
}
/*

*/
class ROB(implicit p: Parameters) extends GRVModule{
    val io = IO(new ROBIO)
    /* 
    ROB状态机 
    idle    ：rob初始状态， 开机之后如果不reset就不会进入，
    normal  ：rob正常状态
    redirect：rob进入重定向状态，根据输入的信号转换：主要有：
    来自EXU阶段的JMP UNIT，来自前端的异常信息，比如无效指令，非对齐等，以及执行阶段的load/store违例
     */
    val s_idle::s_normal::s_redirect = enum(3)
    val rob_state = RegInit(s_idle)

    val rob_enq_ptr = RegInit(0.U(log2Ceil(ROBEntry+1)))
    val rob_deq_ptr = RegInit(0.U(log2Ceil(ROBEntry+1)))
    val full = Wire(Bool())
    
    val do_enq = Wire(Bool())
    val do_deq = Wire(Bool())

    full := RegNext(rob_enq_ptr+1.U===rob_deq_ptr)//没有考虑到入队的同时出队

    do_enq := io.enq.fire
    

}