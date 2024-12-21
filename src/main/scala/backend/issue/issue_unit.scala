package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
case class IssueParams(
    dispatchWidth: Int = 1,
    issueWidth: Int = 1,
    numEntries: Int = 8,
    iqType: BigInt
)
/*

对于issue：
1.来自dispatch的uop
2.来自执行的唤醒:需要执行阶段的pdst和valid(亦可以是发射阶段，取决于指令的类型，比如单周期指令，本来就可以在发射时唤醒，但load指令执行周期不定，需要另外考虑)
3.冲刷信号
需要执行的功能有：
入队
唤醒
仲裁

entry根据信息来进行唤醒操作，假如一个entry的源寄存器全部准备好了：
1.如果单周期，那么此时已经可以发送请求了
2.如果多周期，得等到delay才可以发送请求
 */
class WakeupPort(implicit p: Parameters) extends GRVBundle{
    val valid = Bool()
    val delay = UInt(Delay_Sz.W)
    val pdst  = UInt(pregSz.W)
}
class IssueEntryIO(val numWakeupPorts: Int)(implicit p: Parameters) extends GRVBundle{

    val valid   = Input(Bool())
    val flush   = Input(Bool())
    //ready
    val req     = Output(Bool())
    //select
    val grant   = Input(Bool())
    //from dispatch
    val dis_uop = Flipped(Valid(new MicroOp))
    //wake up
    val wakeup  = Input(Vec(numWakeupPorts,new WakeupPort))
    //to exu
    val ex_uop    = Output(new MicroOp)
}
class IssueEntry(val numWakeupPorts: Int)(implicit p: Parameters) extends  GRVModule{
    val io = IO(new IssueEntryIO(numWakeupPorts))

    val entry = RegInit(0.U.asTypeOf(new MicroOp))
    
    //enq 
    //当有入队的valid信号，表示一定为空或者有要出队的
    when(io.dis_uop.valid){
        entry := io.dis_uop.bits

        rs1_bsy := io.dis_uop.bits.prs1_busy
        rs2_bsy := io.dis_uop.bits.prs2_busy
    }
    /* 
    wake up
    问题：如何去调度每个wakeup端口的延迟，比如有一个ALU指令和一个MUL指令
    且ALU和MUL均
    */
    val rs1_bsy         = RegInit(false.B)
    val rs2_bsy         = RegInit(false.B)
    val rs1_delay       = RegInit(0.U(Delay_Sz.W))
    val rs2_delay       = RegInit(0.U(Delay_Sz.W))
    //both ready and can issue
    val rs1_ready       = WireInit(false.B)
    val rs2_ready       = WireInit(false.B)
    //detect pdst===prsx
    val rs1_wakeupOH    = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.prs1
    }
    val rs2_wakeupOH    = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.prs2
    }
    // val 
    for(i <- 0 until numWakeupPorts){

    }



}

class IssueIO(
val issueWidth:Int,
val numEntries:Int,
val numWakeupPorts: Int,
val dispatchWidth:Int)(implicit p: Parameters)extends GRVBundle{
    val dis_uops    = Flipped(Vec(dispatchWidth,Decoupled(new MicroOp)))
    val issue_uops  = Vec(issueWidth,Valid(new MicroOp))

    val fu_using    = Input(Vec(issueWidth,Valid(UInt(FUC_SZ.W)))) 
    
    val wakeup      = Input(Vec(numWakeupPorts,new WakeupPort()))
    val flush       = Input(Bool())
}
class BaseIssueUnit(
val numWakeupPorts: Int,
val issueWidth:Int,
val numEntries:Int,
val iqType:Int,
val dispatchWidth:Int)(implicit p: Parameters)extends GRVModule{
    val io = IO(new IssueIO(issueWidth,numEntries,numWakeupPorts,dispatchWidth))

}