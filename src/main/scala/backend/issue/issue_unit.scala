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

    val flush       = Input(Bool())
    //ready
    val req         = Output(Bool())
    val can_allocate= Output(Bool())
    //select
    val grant       = Input(Bool())
    //from dispatch
    val dis_uop     = Flipped(Valid(new MicroOp))
    //wake up
    val wakeup      = Input(Vec(numWakeupPorts,new WakeupPort))
    
    
    val out_uop     = Output(Valid(new MicroOp))
    //to exu
    val ex_uop      = Output(Valid(new MicroOp))
}
class IssueIO(
val issueWidth:Int,
val numEntries:Int,
val numWakeupPorts: Int,
val dispatchWidth:Int)(implicit p: Parameters)extends GRVBundle{
    val dis_uops    = Flipped(Vec(dispatchWidth,Decoupled(new MicroOp)))
    val issue_uops  = Vec(issueWidth,Valid(new MicroOp))

    val fu_using    = Input(Vec(issueWidth,Valid(UInt(FUC_SZ.W))))//for unpipeline:DIV 
    
    val wakeup      = Input(Vec(numWakeupPorts,new WakeupPort()))
    val flush       = Input(Bool())
}
class IssueEntry(val numWakeupPorts: Int)(implicit p: Parameters) extends  GRVModule{
    val io = IO(new IssueEntryIO(numWakeupPorts))

    val entry = RegInit(0.U.asTypeOf(Valid(new MicroOp)))
    
    //enq 
    //当有入队的valid信号，表示一定为空或者有要出队的
    //本周期entry无效或者本周期指令有效，但是已经被仲裁电路选中
    //这个信号是为了显示本entry是否为空
    val can_allocate = ((entry.valid&&io.grant)||(!entry.valid))
    val cna_enq = can_allocate&io.dis_uop.valid
    io.can_allocate := can_allocate
    entry.bits := Mux(cna_enq,io.dis_uop.bits,entry.bits)
    entry.valid:= Mux(io.flush,false.B,
                    Mux(cna_enq,true.B,entry.valid))
    /* 
    wake up
    问题：如何去调度每个wakeup端口的延迟，比如有一个ALU指令和一个MUL指令
    且ALU和MUL均
    我们根据valid标示，
    然后wakeup的数量和执行单元的数量相同，每个执行单元都可以唤醒，但有的会延迟唤醒，比如MEM和MUL和DIV，
    这里需要注意的是，最好将issuewidth和执行单元一致，这样可以最大限度的利用ISSUE queue
    然后寄存器读端口的数目和写端口的数目和执行单元个数一致                

    */
    val rs1_bsy         = RegInit(false.B)
    val rs2_bsy         = RegInit(false.B)
    val rs1_delay       = RegInit(0.U(Delay_Sz.W))
    val rs2_delay       = RegInit(0.U(Delay_Sz.W))
    //both ready and can issue
    val rs1_ready       = WireInit(false.B)
    val rs2_ready       = WireInit(false.B)
    val rs1_wait        = RegInit(false.B)
    val rs2_wait        = RegInit(false.B)

    //detect pdst===prsx
    val rs1_wakeupOH    = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.bits.prs1&&w.delay===0.U
    }
    val rs2_wakeupOH    = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.bits.prs2&&w.delay===0.U
    }
    val rs1_wakeup_delay = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.bits.prs1&&w.delay=/=0.U
    }
    val rs2_wakeup_delay = io.wakeup.map{w=>
        w.valid&&w.pdst===entry.bits.prs2&&w.delay=/=0.U
    }


    rs1_delay := Mux(rs1_wakeup_delay.reduce(_||_),io.wakeup(PriorityEncoder(rs1_wakeup_delay)).delay,
                    Mux(rs1_delay=/=0.U,rs1_delay-1.U,rs1_delay))
    rs1_wait  := Mux(rs1_wakeup_delay.reduce(_||_),true.B,
                    Mux(rs1_wait&&rs1_delay===0.U,false.B,rs1_wait))
    rs1_bsy   := Mux(io.dis_uop.valid,io.dis_uop.bits.prs1_busy,
                    Mux(rs1_wakeupOH.reduce(_||_)||rs1_wait&&rs1_delay===0.U,false.B,rs1_bsy))

    rs2_delay := Mux(rs2_wakeup_delay.reduce(_||_),io.wakeup(PriorityEncoder(rs2_wakeup_delay)).delay,
                    Mux(rs2_delay=/=0.U,rs2_delay-1.U,rs2_delay))
    rs2_wait  := Mux(rs2_wakeup_delay.reduce(_||_),true.B,
                    Mux(rs2_wait&&rs2_delay===0.U,false.B,rs2_wait))
    rs2_bsy   := Mux(io.dis_uop.valid,io.dis_uop.bits.prs2_busy,
                    Mux(rs2_wakeupOH.reduce(_||_)||rs2_wait&&rs2_delay===0.U,false.B,rs2_bsy))


    when(io.flush){
        entry.valid := false.B
    }

    io.req    := (!rs1_bsy)&(!rs2_bsy)&(entry.valid)

    io.ex_uop := entry
    io.out_uop:= entry
    // io.dis_uop:= entry
    

}

/* 
issue 模块分三个设计：
1.首先设计非压缩队列并且只要找到issuewidth条就可以发射
2.再设计非压缩队列+age仲裁
3.最后设计压缩队列

目前先完成最简单的1
 */
class BaseIssueUnit(
val numWakeupPorts: Int,
val issueWidth:Int,
val numEntries:Int,
val iqType:Int,
val dispatchWidth:Int)(implicit p: Parameters)extends GRVModule{
    val io = IO(new IssueIO(issueWidth,numEntries,numWakeupPorts,dispatchWidth))

    val entrys = Seq.fill(numEntries)(Module(new IssueEntry(numWakeupPorts)))

    val entry_can_alloc = VecInit(entrys.map{e=>e.io.can_allocate})
    val entry_can_iss   = VecInit(entrys.map{e=>e.io.req})//可以发射的指令
    val ex_uops         = VecInit(entrys.map(_.io.ex_uop))
    val full = WireInit(false.B)

    val in_sels = SelectFirstN(entry_can_alloc.asUInt,dispatchWidth)
    val iss_sels= SelectFirstN(entry_can_iss.asUInt,issueWidth)

    //又是一个将n个数据写入m项的queue中，采用OH

    //每个issue接口都有一个dispatchWidth大小的来选择数据
    //in data
    (entrys.zipWithIndex).foreach{case(entry,idx)=>
        val issue_wenOH = (0 until dispatchWidth).map{i=>
            io.dis_uops(i).valid&&idx.U===OHToUInt(in_sels(i))
        }
        val in_Data = Mux1H(issue_wenOH,io.dis_uops.map(_.bits))
        
        entry.io.dis_uop.valid := issue_wenOH.reduce(_||_) 
        entry.io.dis_uop.bits  := in_Data
    }
    /* 
    需要考虑：
    1.FU是否空闲
    2.是否有多个同FU的指令被仲裁，这样只会有一个会被送出去
     */
    for(i <- 0 until issueWidth){
        val iss_uop = Mux1H(iss_sels(i),ex_uops)
        val can_iss = (!io.fu_using(i).valid)&&((io.fu_using(i).bits&iss_uop.bits.fu_code)=/=0.U)

        // val can_iss = 
    }
    


}