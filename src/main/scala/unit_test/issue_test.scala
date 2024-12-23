package grvcore

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.DontTouch
/*
需要的模块：
1.dispatch请求产生模块
2.wakeup产生模块
3.flush模块

测试要点：
1.issue可以正常入队，以及产生满信号：测试完成
2.issue可以正常唤醒：简单测试完成
3.issue可以正常仲裁：简单测试完成

目前简单的仲裁测试完成
之后会
1.设置多个EXU，每个EXU有不同的FU
2.验证延迟唤醒

*/
class DisGen (val dispatchWidth:Int)(implicit  p:Parameters) extends GRVModule{
    val io=IO(new Bundle{
        val dis_uops = Vec(dispatchWidth,Valid(new MicroOp))
        
        val full     = Input(Bool())
        // val ex_uops  = Vec(dispatchWidth,Valid(new MicroOp))
    })
    val uops = RegInit(VecInit.fill(dispatchWidth)(0.U.asTypeOf((new MicroOp))))

    val cnt = RegInit(0.U(32.W))
    cnt := cnt + 1.U
    io.dis_uops:= DontCare
    for(i <- 0 until dispatchWidth){
        io.dis_uops(i).bits.fu_code   := FU_ALU//Mux(cnt===2.U,FU_JMP,FU_ALU)
        io.dis_uops(i).bits.lrs1_rtype:= RT_FIX
        io.dis_uops(i).bits.lrs2_rtype:= RT_FIX
        io.dis_uops(i).bits.pdst      := Mux(io.dis_uops(i).valid,LFSR(pregSz+1)(pregSz-1,0)+1.U,0.U)
        io.dis_uops(i).bits.prs1      := RegNext(io.dis_uops(i).bits.pdst)
        io.dis_uops(i).bits.prs2      := RegNext(io.dis_uops(i).bits.pdst)
        io.dis_uops(i).bits.ldst_val  := io.dis_uops(i).bits.pdst=/=0.U
        io.dis_uops(i).bits.prs1_busy := io.dis_uops(i).bits.prs1=/=0.U 
        io.dis_uops(i).bits.prs2_busy := io.dis_uops(i).bits.prs2=/=0.U 
        io.dis_uops(i).bits.ftq_idx   := cnt+i.U
        io.dis_uops(i).valid := (!io.full)&&cnt=/=0.U

    }
}


class WakeUpGen (val issueWidth:Int,val dispatchWidth:Int,val numWakeupPorts:Int)(implicit  p:Parameters) extends GRVModule{
    val io=IO(new Bundle{
        val ex_uops  = Flipped(Vec(issueWidth,Valid(new MicroOp)))

        val wakeup   = Output(Vec(numWakeupPorts,new WakeupPort))
        val fu_using = Vec(issueWidth,Valid(UInt(FUC_SZ.W)))
    })

    for(i <- 0 until issueWidth){
        io.fu_using(i).valid := false.B
        io.fu_using(i).bits  := FU_ALU
        io.wakeup(i).valid   := io.ex_uops(i).valid
        io.wakeup(i).pdst    := io.ex_uops(i).bits.pdst
        io.wakeup(i).delay   := 0.U
    }

}
class IssueTest(implicit  p:Parameters) extends GRVModule with DontTouch{
    val dispatchWidth=2
    val issueWidth = 2
    val numWakeupPorts=2
    
    val disgen = Module(new DisGen(dispatchWidth))
    val check = Module(new Checker)
    val wakegen= Module(new WakeUpGen(issueWidth,dispatchWidth,numWakeupPorts))
    val issue  = Module(new BaseIssueUnit(numWakeupPorts,issueWidth,4,0,dispatchWidth))
    val timer = RegInit(0.U(32.W))
    dontTouch((disgen.io))
    timer := timer +1.U
    issue.io.dis_uops.valid:= disgen.io.dis_uops.map(_.valid).reduce(_||_)
    issue.io.dis_uops.bits := disgen.io.dis_uops
    issue.io.flush    := false.B
    issue.io.fu_using := wakegen.io.fu_using
    disgen.io.full :=(!issue.io.dis_uops.ready)
    val iss2ex = withReset(reset.asBool) {
        Module(new Queue(Vec(issueWidth,Valid(new MicroOp)), 1, pipe=true, flow=false)) }
    iss2ex.io.enq<>issue.io.issue_uops
        dontTouch(iss2ex.io)
    dontTouch(wakegen.io)
    wakegen.io.ex_uops:=iss2ex.io.deq.bits
    iss2ex.io.deq.ready := true.B
    issue.io.fu_using := wakegen.io.fu_using
    issue.io.wakeup   := wakegen.io.wakeup
    check.io.clock := clock
    check.io.reset := reset
    check.io.finish := false.B
    check.io.ret := false.B
    when(timer===100.U){
        check.io.finish := true.B
        check.io.ret := false.B
    }
}