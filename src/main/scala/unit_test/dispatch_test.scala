package grvcore

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.DontTouch

class DecGen(implicit p: Parameters)extends GRVModule{
    val io = IO(new Bundle{
        val dec_uops=Vec(coreWidth, DecoupledIO(new MicroOp))
    })

    val cnt = RegInit(false.B)
    cnt := !cnt
    val sel = RegInit(0.U(32.W))
    sel := sel+1.U
    io.dec_uops := DontCare
    for(i <- 0 until coreWidth){
        io.dec_uops(i).bits.iq_type := Mux(i.U===0.U,IQT_INT,IQT_MEM)
        io.dec_uops(i).bits.ftq_idx := sel
        io.dec_uops(i).valid        := true.B
    }
    
}
class DispatcherTest(implicit p: Parameters)extends GRVModule{
    val check = Module(new Checker)
    val dispatcher = Module(new BaseDispatcher())
    val dec        = Module(new DecGen)
    dispatcher.io.dis_uops := DontCare
    val timer = RegInit(0.U(32.W))

    timer := timer +1.U
    dispatcher.io.ren_uops<>dec.io.dec_uops
    dontTouch(dispatcher.io)
    check.io.clock := clock
    check.io.reset := reset
    check.io.finish := false.B
    check.io.ret := false.B
    when(timer===100.U){
        check.io.finish := true.B
        check.io.ret := false.B
    }
    for(i<- 0 until issueParams.size){
        val issueParam = issueParams(i)
        val dis        = dispatcher.io.dis_uops(i)
        println(f"${issueParam.iqType }\n")
        for(j<-0 until coreWidth){
            dis(j).ready := (issueParam.iqType.U&dis(i).bits.iq_type)=/=0.U
        }
    }
}
