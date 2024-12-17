package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
class FreeListTest (implicit  p:Parameters) extends GRVModule{
    val freelist    = Module(new FreeList)
    val check       = Module(new Checker)
    val reqs        = RegInit(0.U(coreWidth.W))
    val timer       = RegInit(0.U(32.W))

    freelist.io.reqs := reqs.asBools
    dontTouch(freelist.io.alloc_pregs)
    
    freelist.io.dealloc_pregs:= RegNext(RegNext(freelist.io.alloc_pregs))
    val deqNum   = PopCount(freelist.io.dealloc_pregs.map(_.valid))
    val enqNum   = PopCount(reqs)
    val startNum = RegInit(63.U(10.W))
    startNum := startNum-enqNum+deqNum

    reqs := reqs + 1.U
    dontTouch(startNum)
    dontTouch(enqNum)
    dontTouch(deqNum)
    timer := timer +1.U
    check.io.clock := clock
    check.io.reset := reset
    check.io.finish := false.B
    check.io.ret := false.B
    when(timer===500.U){
        check.io.finish := true.B
        check.io.ret := false.B
    }
    .elsewhen(startNum=/=PopCount(freelist.io.debug_freelist)){
        check.io.finish := true.B
        check.io.ret := true.B
    }
}