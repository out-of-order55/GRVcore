package grvcore

import chisel3._
import chisel3.util._

class Grvcore(val coreParams: CoreConfig)
extends Module with ScalarOpConstants{
    def pipelineConnect[T <: Data, T2 <: Data](prevOut: DecoupledIO[T],
        thisIn: DecoupledIO[T]) = {
            prevOut.ready := thisIn.ready
            thisIn.bits := RegEnable(prevOut.bits, prevOut.valid && thisIn.ready)
            thisIn.valid := RegEnable(prevOut.valid,thisIn.ready)
            // thisOut.valid := thisIn.valid
            // thisOut.bits  := thisIn.bits
            // thisIn.ready := thisOut.ready
    }


    val io = IO(new Bundle {
        val icache = Flipped(new CacheIO(coreParams.xlen, coreParams.xlen))
        val dcache = Flipped(new CacheIO(coreParams.xlen, coreParams.xlen))
    })
    val ifu = Module(new IFU(coreParams.xlen)  )
    val idu = Module(new IDU(coreParams.xlen)  )
    val isu = Module(new Issue(coreParams.xlen))
    val exu = Module(new EXU(coreParams.xlen)  ) 
    val wbu = Module(new WBU(coreParams.xlen,coreParams.IsDiff)  )
    val regfiles = Module(new regfiles())

    pipelineConnect(ifu.io.out,idu.io.in)
    pipelineConnect(idu.io.out,isu.io.in)
    pipelineConnect(isu.io.out,exu.io.in)
    pipelineConnect(exu.io.out,wbu.io.in)
    
    

    io.icache<>ifu.io.icache
    io.dcache<>exu.io.dcache

    regfiles.io.wreq<>wbu.io.regfile
    regfiles.io.rreq <> isu.io.RegfileReq
    regfiles.io.rresp <> isu.io.RegfileResp

    exu.io.exu_bypass<>isu.io.ExuBypass
    wbu.io.wbu_bypass<>isu.io.WbuBypass

    ifu.io.branch <> wbu.io.bjp
}