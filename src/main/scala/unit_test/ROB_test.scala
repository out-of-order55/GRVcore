package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
class ROBEnqGen (implicit  p:Parameters) extends GRVModule{
    val io=IO(new Bundle{
        val dis2rob     = Decoupled(new robEnqueue)
        val rob_idxs    = Input(Vec(coreWidth,UInt(log2Ceil(ROBEntry).W)))
        val wb_resp     = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, Valid(new ExuResp))))
        val br_update   = Output(new BrUpdateInfo)
    })
}
class ROBTest (implicit  p:Parameters) extends GRVModule{

    val rob         = Module(new ROB)
    val dispatcher  = Module(new BaseDispatcher)
    

}