package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
class DispatchIO(implicit p: Parameters) extends GRVBundle
{

    val ren_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))

    val dis_uops = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, DecoupledIO(new MicroOp))))
}
class BaseDispatcher(implicit p: Parameters) extends GRVModule{
    val io = IO(new DispatchIO)
    //只有一个IQ可以接受corewidth大小的，此时才可以ready
    val ren_readys = io.dis_uops.map(d=>VecInit(d.map(_.ready)).asUInt).reduce(_&_)
    for(i <- 0 until coreWidth){
        io.ren_uops(i).ready := ren_readys(i)
    }
    for(i<- 0 until issueParams.size){
        val issueParam = issueParams(i)
        val dis        = io.dis_uops(i)
        for(j<-0 until coreWidth){
            dis(j).valid := io.ren_uops(j).valid && ((io.ren_uops(j).bits.iq_type & issueParam.iqType.U) =/= 0.U)
            dis(j).bits  := io.ren_uops(j).bits
        }
    }
}
/* 
一个周期可以将指令发往不同的IQ
 */
class ComplexDispatcher(implicit p: Parameters) extends GRVModule{
    val io = IO(new DispatchIO)
    val ren_readys = Wire(Vec(issueParams.size, Vec(coreWidth, Bool())))
    //每个的ren都不同
    for(i<- 0 until issueParams.size){
        val ren = Wire(Vec(coreWidth, Decoupled(new MicroOp)))
        val issueParam = issueParams(i)
        val dis        = io.dis_uops(i)
        ren <> io.ren_uops
        //1说明选择这个
        val iq_type = ren.map{u=>(u.bits.iq_type & issueParam.iqType.U).orR}

        //
        for(j<-0 until coreWidth){
            ren(j).valid:=io.ren_uops(j).valid&iq_type(i)
        }
        //n入k出
        val compactor = Module(new Compactor(coreWidth,issueParam.dispatchWidth,new MicroOp))
        compactor.io.in<>ren
        dis<>compactor.io.out
        ren_readys(i) := (ren zip iq_type).map{ case(a,b)=> a.ready || !b}
    }
    ren_readys.map(r=> r.reduce(_&&_)) zip io.ren_uops map{case(r,i)=>
        i.ready :=  r
    }
}