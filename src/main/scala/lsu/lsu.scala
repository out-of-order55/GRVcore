package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
class LSUReq(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val uop          = new MicroOp
    val rs1_data     = UInt(XLEN.W)
    val rs2_data     = UInt(XLEN.W)
}
class LSUReplay(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val uop          = new MicroOp
    val replay       = Bool()
}
/* 
目前没完成的功能：
1.重新发送请求
 */
class DisIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val enq     = Vec(coreWidth,Flipped(Decoupled(new MicroOp)))//dispatch
    val enq_idx = (Vec(coreWidth,Valid(UInt(log2Ceil(numLDQs).W))))//dispatch
}
class LSUBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dis          = Vec(2,new DisIO)//写入 0 for ld 1 for st

    
    val ld_req           = Vec(numReadport,Flipped(Valid(new LSUReq)))//read issue
    val ld_wb_resp       = Vec(numReadport,Valid(new ExuResp))
    val ld_replay        = Output(Vec(numReadport,new LSUReplay))
    val st_req           = Flipped(Valid(new LSUReq))
    val st_wb_resp       = Valid(new ExuResp)
    val st_replay        = Output(new LSUReplay)
    //resp for check 
    val check_resp    = Output(new CheckRAWResp)

    val commit        = Input(new CommitMsg)
    val flush         = Input(Bool())
}

class LSU(implicit p: Parameters) extends LazyModule{
    lazy val module = new LSUImp(this)
    val ldcache = LazyModule(new DCache())

    val masterNode = ldcache.masterNode

}
class LSUImp(val outer: LSU)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasDCacheParameters with GRVOpConstants{
    val io = IO(new LSUBundle)
    val dcache = outer.ldcache.module
    val st_pipeline = Module(new STPipeline)
    val ld_pipeline = Module(new LDPipeline)
    
/////////////////////////   TO ST     ///////////////////////////////
    st_pipeline.io.dis <> io.dis(0)
    st_pipeline.io.req := io.st_req
    st_pipeline.io.flush:= io.flush
    st_pipeline.io.commit:= io.commit
    st_pipeline.io.wb_resp<> io.st_wb_resp
/////////////////////////   TO LD     ///////////////////////////////
    ld_pipeline.io.dis <>io.dis(1)
    ld_pipeline.io.req := io.ld_req
    ld_pipeline.io.flush:= io.flush
    ld_pipeline.io.commit:= io.commit
    ld_pipeline.io.wb_resp<> io.ld_wb_resp
/////////////////////////   DCache     ///////////////////////////////
    dcache.io.flush := io.flush
    dcache.io.read <>ld_pipeline.io.read
    dcache.io.write <> st_pipeline.io.write
    dcache.io.s1_kill:= false.B
    dcache.io.s2_kill:= false.B
    ld_pipeline.io.refillMsg := dcache.io.refillMsg
    st_pipeline.io.refillMsg := dcache.io.refillMsg
/////////////////////////   LD&ST     ///////////////////////////////
    ld_pipeline.io.check_unorder := st_pipeline.io.check_unorder
    ld_pipeline.io.search_resp   := st_pipeline.io.search_resp
    st_pipeline.io.search_req    := ld_pipeline.io.search_req
    dontTouch(st_pipeline.io.search_req)
    dontTouch(ld_pipeline.io.search_resp)

    io.check_resp := ld_pipeline.io.check_resp

    io.st_replay<>st_pipeline.io.replay
    io.ld_replay<>ld_pipeline.io.replay
}