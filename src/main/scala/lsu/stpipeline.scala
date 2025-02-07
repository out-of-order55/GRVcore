package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._



class STBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dis          = new DisIO//写入

    val req          = Flipped(Valid(new LSUReq))
    val write        = Flipped(new DCacheWriteIO)//used by sb


    val search_req    = Flipped(new LDQSearchReq)

    val check_unorder = Valid(new CheckRAWReq)
    val refillMsg     = Input(new RefillMsg())
    val wb_resp       = Valid(new ExuResp)
    val search_resp   = Vec(2,Vec(numReadport,(Valid(new LDQSearchResp))))//data
    val commit        = Input(new CommitMsg)
    val replay        = Output(new LSUReplay)
    val flush         = Input(Bool())
}
class STPipeline(implicit p: Parameters) extends GRVModule with HasDCacheParameters
with freechips.rocketchip.rocket.constants.MemoryOpConstants{
    val io = IO(new STBundle)
    val stq= Module(new STQ)
    val store_buffer = Module(new StoreBuffer)
    stq.io.commit := io.commit
    stq.io.dis    <>io.dis
    stq.io.flush  := io.flush
    // val s0_replay= io.write.req.valid&&(!io.write.req.ready)//s0阶段dcache无法接受请求
    val s0_replayMsg= WireInit(0.U.asTypeOf(new LSUReplay))

    val s0_valid = io.req.valid
    // &(!s0_replay)
    
    val s0_waddr = Wire(UInt(XLEN.W))
    val s0_wdata = Wire(UInt(XLEN.W))
    val s0_uop   = WireInit(io.req.bits.uop)
    val s0_offset  = s0_waddr(log2Ceil(XLEN/8)-1,0)
    val s0_align_wdata = s0_wdata<<(8.U*s0_offset)
    val s0_mask  = Mux(s0_uop.mem_size===0.U,"b0001".U,
                Mux(s0_uop.mem_size===1.U,"b0011".U,"b1111".U))
    val s0_align_mask = s0_mask<<s0_offset
    dontTouch(s0_waddr)
    // val s1_replay= RegNext(s0_replay)//目前没有replay的请求
    val s1_valid = RegNext(s0_valid&(!io.flush))
    val s1_waddr = RegNext(s0_waddr)
    val s1_uop   = RegNext(s0_uop)


    // val s2_replay= RegNext(s1_replay)
    val s2_fail  = WireInit(false.B)
    val s2_valid = RegNext(s1_valid&&(!io.flush))
    val s2_waddr = RegNext(s1_waddr)
    val s2_uop   = RegNext(s1_uop)
    dontTouch(s2_valid)
    dontTouch(s2_uop)
    val dcache_replayMsg = WireInit(0.U.asTypeOf(new LSUReplay))
    // s0_replayMsg.replay := s0_replay
    // s0_replayMsg.uop    := s0_uop
    s0_waddr := (io.req.bits.rs1_data.asSInt + io.req.bits.uop.imm_packed(19,8).asSInt).asUInt
    s0_wdata := io.req.bits.rs2_data
//////////////////////////   stage0    //////////////////////////////
/////////////////////////   TO STQ    ///////////////////////////////
    stq.io.pipe.s0_addr := OffsetAlign(s0_waddr)
    stq.io.pipe.s0_data := s0_align_wdata
    stq.io.pipe.s0_mask := s0_align_mask
    stq.io.pipe.s0_uop.valid := s0_valid
    stq.io.pipe.s0_uop.bits  := s0_uop
//////////////////////////CHECK UNORDER///////////////////////////////
    io.check_unorder.valid := s0_valid
    io.check_unorder.bits.check_addr := OffsetAlign(s0_waddr)
    io.check_unorder.bits.uop    := s0_uop
//////////////////////////   FORWARD    //////////////////////////////
    stq.io.search_req           := io.search_req
    store_buffer.io.search_req  := io.search_req
    val stq_resp = RegNext(stq.io.search_resp)
    val sb_resp  = RegNext(store_buffer.io.search_resp)
    io.search_resp(0):= stq_resp
    io.search_resp(1):= sb_resp

    dcache_replayMsg.replay := io.write.resp.bits.replay&&(io.write.resp.valid)
    dcache_replayMsg.uop    := s2_uop
    io.replay.replay := dcache_replayMsg.replay
    io.replay.uop    := dcache_replayMsg.uop
//////////////////////////   WB_RESP    //////////////////////////////
    io.wb_resp.valid        := s2_valid
    io.wb_resp.bits.uop     := s2_uop
    io.wb_resp.bits.wb_data := DontCare
//////////////////////////   DCache    //////////////////////////////
    store_buffer.io.dcache_write<>io.write
    store_buffer.io.refillMsg := io.refillMsg
    store_buffer.io.flush := io.flush
//////////////////////////  STQ2SB    //////////////////////////////
    //这里是时序瓶颈，之后插入寄存器
    store_buffer.io.sb_req <> stq.io.sb_req
}