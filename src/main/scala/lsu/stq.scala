package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
import freechips.rocketchip.regmapper.RegField.w
/* 
STQ需要的内容：
1.地址
2.数据
3.状态
4.数据mask
已经commit的数据会被写入store buffer

Stage 0
计算地址|写入STQ数据
Stage 1
开始进行访存依赖检查，如果发生sw/ld违例，需要通知ROB，然后重新从这个load指令
执行
Stage 2
完成访存依赖检查
通知 ROB 可以提交指令
 */
class STQReq(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val uop     =   new MicroOp
    val data    =   UInt(XLEN.W)
    val addr    =   UInt(XLEN.W)
    val mask    =   UInt((XLEN/8).W)
}
class STQPipeIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val s0_addr        = Input(UInt(XLEN.W))
    val s0_data        = Input(UInt(XLEN.W))
    val s0_mask        = Input(UInt((XLEN/8).W))
    val s0_uop         = Flipped(Valid(new MicroOp))
    // val s2_uop         = Input(Vec(numReadport,new MicroOp))
    // val s2_wb_req      = Input(Vec(numReadport,Bool()))
    // val s2_miss        = Input(Vec(numReadport,Bool()))
}
class STQCommit(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val uop     =   new MicroOp
    val data    =   UInt(XLEN.W)
    val addr    =   UInt(XLEN.W)
    val mask    =   UInt((XLEN/8).W)
}
class STQBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dis          = new DisIO//写入
    
    val pipe           = new STQPipeIO//写入地址和数据
    val sb_req        = (Decoupled(new STQCommit))
    // val wb_resp       = Valid(new ExuResp)
    val search_req    = Flipped(new LDQSearchReq)
    val search_resp   = Vec(numReadport,(Valid(new LDQSearchResp)))
    val commit        = Input(new CommitMsg)
    val flush         = Input(Bool())
}
class STQ(implicit p: Parameters) extends GRVModule with HasDCacheParameters
with freechips.rocketchip.rocket.constants.MemoryOpConstants  
{
    val io = IO(new STQBundle)
    val stqSz = log2Ceil(numSTQs)
    class STQFlag extends Bundle{
        val allocated = Bool()
        val datavalid = Bool()
        val addrvalid = Bool()
        val commited  = Bool()
    }
    class STQEntry extends Bundle{
        val flag = new STQFlag
        val uop   = new MicroOp//冗余部分，其实用不倒那末多

        val data = UInt(XLEN.W)
        val addr = UInt(XLEN.W)
        val mask = UInt((XLEN/8).W)
    }
    // val s_idle::s_allocated::s_datavalid::s_miss :: Nil = Enum(5)
    val s_idle::s_normal::s_repair::Nil = Enum(3)
    val stq_state = RegInit(s_idle)
    val stq = RegInit(VecInit.fill(numSTQs)(0.U.asTypeOf(new STQEntry)))
    val stq_enq_ptr  = RegInit(0.U(log2Ceil(numSTQs+1).W))
    val stq_deq_ptr  = RegInit(0.U(log2Ceil(numSTQs+1).W))
    val stq_commit_ptr  = RegInit(0.U(log2Ceil(numSTQs+1).W))
    val numValid = PopCount(stq.map(_.flag.allocated))
    val ValidVec = stq.map{i=>i.flag.allocated&&i.flag.addrvalid&&i.flag.datavalid}
    val numEnq = PopCount(io.dis.enq.map{i=>i.valid&&i.bits.mem_cmd===M_XWR})

    
    dontTouch(stq_commit_ptr)
    val can_commit = io.commit.valid zip io.commit.commit_uops map{case(vld,uop)=>
        vld&&uop.mem_cmd===M_XWR
    }
    val commit_idxs = VecInit.tabulate(coreWidth)(i => PopCount(can_commit.take(i)))
    val commit_offset = VecInit(commit_idxs.map(_+stq_commit_ptr(log2Ceil(numSTQs)-1,0)))

    val deq_ptr_value = stq_deq_ptr(log2Ceil(numSTQs)-1,0)
    dontTouch(deq_ptr_value)
    val deq_valid = stq(deq_ptr_value).flag.commited


    val flush = stq_state===s_repair&&(!deq_valid)||(io.flush&&(!deq_valid)&&(!can_commit.reduce(_||_)))


    // val numDeq = io.sb_req.fire
    
    val enqNextPtr = stq_enq_ptr+numEnq
	// val deqNextPtr = stq_deq_ptr+numDeq.asUInt
    val full = (stq_enq_ptr(stqSz)=/=stq_deq_ptr(stqSz)&&(numEnq+stq_enq_ptr)(stqSz-1,0)>stq_deq_ptr(stqSz-1,0))||
    numValid===numSTQs.U
	val empty = (stq_enq_ptr(stqSz)===stq_deq_ptr(stqSz))&&stq_enq_ptr(stqSz-1,0)===stq_deq_ptr(stqSz-1,0)
    
    io.dis.enq.foreach{dis=>
        dis.ready := (!full)&(stq_state=/=s_repair)
    }
    when(flush){
        stq_enq_ptr := 0.U
    }
    .elsewhen(numEnq.orR){
        stq_enq_ptr := stq_enq_ptr + numEnq
    }
////////////////////////////enq logic   ///////////////////////////// 
    val enq_idxs = VecInit.tabulate(coreWidth)(i => PopCount(io.dis.enq.map{i=>i.fire&&i.bits.mem_cmd===M_XWR}.take(i)))
    val enq_offset = VecInit(enq_idxs.map(_+stq_enq_ptr(stqSz-1,0)))
    dontTouch(enq_idxs)
    dontTouch(enq_offset)
    when(flush){
        for(i <- 0 until numSTQs){
            stq(i).flag := 0.U.asTypeOf(new STQFlag)
        }
    }
    for(i <- 0 until coreWidth){
        val do_enq = io.dis.enq(i).fire&&io.dis.enq(i).bits.mem_cmd===M_XWR&&(!full)
        for(j <- 0 until numSTQs){
            when(do_enq&&enq_offset(i)===j.U){
                stq(j).flag.allocated := true.B
                stq(j).uop := io.dis.enq(i).bits
            }
        }
        io.dis.enq_idx(i).bits := enq_offset(i)
        io.dis.enq_idx(i).valid:= do_enq
    }
////////////////////////////data addr in ///////////////////////////// 
    val s0_addr = io.pipe.s0_addr
    val s0_data = io.pipe.s0_data
    val s0_mask = io.pipe.s0_mask
    val s0_idx  = io.pipe.s0_uop.bits.stq_idx(log2Ceil(numSTQs)-1,0)
    val s0_valid= io.pipe.s0_uop.valid&&io.pipe.s0_uop.bits.mem_cmd===M_XWR

    stq(s0_idx).addr := Mux(s0_valid,s0_addr,stq(s0_idx).addr)
    stq(s0_idx).data := Mux(s0_valid,s0_data,stq(s0_idx).data)
    stq(s0_idx).mask := Mux(s0_valid,s0_mask,stq(s0_idx).mask)

    stq(s0_idx).flag.addrvalid := Mux(s0_valid,true.B,stq(s0_idx).flag.addrvalid)
    stq(s0_idx).flag.datavalid := Mux(s0_valid,true.B,stq(s0_idx).flag.datavalid)
///////////////////////////forward data//////////////////////////////
    val forward_addr = io.search_req.addr
    val forward_OH = io.search_req.stq_idx.map{i=> UIntToOH(i)}
    val deq_OH     = UIntToOH(stq_deq_ptr(log2Ceil(numSTQs)-1,0))
    val differentFlag = io.search_req.stq_idx.map{i=>
        i(log2Ceil(numSTQs))=/=stq_deq_ptr(log2Ceil(numSTQs))
    }
    val forward_mask1 = Wire(Vec(numReadport,UInt(numSTQs.W)))
    val forward_mask2 = Wire(Vec(numReadport,UInt(numSTQs.W)))
    for(i <- 0 until numReadport){
        forward_mask1(i) := Mux(differentFlag(i),MaskUpper(deq_OH),MaskLower(forward_OH(i))&MaskUpper(deq_OH))
        forward_mask2(i) := Mux(differentFlag(i),MaskLower(forward_OH(i)),0.U)
    }

    val forward_sels1 = WireInit(VecInit.fill(numReadport)(VecInit.fill(numSTQs)(false.B)))
    val forward_sels2 = WireInit(VecInit.fill(numReadport)(VecInit.fill(numSTQs)(false.B)))
    val forward_idxs = Wire(Vec(numReadport,UInt(log2Ceil(numSTQs).W)))
    for(i <- 0 until numReadport){
        for(j <- 0 until numSTQs){
            forward_sels1(i)(j) := Mux(forward_mask1(i)(j)===1.U,stq(j).addr===forward_addr(i).bits&&forward_addr(i).valid&&ValidVec(j),
                                false.B)
            forward_sels2(i)(j) := Mux(forward_mask2(i)(j)===1.U,stq(j).addr===forward_addr(i).bits&&forward_addr(i).valid&&ValidVec(j),
                                false.B)
        }
        forward_idxs(i) := Mux(forward_sels2(i).reduce(_||_),PriorityEncoder(forward_sels2(i)),PriorityEncoder(forward_sels1(i)))
        io.search_resp(i).valid     := forward_sels1(i).reduce(_||_)||forward_sels1(i).reduce(_||_)
        io.search_resp(i).bits.data := stq(forward_idxs(i)).data
        io.search_resp(i).bits.mask := stq(forward_idxs(i)).mask
        io.search_resp(i).bits.uop  := stq(forward_idxs(i)).uop
    }
    
    
///////////////////////////    commit     //////////////////////////////

    dontTouch(commit_offset)
    for(i<- 0 until coreWidth){
        when(can_commit(i)){
            stq(commit_offset(i)).flag.commited := true.B
        }
    }
    when(flush){
        stq_commit_ptr := 0.U
    }.elsewhen(can_commit.reduce(_||_)){
        stq_commit_ptr := stq_commit_ptr + PopCount(can_commit)
    }
///////////////////////////TO STORE BUFFER//////////////////////////////


    io.sb_req.valid := deq_valid
    val can_go = io.sb_req.fire
    dontTouch(can_go)
    when(can_go){
        stq(deq_ptr_value).flag := 0.U.asTypeOf(new STQFlag)
    }
	switch (stq_state){
		is(s_idle){
			stq_state := s_normal
		}
        is(s_normal){
            when(io.flush&&(deq_valid||can_commit.reduce(_||_))){
                stq_state := s_repair
            }
        }
        is(s_repair){
            when(!deq_valid){
                stq_state := s_normal
            }
        }
    }
    io.sb_req.bits.addr := stq(deq_ptr_value).addr
    io.sb_req.bits.data := stq(deq_ptr_value).data
    io.sb_req.bits.mask := stq(deq_ptr_value).mask
    io.sb_req.bits.uop  := stq(deq_ptr_value).uop
    stq_deq_ptr := Mux(flush,0.U,Mux(can_go,stq_deq_ptr+1.U,stq_deq_ptr))
}