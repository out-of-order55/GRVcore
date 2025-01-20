package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config._
class CheckRAWReq(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val check_addr = UInt(XLEN.W)
    val uop         = new MicroOp//sw uop
}

class CheckRAWResp(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val redirect    = Bool()
    val uop         = new MicroOp
}
// class LDReq(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
//     val uop          = new MicroOp
//     val rs1_data     = UInt(XLEN.W)
//     val rs2_data     = UInt(XLEN.W)
// }
class LDBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dis          = new DisIO//写入

    val req          = Vec(numReadport,Flipped(Valid(new LSUReq)))
    val read         = Vec(numReadport,Flipped(new DCacheReadIO))

    val refillMsg= Input(new RefillMsg())

    val search_req = new LDQSearchReq

    val check_unorder = Flipped(Valid(new CheckRAWReq)) 
    val check_resp    = Output(new CheckRAWResp)
    val wb_resp       = Vec(numReadport,Valid(new ExuResp))
    val search_resp   = Vec(2,Vec(numReadport,Flipped(Valid(new LDQSearchResp))))//data
    val commit        = Input(new CommitMsg)
    val flush         = Input(Bool())
}
/* 

面积主要在检查违例上，可以去尝试优化比较逻辑，
时钟频率可以达到1GHz
目前对于load没有进行replay，当bank冲突和mshr满的时候都需要replay
 */
class LDPipeline(implicit p: Parameters) extends GRVModule with HasDCacheParameters
with freechips.rocketchip.rocket.constants.MemoryOpConstants{
    val io = IO(new LDBundle)
    val ldq = Module(new LDQ)
    def isOlder(idx1:UInt,idx2:UInt,totalSz:Int):Bool={
        (idx1(totalSz)===idx2(totalSz)&&
        idx1(totalSz-1,0)<idx2(totalSz-1,0)||
        idx1(totalSz)   =/=idx2(totalSz)&&
        idx1(totalSz-1,0)>idx2(totalSz-1,0))
    }


    val s0_valid = VecInit(io.read map{i=>
        i.req.fire
    })
    dontTouch(s0_valid)
    val s0_raddr = Wire(Vec(numReadport,UInt(XLEN.W)))
    val s0_uop   = WireInit(VecInit(io.req.map(_.bits.uop)))
    // val s0_check_uop = Wire(new MicroOp)
    val s0_fail  = WireInit(false.B)

    // val s1_check_uop = Wire(new MicroOp)
    val s1_valid = RegNext(VecInit(s0_valid.map{i=>i&(!io.flush)}))
    val s1_raddr = RegNext(s0_raddr)
    val s1_uop   = RegNext(s0_uop)
    val s1_fail  = WireInit(false.B)

    // val s2_check_uop = Wire(new MicroOp)
    val s2_fail  = WireInit(false.B)
    val s2_valid = RegNext(VecInit(s1_valid.map{i=>i&(!io.flush)}))
    val s2_raddr = RegNext(s1_raddr)
    val s2_uop   = RegNext(s1_uop)
    val s2_wbvalid = WireInit(VecInit.fill(numReadport)(false.B))
    val s2_dcache_resp = WireInit(VecInit.fill(numReadport)(0.U.asTypeOf(new ReadResp)))

    val bypassMsg =Wire(Vec(numReadport,new LDQSearchResp))
    val bypass_en =WireInit(VecInit.fill(numReadport)(false.B))
    val stq_resp = io.search_resp(0)
    val sb_resp  = io.search_resp(1)

    val finalMsg  = Wire(Vec(numReadport,new ExuResp))
	val splitData = Wire(Vec(numReadport,Vec(XLEN/8,UInt(8.W))))

    ldq.io.commit := io.commit
    ldq.io.refillMsg := io.refillMsg
    // ldq.io.dis    := io.dis
    ldq.io.flush  := io.flush
    ldq.io.s1_kill := false.B
    ldq.io.s2_kill := false.B
    ldq.io.check_unorder := io.check_unorder
    for(i <- 0 until numReadport){
        ldq.io.search_resp(i).bits  := bypassMsg(i)
        ldq.io.search_resp(i).valid := bypass_en(i) 
    }
    
    for(i <- 0 until coreWidth){
        io.dis.enq(i).ready     := ldq.io.dis.enq(i).ready&&(!io.flush)
        io.dis.enq_idx(i)       := ldq.io.dis.enq_idx(i)
        ldq.io.dis.enq(i).bits  := io.dis.enq(i).bits
        ldq.io.dis.enq(i).valid := io.dis.enq(i).valid
    }
//////////////////////////   stage0    //////////////////////////////
////////////////////////////to DCache///////////////////////////////

    for(i <- 0 until numReadport){
        s0_raddr(i)                 := (io.req(i).bits.rs1_data.asSInt + io.req(i).bits.uop.imm_packed(19,8).asSInt).asUInt
        io.read(i).req.valid        := io.req(i).valid
        io.read(i).req.bits.addr    := OffsetAlign(s0_raddr(i))
    }
///////////////////////////to LDQ///////////////////////////////////
    for(i <- 0 until numReadport){
        ldq.io.pipe.s0_addr(i)      := OffsetAlign(s0_raddr(i))
        ldq.io.pipe.s0_uop(i).bits  := io.req(i).bits.uop
        ldq.io.pipe.s0_uop(i).valid := io.req(i).valid
    }

//////////////////////////   stage1    //////////////////////////////
//////////////////////////forward data req///////////////////////////

    for(i <- 0 until numReadport){
        io.search_req.addr(i).valid := s1_valid(i)
        io.search_req.addr(i).bits  := s1_raddr(i)
        io.search_req.stq_idx(i)    := s1_uop(i).stq_idx
    }
//////////////////////////   stage2    ////////////////////////////// 
////////////////////////////recieve resp///////////////////////////// 

    for(i <- 0 until numReadport){
        bypassMsg(i) := Mux(stq_resp(i).valid,stq_resp(i).bits,
                        Mux(sb_resp(i).valid,sb_resp(i).bits,0.U.asTypeOf((new LDQSearchResp))))
        bypass_en(i) := stq_resp(i).valid||sb_resp(i).valid
        
    }



    //reoriganize data
    for(i <- 0 until numReadport){
        val bypass_mask = bypassMsg(i).mask
        val bypass_data = bypassMsg(i).data
        val dcache_data = s2_dcache_resp(i).data
        for(j<- 0 until XLEN/8){
            splitData(i)(j) := Mux(bypass_mask(j)===1.U,bypass_data((i+1)*8-1,i*8),dcache_data((i+1)*8-1,i*8)) 
        }
        val wb_offset = s2_raddr(i)(log2Ceil(XLEN/8)-1,0)
        val wb_data = Cat(splitData(i).map(_.asUInt).reverse)>>(8.U*wb_offset)
        val wb_sels = Cat(s2_uop(i).mem_signed.asUInt,s2_uop(i).mem_size)

        finalMsg(i).uop     := s2_uop(i)
        finalMsg(i).wb_data := newRdataHelper(wb_sels,wb_data)
    }

    for(i <- 0 until numReadport){
        
        s2_dcache_resp(i)   :=  io.read(i).resp.bits
        s2_wbvalid(i)       :=  io.read(i).resp.bits.hit&&s2_valid(i)&&(!io.flush)
        //TO LDQ
        ldq.io.pipe.s2_miss(i)  := (!io.read(i).resp.bits.hit)&&s2_valid(i)&&(!io.flush)
        ldq.io.pipe.s2_uop(i)   := s2_uop(i)
        ldq.io.pipe.s2_wb_req(i):= io.read(i).resp.bits.hit&&s2_valid(i)&&(!io.flush)
    }
////////////////////////////wb   resp///////////////////////////// 
    for(i <- 0 until numReadport){
        io.wb_resp(i).valid := Mux(s2_wbvalid(i),true.B,Mux(ldq.io.wb_resp(i).valid,true.B,false.B))
        io.wb_resp(i).bits  := Mux(s2_wbvalid(i),finalMsg(i),
                            Mux(ldq.io.wb_resp(i).valid,ldq.io.wb_resp(i).bits,0.U.asTypeOf(new ExuResp)))
        ldq.io.wb_resp(i).ready := (!s2_wbvalid(i))
        
    }
///////////////////////////check unorder//////////////////////////
    /* 
    s0      s1      s2      ldq
        sel             sel
                sel
     */
    val st_rob_idx = io.check_unorder.bits.uop.rob_idx
    val st_valid = io.check_unorder.valid
    val st_addr= io.check_unorder.bits.check_addr
    val s0_check_sels= VecInit(s0_valid  zip s0_uop zip s0_raddr map{case((valid,uop),addr)=>
        st_valid&&valid&&isOlder(st_rob_idx,uop.rob_idx,log2Ceil(ROBEntry))&&OffsetAlign(addr)===st_addr
    })
    val s1_check_sels= VecInit(s1_valid  zip s1_uop zip s1_raddr map{case((valid,uop),addr)=>
        st_valid&&valid&&isOlder(st_rob_idx,uop.rob_idx,log2Ceil(ROBEntry))&&OffsetAlign(addr)===st_addr
    })
    val s2_check_sels= VecInit(s2_valid zip s2_uop zip s2_raddr map{case((valid,uop),addr)=>
        st_valid&&valid&&isOlder(st_rob_idx,uop.rob_idx,log2Ceil(ROBEntry))&&OffsetAlign(addr)===st_addr
    })
    val s0_isOlder = WireInit(VecInit.fill(numReadport)(false.B))

    val s1_isOlder = WireInit(VecInit.fill(numReadport)(false.B))
    val s2_isOlder = WireInit(VecInit.fill(numReadport)(false.B))

    val s0_Oldest  = PriorityEncoder(s0_isOlder zip s0_check_sels map{case(a,b)=>
        a&&b
    })
    val s1_Oldest  = PriorityEncoder(s1_isOlder zip s1_check_sels map{case(a,b)=>
        a&&b
    })
    
    val s2_Oldest  = PriorityEncoder(s2_isOlder zip s2_check_sels map{case(a,b)=>
        a&&b
    })
    s0_fail := s0_check_sels.reduce(_||_)
    s1_fail := s1_check_sels.reduce(_||_)
    s2_fail := s2_check_sels.reduce(_||_)

    val part0_Oldest = isOlder(s1_uop(s1_Oldest).rob_idx,s0_uop(s0_Oldest).rob_idx,log2Ceil(ROBEntry))
    val part1_Oldest = isOlder(ldq.io.check_resp.uop.rob_idx,s2_uop(s2_Oldest).rob_idx,log2Ceil(ROBEntry))
    val part0_fail   = s0_fail||s1_fail
    val part1_fail   = s2_fail||ldq.io.check_resp.redirect
    val part0_uop = Mux(s0_fail&&s1_fail,
            Mux(part0_Oldest,s1_uop(s1_Oldest),s0_uop(s0_Oldest)),
            Mux(s0_fail,s0_uop(s0_Oldest),
            Mux(s1_fail,s1_uop(s1_Oldest),0.U.asTypeOf(new MicroOp))))
    val part1_uop = Mux(s2_fail&&ldq.io.check_resp.redirect,
            Mux(part1_Oldest,ldq.io.check_resp.uop,s2_uop(s2_Oldest)),
            Mux(s2_fail,s2_uop(s2_Oldest),
            Mux(ldq.io.check_resp.redirect,ldq.io.check_resp.uop,0.U.asTypeOf(new MicroOp))))
    val final_Oldest = isOlder(part1_uop.rob_idx,part0_uop.rob_idx,log2Ceil(ROBEntry))
    val final_fail = part0_fail||part1_fail

    io.check_resp.redirect := final_fail
    io.check_resp.uop := Mux(part0_fail&&part1_fail,Mux(final_Oldest,part1_uop,part0_uop),
                    Mux(part0_fail,part0_uop,Mux(part1_fail,part1_uop,0.U.asTypeOf(new MicroOp))))
    // io.check_resp.redirect := false.B
    // io.check_resp.uop := 0.U.asTypeOf(new MicroOp)
    for(i <- 0 until numReadport){
        s0_isOlder(i) := VecInit((0 until i).foldLeft(true.B)((w,idx)=>
                isOlder(s0_uop(i).rob_idx,s0_uop(idx).rob_idx,log2Ceil(ROBEntry))
        )).reduce(_||_)
        s1_isOlder(i) := VecInit((0 until i).foldLeft(true.B)((w,idx)=>
                        isOlder(s1_uop(i).rob_idx,s1_uop(idx).rob_idx,log2Ceil(ROBEntry))
        )).reduce(_||_)
        s2_isOlder(i) := VecInit((0 until i).foldLeft(true.B)((w,idx)=>
                isOlder(s2_uop(i).rob_idx,s2_uop(idx).rob_idx,log2Ceil(ROBEntry))
        )).reduce(_||_)
    }
    
}