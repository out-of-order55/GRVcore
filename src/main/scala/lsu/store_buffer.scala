package grvcore
import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
/* 
当指令提交写入store buffer（类似于小的Cache）
如果写入请求在store buffer找到地址，直接合并，如果没找到地址，直接分配新的表项
*/
class SBBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dcache_write  = Flipped(new DCacheWriteIO)
    val sb_req        = Flipped(Decoupled(new STQCommit))
    val search_req    = Flipped(new LDQSearchReq)
    val search_resp   = Vec(numReadport,(Valid(new LDQSearchResp)))
    // val commit        = Input(new CommitMsg)
    val refillMsg     = Input(new RefillMsg())

    val flush         = Input(Bool())
}
class StoreBuffer(implicit p: Parameters) extends GRVModule with HasDCacheParameters 
{
    val io = IO(new SBBundle)
    val sbSz = log2Ceil(numSBs)
    val timeOut = 15.U
    class SBFlag extends Bundle{
        val valid = Bool()
        val inflight = Bool()
        val timeout  = Bool()
        val same_inflight  = Bool()
    }
    class SBEntry extends Bundle{
        val flag      = new SBFlag
        val retry_cnt = UInt(4.W)
        // val uop   = new MicroOp//冗余部分，其实用不倒那末多

        val data = Vec(bankNum,UInt(XLEN.W))
        val addr = UInt(XLEN.W)//aligned addr
        val mask = Vec(bankNum,UInt((XLEN/8).W))
    }
    // val s_idle::s_valid::s_inflight ::s_sameblock_inflight::s_wait_timeout::Nil = Enum(3)

    val store_buf = RegInit(VecInit.fill(numSBs)(0.U.asTypeOf(new SBEntry)))
    val numValid = PopCount(store_buf.map{buf=>buf.flag.valid})
    val full = RegNext(numValid+(io.sb_req.fire).asUInt===numSBs.U)
	val empty = numValid===0.U
    val almost_full = numValid+1.U===numSBs.U
////////////////////////////enq logic   ///////////////////////////// 
    val do_enq = io.sb_req.fire
    val align_addr   = BankAlign(io.sb_req.bits.addr)
    val bank         = io.sb_req.bits.addr(offsetWidth-1,bankWidth-1)

    io.sb_req.ready := (!full)
    val checkOH = store_buf.map{buf=>
        buf.addr===io.sb_req.bits.addr&&(buf.flag.valid)
    }
    val check_idx = OHToUInt(checkOH)//only 1
    val sb_data   = store_buf(check_idx).data(bank)
    val sb_mask   = store_buf(check_idx).mask(bank)
    val enq_data  = io.sb_req.bits.data
    val enq_mask  = io.sb_req.bits.mask
    val enq_sels  = store_buf.map{buf=> (!buf.flag.valid)}
    val enq_idx   = PriorityEncoder(enq_sels)
    val SplitData = Wire(Vec(XLEN/8,UInt(8.W)))


    for(i <- 0 until XLEN/8){
        SplitData(i) := Mux(enq_mask(i)===1.U,enq_data((i+1)*8-1,i*8),
                        Mux(sb_mask(i)===1.U,sb_data((i+1)*8-1,i*8),0.U))
    }
    val is_has_same_entry = store_buf(check_idx).flag.inflight
    when(do_enq){
        when(checkOH.reduce(_||_)){//merge entry
            when(is_has_same_entry){
                store_buf(enq_idx).addr      := align_addr
                store_buf(enq_idx).data(bank):= enq_data
                store_buf(enq_idx).mask(bank):= enq_mask
                store_buf(enq_idx).flag.same_inflight     := true.B

            }.otherwise{
                store_buf(check_idx).data(bank) := Cat(SplitData.map(_.asUInt).reverse)
                store_buf(check_idx).mask(bank) := sb_mask|enq_mask
            }
            // for()
        }.otherwise{//new entry
            store_buf(enq_idx).addr      := align_addr
            store_buf(enq_idx).data(bank):= enq_data
            store_buf(enq_idx).mask(bank):= enq_mask
        }
    }


//////////////////////////write to Dcache logic///////////////////////////// 
    
    val timeout_sels = store_buf.map{buf=>
        buf.flag.timeout&&buf.retry_cnt===timeOut
    }
    val timeout = timeout_sels.reduce(_||_)
    val timeout_idx = PriorityEncoder(timeout_sels)
    val alloc_sels= store_buf.map{buf=>
        buf.flag.valid&&(!buf.flag.timeout)
    }
    val alloc_idx = PriorityEncoder(alloc_sels)
    val do_deq      = io.dcache_write.req.fire


    io.dcache_write.req.valid     := (!empty)&&(almost_full||timeout_sels.reduce(_||_))
    io.dcache_write.req.bits.addr := Mux(timeout,store_buf(timeout_idx).addr,
                                    store_buf(alloc_idx).addr)
    io.dcache_write.req.bits.data := Mux(timeout,store_buf(timeout_idx).data,
                                    store_buf(alloc_idx).data)
    io.dcache_write.req.bits.mask := Mux(timeout,store_buf(timeout_idx).mask,
                                    store_buf(alloc_idx).mask)  


    



//////////////////////////resp from Dcache logic/////////////////////////////
    val resp_valid = io.dcache_write.resp.valid
    val resp_hit   = io.dcache_write.resp.bits.hit
    val resp_addr  = io.dcache_write.resp.bits.addr
    val resp_success = resp_valid&&resp_hit
    val resp_replay = resp_valid&&io.dcache_write.resp.bits.replay
    //only one
    val resp_sels  = store_buf.map{buf=>
        buf.flag.inflight&&buf.addr===resp_addr
    }
    //no const
    val resp_sameblock_sels = store_buf.map{buf=>
        buf.flag.same_inflight&&buf.addr===resp_addr
    }

//////////////////////////resp from Dcache logic/////////////////////////////
    val refill_valid = io.refillMsg.refilled
    val refill_addr  = io.refillMsg.refill_addr
    //only one 
    val refill_sels = store_buf.map{buf=>
        buf.flag.inflight&&refill_addr===buf.addr&&refill_valid
    }
    val refill_sameblock_sels = store_buf.map{buf=>
        buf.flag.same_inflight&&buf.addr===resp_addr
    }
    val refill_idx = OHToUInt(refill_sels)
    val refill_same_idx = OHToUInt(refill_sameblock_sels)


//////////////////////////////store buf///////////////////////////////////
    for(i<- 0 until numSBs){
        store_buf(i).retry_cnt          := Mux(store_buf(i).flag.timeout,store_buf(i).retry_cnt+1.U,0.U)
        store_buf(i).flag.inflight      := Mux(resp_sels(i)&&resp_success||refill_valid&&refill_sels.reduce(_||_),false.B,
                                            Mux(timeout_sels(i)&&timeout&&do_deq,true.B,
                                            Mux(alloc_sels(i)&&(!timeout)&&do_deq,true.B,store_buf(alloc_idx).flag.inflight ))) 
        
        store_buf(i).flag.valid         := Mux(resp_sels(i)&&resp_success||refill_valid&&refill_sels.reduce(_||_),false.B,
                                            Mux(do_enq&&enq_sels(i),true.B,store_buf(i).flag.valid ))
        
        store_buf(i).flag.timeout       := Mux(resp_sels(i)&&resp_replay ,true.B ,
                                        Mux(resp_sels(i)&&resp_success||refill_valid&&refill_sels.reduce(_||_),false.B,store_buf(i).flag.timeout)) 

        store_buf(i).flag.same_inflight := Mux((resp_sels(i)||resp_sameblock_sels(i))&&resp_success||refill_valid&&refill_sels.reduce(_||_),
                                        false.B,
                                        Mux(do_enq&&checkOH.reduce(_||_)&&is_has_same_entry&&enq_sels(i),true.B,store_buf(i).flag.same_inflight))  
    }

//////////////////////////forward         logic/////////////////////////////
    val forward_valid = io.search_req.addr.map(_.valid)
    val forward_addr  = io.search_req.addr.map(_.bits)
    val forward_bank  = io.search_req.addr.map(_.bits(offsetWidth-1,0)>>2)
    for(i <- 0 until numReadport){
        //可能遇到一个在flight 另一个在wait_sameblock
        val forward_sels_inflight     = store_buf.map{buf=> buf.addr===forward_addr(i)&&buf.flag.inflight&&forward_valid(i)}
        val forward_sels_wait_same  = store_buf.map{buf=> buf.addr===forward_addr(i)&&buf.flag.same_inflight&&forward_valid(i)}
        val forward_sels_valid      = store_buf.map{buf=> buf.addr===forward_addr(i)&&
                                (buf.flag.valid)&&forward_valid(i)}
        val forward_inflight_idx = OHToUInt(forward_sels_inflight) 
        val forward_wait_same_idx= OHToUInt(forward_sels_wait_same)
        val forward_valid_idx    = OHToUInt(forward_sels_valid)   
        val forward_has_same_entry = forward_sels_inflight.reduce(_||_)&forward_sels_wait_same.reduce(_||_)
        val inflight_data = store_buf(forward_inflight_idx).data(forward_bank(i))
        val inflight_mask = store_buf(forward_inflight_idx).mask(forward_bank(i))
        val wait_same_data= store_buf(forward_wait_same_idx).data(forward_bank(i))
        val wait_same_mask= store_buf(forward_wait_same_idx).mask(forward_bank(i))
        val valid_data    = store_buf(forward_valid_idx).data(forward_bank(i))
        val valid_mask    = store_buf(forward_valid_idx).mask(forward_bank(i))
	    val final_data    = Wire(Vec(XLEN/8,UInt(8.W)))

	
        //32 to 8

        for(j<- 0 until XLEN/8){
            final_data(j) := Mux(wait_same_mask(j)===1.U,wait_same_data((j+1)*8-1,j*8),inflight_data((j+1)*8-1,j*8))
        }

        io.search_resp(i).valid    := forward_sels_valid.reduce(_||_)
        io.search_resp(i).bits.data:= Mux(forward_has_same_entry,Cat(final_data.map(_.asUInt).reverse),valid_data)
        io.search_resp(i).bits.mask:= Mux(forward_has_same_entry,inflight_mask|wait_same_mask,valid_mask)
        io.search_resp(i).bits.uop := 0.U.asTypeOf(new MicroOp)

    }


}