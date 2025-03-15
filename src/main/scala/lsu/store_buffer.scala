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
目前两项会出错，暂时没有找到原因
*/
class SBBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val dcache_write  = Flipped(new DCacheWriteIO)
    val sb_req        = Flipped(Decoupled(new STQCommit))
    val pipe_snoop    = new STQSnoopIO//是否可以提交
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
    dontTouch(store_buf)
    val numValid = PopCount(store_buf.map{buf=>buf.flag.valid})
    val full = RegNext(numValid+(io.sb_req.fire).asUInt===numSBs.U)
	val empty = numValid===0.U
    // val almost_full = numSBs.U-numValid<=1.U
    val almost_full = numSBs.U-numValid<=1.U
////////////////////////////enq logic   ///////////////////////////// 
    val do_enq = io.sb_req.fire
    val align_addr   = BankAlign(io.sb_req.bits.addr)
    val enq_bank         = io.sb_req.bits.addr(offsetWidth-1,bankWidth)

    
    val is_snoop_merge= store_buf.map{buf=>
        buf.addr===BankAlign(io.pipe_snoop.s2_addr)&&(buf.flag.valid)&&(!buf.flag.inflight)&&io.pipe_snoop.s2_valid
    }
    val is_enq_merge= store_buf.map{buf=>
        buf.addr===BankAlign(io.sb_req.bits.addr)&&(buf.flag.valid)&&(!buf.flag.inflight)&&io.sb_req.valid
    }
    io.pipe_snoop.s2_nack := (full&&(!is_snoop_merge.reduce(_||_)))&&io.pipe_snoop.s2_valid
    io.sb_req.ready := (!full)||(full&&is_enq_merge.reduce(_||_))
    val enq_merge_sels= VecInit(store_buf.map{buf=>
        buf.addr===BankAlign(io.sb_req.bits.addr)&&(buf.flag.valid)&&(!buf.flag.inflight)&&do_enq
    })//only 1
    val enq_merge_idx = PriorityEncoder(enq_merge_sels)
    // assert(do_enq&&(PopCount(enq_merge_sels)<=1.U),"allocate multi entry for the same addr")
    val enq_can_merge       = enq_merge_sels.reduce(_||_)
    val enq_allocate_sels   = PriorityEncoderOH(store_buf.map{buf=> (!buf.flag.valid)&&(!enq_can_merge)&&do_enq}) 
    val is_enq_allocate     = (!enq_can_merge)
    val is_same_block_inflight = VecInit(store_buf.map{buf=>
        buf.addr===BankAlign(io.sb_req.bits.addr)&&(buf.flag.valid)&&(buf.flag.inflight)
    }).reduce(_||_)

    val sb_data   = WireInit(store_buf(enq_merge_idx).data(enq_bank))
    val sb_mask   = WireInit(store_buf(enq_merge_idx).mask(enq_bank))
    val enq_data  = io.sb_req.bits.data
    val enq_mask  = io.sb_req.bits.mask
    val SplitData = Wire(Vec(XLEN/8,UInt(8.W)))
    dontTouch(sb_mask)
    dontTouch(enq_mask)

    for(i <- 0 until XLEN/8){
        SplitData(i) := Mux(enq_mask(i)===1.U,enq_data((i+1)*8-1,i*8),sb_data((i+1)*8-1,i*8))
    }

    dontTouch(do_enq)



//////////////////////////write to Dcache logic///////////////////////////// 
    
    val timeout_sels = store_buf.map{buf=>
        buf.flag.timeout&&buf.retry_cnt===timeOut
    }
    val timeout = timeout_sels.reduce(_||_)
    val timeout_idx = PriorityEncoder(timeout_sels)
    val alloc_ops= store_buf zip enq_merge_sels map{case(buf,sels)=>
        buf.flag.valid&&(!buf.flag.timeout)&&(!buf.flag.inflight)&&(!buf.flag.same_inflight)&&(!sels)
    }
    val alloc_sels = PriorityEncoderOH(alloc_ops)
    val alloc_idx = PriorityEncoder(alloc_sels)
    val do_deq      = io.dcache_write.req.fire

    dontTouch(almost_full)
    io.dcache_write.req.valid     := (!empty)&&(almost_full&&(alloc_ops.reduce(_||_))||timeout_sels.reduce(_||_))
    io.dcache_write.req.bits.addr := Mux(timeout,store_buf(timeout_idx).addr,
                                    store_buf(alloc_idx).addr)
    io.dcache_write.req.bits.data := Mux(timeout,store_buf(timeout_idx).data,
                                    store_buf(alloc_idx).data)
    io.dcache_write.req.bits.mask := Mux(timeout,store_buf(timeout_idx).mask,
                                    store_buf(alloc_idx).mask)  

    dontTouch(io.dcache_write.req.bits.mask)
    



//////////////////////////resp from Dcache logic/////////////////////////////
    val resp_valid = io.dcache_write.resp.valid
    val resp_hit   = io.dcache_write.resp.bits.hit
    val resp_addr  = io.dcache_write.resp.bits.addr
    val resp_success = resp_valid&&resp_hit
    val resp_replay  = resp_valid&&io.dcache_write.resp.bits.replay
    //only one
    val resp_sels  = WireInit(VecInit(store_buf.map{buf=>
        buf.flag.inflight&&buf.addr===resp_addr
    }))
    
    //no const
    val resp_sameblock_sels = WireInit(VecInit(store_buf.map{buf=>
        buf.flag.same_inflight&&buf.addr===resp_addr
    }))

//////////////////////////resp from Dcache logic/////////////////////////////
    val refill_valid = io.refillMsg.refilled
    val refill_addr  = io.refillMsg.refill_addr
    //only one 
    val refill_sels = VecInit(store_buf.map{buf=>
        buf.flag.inflight&&refill_addr===buf.addr&&refill_valid
    })
    val refill_sameblock_sels = store_buf.map{buf=>
        buf.flag.same_inflight&&buf.addr===resp_addr
    }
    val refill_idx = OHToUInt(refill_sels)
    val refill_same_idx = OHToUInt(refill_sameblock_sels)


//////////////////////////////store buf///////////////////////////////////
    dontTouch(refill_sels)
    dontTouch(refill_valid)


    for(i<- 0 until numSBs){
        

        //当写入的同时状态变为inflight，这时的写入信息会丢失，目前做法是当有merge信号时，不许发出写入dcache请求
        val resp_op     = (resp_sels(i)&&resp_success)
        val resp_replay_op = resp_sels(i)&resp_replay
        val refill_op   = (refill_valid&&refill_sels(i))
        val enq_allocate_valid = enq_allocate_sels(i)
        val enq_merge_valid    = enq_merge_sels(i)
        //data
        store_buf(i).addr           := Mux(enq_allocate_valid,align_addr,store_buf(i).addr)
        store_buf(i).data(enq_bank) := Mux(enq_allocate_valid,enq_data,
                                        Mux(enq_merge_valid,Cat(SplitData.map(_.asUInt).reverse),store_buf(i).data(enq_bank)))
        //ctrl
        store_buf(i).retry_cnt          := Mux(store_buf(i).flag.timeout,store_buf(i).retry_cnt+1.U,0.U)
        store_buf(i).flag.inflight      := Mux((resp_op||resp_replay_op)||refill_op,false.B,
                                            Mux(timeout_sels(i)&&timeout&&do_deq,true.B,
                                            Mux(alloc_sels(i)&&(!timeout)&&do_deq,true.B,store_buf(i).flag.inflight ))) 
        
        store_buf(i).flag.valid         := Mux(resp_op||refill_op,false.B,
                                            Mux(enq_allocate_valid,true.B,store_buf(i).flag.valid ))
        for(j <- 0 until bankNum){
            val is_enq_bank = enq_bank===j.U
            store_buf(i).mask(j) := Mux(resp_sels(i)&&resp_success||refill_valid&&refill_sels(i),0.U,
            Mux(enq_allocate_valid&&is_enq_bank,enq_mask,
            Mux(enq_merge_valid&&is_enq_bank,enq_mask|sb_mask,store_buf(i).mask(j))))
        }
        store_buf(i).flag.timeout       := Mux(resp_replay_op,true.B ,
                                        Mux(resp_op||refill_op||store_buf(i).retry_cnt===timeOut,false.B,store_buf(i).flag.timeout)) 

        store_buf(i).flag.same_inflight := Mux((resp_sels(i)||resp_sameblock_sels(i))&&resp_success||refill_op,
                                        false.B,
                                        Mux(is_same_block_inflight&&enq_allocate_valid,true.B,store_buf(i).flag.same_inflight))  
    }

//////////////////////////forward         logic/////////////////////////////
    val forward_valid = io.search_req.addr.map(_.valid)
    val forward_addr  = io.search_req.addr.map{i=>BankAlign((i.bits))}
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
        io.search_resp(i).bits.ldq_idx := 0.U

    }


}