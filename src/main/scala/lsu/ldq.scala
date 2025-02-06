package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config._
/* 
LDQ流水线
stage0
计算地址，写入LDQ，向DCache发送访存请求
stage1
DCache对比tag，地址送到STQ和store buffer，得到数据,
stage2
DCache返回命中信息，如果命中，将从STQ store buffer 和DCache选择数据（STQ优先级最高），写回数据
如果发生miss，会将miss信息送入issue unit，重新发送数据，DCache分配MSHR，LDQ监听数据


1.在dispatch写入
2.当load指令得到hit信息，更新LDQ，当指令提交时，释放表项
3.如果load指令发生miss，需要进行replay（不需要冲刷流水线：因为他miss，不能对其issue unit的指令进行wake up）
miss期间，LDQ需要侦听DCache的refill数据，如果数据到来，写入LDQ，然后准备写入rf，如果发生端口冲突，阻塞LDQ

LDQ需要存：
1.地址
2.数据
3.状态：
idle：初始状态
valid：disptach写入
miss：当发生cache_miss

需要设置的端口
1.来自dispatch的端口
2.来自LD流水线的端口
3.来自commit的端口
4.来自DCache的refill数据端口
5.写回端口


可能发生的异常：
1.bank冲突：进行replay
2.sw/ld违例：进行redirect

当发生miss，ldq发出replay信号，对issueunit的进行replay，再次执行，直到当miss&&（datvalid）(弃用)
目前准备miss，ldq侦听，等到refill就可以进行写回了，会和LD流水线争用端口

目前的面积很大40000
面积根据LDQ项变化几乎为线性的（1，2，4，8）
主要原因就是存储的uop太大，之后在完成设计后，会对这部分存储的进行裁剪
 */
class LDQPipeIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val s0_addr        = Input(Vec(numReadport,UInt(XLEN.W)))
    val s0_uop         = Vec(numReadport,Flipped(Valid(new MicroOp)))
    val s2_uop         = Input(Vec(numReadport,new MicroOp))
    val s2_wb_req      = Input(Vec(numReadport,Bool()))
    val s2_miss        = Input(Vec(numReadport,Bool()))
}
// class LDQDisIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
//     val enq     = Vec(coreWidth,Flipped(Decoupled(new MicroOp)))//dispatch
//     val enq_idx = (Vec(coreWidth,Valid(UInt(log2Ceil(numLDQs).W))))//dispatch
// }
/* 这里设置mask是为了简便forward逻辑，
只需要对mask为1的区域进行forward检查，
如果没有mask，首先要检查addr相等的，其次还要找出最近的一个比ld旧的store
 */

class LDQSearchReq(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val addr        = Vec(numReadport,Valid(UInt(XLEN.W)))//addr是流水线发出的
    val stq_idx     = Vec(numReadport,UInt(log2Ceil(numSTQs+1).W))
}
class LDQSearchResp(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    // val addr    = Vec(numReadport,Valid(UInt(XLEN.W)))//addr是流水线发出的
    // val 
    val data    = UInt(XLEN.W)
    val mask    = UInt((XLEN/8).W)
    val uop     = new MicroOp
}
class LDQResp(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val data    = Vec(numReadport,UInt(XLEN.W))
    
    val uop     = Vec(numReadport,Flipped(Valid(new MicroOp)))
}
class LDQBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{


    val dis     = new DisIO//写入
    val pipe    = new LDQPipeIO//写入地址，更新状态

    val check_unorder = Flipped(Valid(new CheckRAWReq)) 
    val check_resp  = Output(new CheckRAWResp)

    val refillMsg= Input(new RefillMsg())
    val wb_resp = Vec(numReadport,Decoupled(new ExuResp))//会和LD流水线冲突

    val search_resp  =Vec(numReadport,Flipped(Valid(new LDQSearchResp)))//data
    val commit  = Input(new CommitMsg)
    // val resp    = new LDQResp
    val flush   = Input(Bool())
    val s1_kill = Input(Bool())
    val s2_kill = Input(Bool())
}
class LDQ(implicit p: Parameters) extends GRVModule with HasDCacheParameters
with freechips.rocketchip.rocket.constants.MemoryOpConstants  
{
    val io = IO(new LDQBundle)
    val ldqSz = log2Ceil(numLDQs)
    class LDQFlag extends Bundle{
        val allocated = Bool()
        val datavalid = Bool()//used for refilled
        val wb_valid  = Bool()//already write back
        val miss      = Bool()//
    }
    class LDQEntry extends Bundle{
        val flag = new LDQFlag
        val uop   = new MicroOp//冗余部分，其实用不倒那末多

        val data = UInt(XLEN.W)
        val addr = UInt(XLEN.W)
        val mask = UInt((XLEN/8).W)
    }
    // val s_idle::s_allocated::s_datavalid::s_miss :: Nil = Enum(5)

    val ldq = RegInit(VecInit.fill(numLDQs)(0.U.asTypeOf(new LDQEntry)))
    val ldq_enq_ptr  = RegInit(0.U(log2Ceil(numLDQs+1).W))
    val ldq_deq_ptr  = RegInit(0.U(log2Ceil(numLDQs+1).W))
    val numValid = PopCount(ldq.map(_.flag.allocated))
    val numEnq = PopCount(io.dis.enq.map{i=>i.valid&&i.bits.mem_cmd===M_XRD})

    val deq  = VecInit((0 until coreWidth).map{i=>
        io.commit.valid(i)&&io.commit.commit_uops(i).mem_cmd===M_XRD&&io.commit.commit_uops(i).uses_ldq
    })

    val numDeq = PopCount(deq)
    
    val enqNextPtr = ldq_enq_ptr+numEnq
	val deqNextPtr = ldq_deq_ptr+numDeq
    val full = (ldq_enq_ptr(ldqSz)=/=ldq_deq_ptr(ldqSz)&&(numEnq+ldq_enq_ptr)(ldqSz-1,0)>ldq_deq_ptr(ldqSz-1,0))||
    numValid===numLDQs.U
	val empty = (ldq_enq_ptr(ldqSz)===ldq_deq_ptr(ldqSz))&&ldq_enq_ptr(ldqSz-1,0)===ldq_deq_ptr(ldqSz-1,0)
    
    io.dis.enq.foreach{dis=>
        dis.ready := (!full)&(!io.flush)
    }
    when(io.flush){
        ldq_enq_ptr := 0.U
    }
    .elsewhen(numEnq.orR){
        ldq_enq_ptr := ldq_enq_ptr + numEnq
    }
    val enq_idxs = VecInit.tabulate(coreWidth)(i => PopCount(io.dis.enq.map{i=>i.fire&&i.bits.mem_cmd===M_XRD}.take(i)))
    val enq_offset = VecInit(enq_idxs.map(_+ldq_enq_ptr(ldqSz-1,0)))
    // dontTouch(enq_idxs)
    // dontTouch(enq_offset)

    for(i <- 0 until coreWidth){
        val do_enq = io.dis.enq(i).fire&&io.dis.enq(i).bits.mem_cmd===M_XRD
        for(j <- 0 until numLDQs){
            when(do_enq&&enq_offset(i)===j.U){
                ldq(j).flag.allocated := true.B
                // ldq(j).uop := io.dis.enq(i).bits
            }
        }
        io.dis.enq_idx(i).bits := enq_offset(i)
        io.dis.enq_idx(i).valid:= do_enq
    }

    //stage0 write addr
    val s0_waddr    = io.pipe.s0_addr
    val s0_valid    = io.pipe.s0_uop.map(_.valid)
    val s0_idx      = io.pipe.s0_uop.map(_.bits.ldq_idx)
    dontTouch(VecInit(s0_idx))
    dontTouch(s0_waddr)
    for(i <- 0 until numReadport){
        when(s0_valid(i)){
            ldq(s0_idx(i)).uop := io.pipe.s0_uop(i).bits
            ldq(s0_idx(i)).addr := s0_waddr(i)
        }
    }

    //stage 2 update state
    val s2_valid    = io.pipe.s2_wb_req//not miss
    val s2_idx      = io.pipe.s2_uop.map(_.ldq_idx)
    val s2_miss     = io.pipe.s2_miss
    for(i <- 0 until numReadport){
        ldq(s2_idx(i)).flag.datavalid := Mux(s2_valid(i),true.B,ldq(s2_idx(i)).flag.datavalid)
        ldq(s2_idx(i)).flag.wb_valid  := Mux(s2_valid(i),true.B,ldq(s2_idx(i)).flag.wb_valid)
        ldq(s2_idx(i)).flag.miss      := Mux(s2_miss(i),true.B,ldq(s2_idx(i)).flag.miss)
    }

    //forward data

    val bypassMsg =WireInit(io.search_resp)

    for(i <- 0 until numReadport){

        val bypass_en = bypassMsg(i).valid
        val idx = bypassMsg(i).bits.uop.ldq_idx
        ldq(idx).data := Mux(bypass_en,bypassMsg(i).bits.data,ldq(idx).data)
        ldq(idx).mask := Mux(bypass_en,bypassMsg(i).bits.mask,ldq(idx).mask)
    }


//////////////////////refill//////////////////////
    val refill_sels = ldq.map{ldq=>
        BankAlign(ldq.addr)===io.refillMsg.refill_addr&&io.refillMsg.refilled
    }
    
    //refill的数据mask总是全为1


							
    for(i<- 0 until numLDQs){
        val refill_bank =  (ldq(i).addr)(offsetWidth-1,bankWidth)
        val refill_data = io.refillMsg.refillData(refill_bank)
        val final_data  = Wire(UInt(XLEN.W))
        val ldq_data = ldq(i).data
        val ldq_mask = ldq(i).mask

        val splitData = Wire(Vec(XLEN/8,UInt(8.W)))
        //32 to 8
        
        for(i<- 0 until XLEN/8){
            splitData(i) := Mux(ldq_mask(i)===1.U,ldq_data((i+1)*8-1,i*8),refill_data((i+1)*8-1,i*8)) 
        }
        final_data := Cat(splitData.map(_.asUInt).reverse)
        when(refill_sels(i)&&(ldq(i).flag.allocated)){
            ldq(i).flag.miss := false.B
            ldq(i).flag.datavalid :=true.B
            ldq(i).data := final_data
        }
    }

////////////////////////////////////////

///////////////////wb//////////////////
    val wb_sels_resp  = VecInit(ldq.map{i=>i.flag.datavalid&(!i.flag.wb_valid)&(i.flag.allocated)})//只有datavalid&（！wbvalid）才说明这个是refill的load，正常的都是datavalid（wbvalid）同时拉高
    val wb_selOH = SelectFirstN(wb_sels_resp.asUInt,numReadport) 

    for(i <- 0 until numReadport){
        val wb_idx = PriorityEncoder(wb_selOH(i))
        val wb_valid = io.wb_resp(i).fire
        val wb_offset = ldq(wb_idx).addr(log2Ceil(XLEN/8)-1,0)
        val wb_data = ldq(wb_idx).data>>(8.U*wb_offset)
        val wb_sels = WireInit(Cat(ldq(wb_idx).uop.mem_signed.asUInt,ldq(wb_idx).uop.mem_size))
        dontTouch(wb_sels)

        io.wb_resp(i).valid    := ldq(wb_idx).flag.datavalid
        io.wb_resp(i).bits.uop := ldq(wb_idx).uop
        io.wb_resp(i).bits.wb_data := newRdataHelper(wb_sels,wb_data)
        ldq(wb_idx).flag.wb_valid := Mux(wb_valid,true.B,ldq(wb_idx).flag.wb_valid)
        ldq(wb_idx).flag.datavalid := Mux(wb_valid,false.B,ldq(wb_idx).flag.datavalid)
    }
////////////////commit/////////////////
    for(i <- 0 until coreWidth){
        val commit_idx = io.commit.commit_uops(i).ldq_idx
        when(deq(i)){
            ldq(commit_idx) := 0.U.asTypeOf(new LDQEntry) 
        }
    }
    when(io.flush){
        ldq_deq_ptr := 0.U
    }
    .elsewhen(deq.reduce(_||_)){
        ldq_deq_ptr := ldq_deq_ptr + numDeq
    }
////////////////check/////////////////
    val st_rob_idx = io.check_unorder.bits.uop.rob_idx
    val checkSels = VecInit(ldq.map{i=>
            
            val isOlder = st_rob_idx(log2Ceil(ROBEntry))===i.uop.rob_idx(log2Ceil(ROBEntry))&&
                        st_rob_idx(log2Ceil(ROBEntry)-1,0)<i.uop.rob_idx(log2Ceil(ROBEntry)-1,0)||
                        st_rob_idx(log2Ceil(ROBEntry))=/=i.uop.rob_idx(log2Ceil(ROBEntry))&&
                        st_rob_idx(log2Ceil(ROBEntry)-1,0)>i.uop.rob_idx(log2Ceil(ROBEntry)-1,0)
            i.addr===io.check_unorder.bits.check_addr&&io.check_unorder.valid&&i.flag.wb_valid&&isOlder
        }) 
    val unorder_idx = PriorityEncoder(checkSels)
    io.check_resp.redirect := checkSels.reduce(_||_)
    io.check_resp.uop      := ldq(unorder_idx).uop
//////////////////////////flush/////////////////
    when(io.flush){
        for(i <- 0 until numLDQs){
            ldq(i).flag := 0.U.asTypeOf(new LDQFlag)
        }
    }
}