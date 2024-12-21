package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
/* 
ROB实现的功能：
1.接受来自dispatch给出的寄存器旧映射关系，以及入队的Microop，然后返回ROB_idx

2.提交时更新处理器状态，通知FTQ，更新RAS，RAT，（ghist）
3.如果在执行发生异常，会发出锁存信号，并且，之后的指令执行实际无效，然后当ROB等到指令变为最旧的指令，将流水线冲刷，从该指令开始取指令
3.目前采用的是boom的提交方案，也就是只有一个corewidth所有的指令都执行完毕了，这时候才可以对指令进行提交，还有就是香山或者玄铁的方案，每个周期退休尽量多的指令
这时候一种设计方法就是FTQ和指令记录mask数值，提交时对比值，只有mask全部为1 FTQ才可以退队，注意这里不包含无效指令，
目前ROB会存储分支更新信息，在提交阶段送出

 */
class CommitMsg(implicit p: Parameters) extends GRVBundle{
    val valid       = Vec(coreWidth,Bool())
    val commit_uops = Vec(coreWidth,new MicroOp)
}
/* 
 */
class CommitExcMsg(implicit p: Parameters) extends GRVBundle{

    val ftq_idx    = UInt(log2Ceil(ftqentries).W)
    val pc_lob     = UInt(log2Ceil(blockBytes).W)
    val cause      = UInt(32.W)
    val epc        = UInt(XLEN.W)//for mispred
    val flush_typ  = UInt(FLUSH_SZ.W)
}
class robEnqueue (implicit p: Parameters) extends GRVBundle{
    val uops  = Vec(coreWidth,new MicroOp)
    val valid = Vec(coreWidth,Bool())
}
class Exception(implicit p: Parameters) extends GRVBundle{
    val uops  = (new MicroOp)
    val cause = UInt(32.W)
}
class ROBIO(implicit p: Parameters) extends GRVBundle{

    val enq         = Flipped(Decoupled(new robEnqueue))
    val enq_idxs    = Output(Vec(coreWidth,UInt(log2Ceil(ROBEntry).W)))
    val wb_resp     = Flipped(MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, Valid(new ExuResp)))))

    val lsuexc      = Input(new Exception)
    val br_info = Input(new BrUpdateInfo)


    val br_update   = Output(new BrUpdateInfo)
    val commit      = Valid(new CommitMsg)
    val flush       = Valid(new CommitExcMsg)
}
/*

*/
class ROBEntryBundle(implicit p: Parameters)extends Bundle{
    val valid   = Bool()
    val finish  = Bool()
    val flush   = Bool()
    val uop     = new MicroOp//目前没有优化逻辑，所以仍然使用的这个
}
class ROB(implicit p: Parameters) extends GRVModule{
    val io = IO(new ROBIO)
    val RobSz = log2Ceil(ROBEntry)
    /* 
    ROB状态机 
    idle    ：rob初始状态， 开机之后如果不reset就不会进入，
    normal  ：rob正常状态
    redirect：rob进入重定向状态，根据输入的信号转换：主要有：
    来自EXU阶段的JMP UNIT，来自前端的异常信息，比如无效指令，非对齐等，以及执行阶段的load/store违例
     */
    val s_idle::s_normal::s_wait_repair::s_redirect::Nil = Enum(4)
    val rob_state = RegInit(s_idle)

    val rob_enq_ptr = RegInit(0.U(log2Ceil(ROBEntry+1).W))
    val rob_deq_ptr = RegInit(0.U(log2Ceil(ROBEntry+1).W))
    val full = Wire(Bool())
    
    val do_enq = Wire(Bool())
    // val do_deq = Wire(Bool())

    

    

    def GetRowIdx(rob_idx: UInt): UInt = {
        if (coreWidth == 1) return rob_idx
        else return (rob_idx >> log2Ceil(coreWidth))(log2Ceil(ROBEntry)-1,0)
    }
    def GetBankIdx(rob_idx: UInt): UInt = {
        if(coreWidth == 1) { return 0.U }
        else           { return rob_idx(log2Ceil(coreWidth)-1, 0).asUInt }
    }
    def GetPtrMask(ptr:UInt):UInt={
        return ptr(RobSz)
    }
    def GetPtrVal(ptr:UInt):UInt={
        return ptr(RobSz-1,0)
    }
    val rob_entry   = RegInit(VecInit.fill(coreWidth)(VecInit.fill(ROBEntry)(0.U.asTypeOf(new ROBEntryBundle))))
    
    val rob_enq_val = GetPtrVal(rob_enq_ptr)
    val rob_deq_val = GetPtrVal(rob_deq_ptr)
    val rob_enq_mask= GetPtrMask(rob_enq_ptr)
    val rob_deq_mask= GetPtrMask(rob_deq_ptr)
    //enq
    val enqInfo     = WireInit(io.enq)
    val br_info     = WireInit(io.br_info)
    val flush       = WireInit(br_info.cfi_mispredicted)
    val is_exc_inst_commit = WireInit(false.B)
    val br_update   = RegInit(0.U.asTypeOf(new BrUpdateInfo))
    do_enq := io.enq.fire&&(!rob_state===s_redirect)
    val deq_vld_mask    = WireInit(UInt(log2Ceil(blockBytes).W),(VecInit(rob_entry.map{i=> i(rob_deq_val).valid}).asUInt))
    val deq_finish_mask = WireInit(UInt(log2Ceil(blockBytes).W),(VecInit(rob_entry.map{i=>i(rob_deq_val).finish}).asUInt))
    /* 
    可以提交的情况：
    1.无异常，此时只要finfish的个数等于vld的个数，就说明可以提交
    2.发生异常，此时只要该异常指令变为最旧的指令，此时就可以进行提交
     */
    val is_commit_flush = WireInit(MaskLower(VecInit(rob_entry.map{i=>i(rob_deq_val).finish&&i(rob_deq_val).flush}).asUInt))
    val is_exc_oldest   = Wire(Bool())
    is_exc_oldest := ((is_commit_flush)&(deq_finish_mask))===is_commit_flush
    dontTouch(is_exc_oldest)
    val can_commit      = ((((deq_vld_mask)===(deq_finish_mask)))||is_exc_oldest)&&(deq_vld_mask=/=0.U)

    
    full := (rob_enq_val===rob_deq_val)&&(rob_enq_mask=/=rob_deq_mask)
    dontTouch(full)
    io.enq.ready := (!full)&&(rob_state===s_normal)
    io.commit:= DontCare
    io.commit.valid := false.B
    io.enq_idxs:= DontCare
    io.flush := DontCare
    io.flush.valid := false.B
    for(i <- 0 until coreWidth){

        //enq
        when(do_enq){
            rob_entry(i)(rob_enq_val).valid := enqInfo.bits.valid(i)
            rob_entry(i)(rob_enq_val).uop   := enqInfo.bits.uops(i)
            io.enq_idxs(i) := rob_enq_val + i.U
        }
        
        //deq
        when(can_commit){
            rob_entry(i)(rob_deq_val).valid:=false.B
            io.commit.valid := true.B
            io.commit.bits.valid(i) := rob_entry(i)(rob_deq_val).valid
            io.commit.bits.commit_uops(i):=rob_entry(i)(rob_deq_val).uop
            //由于采用提交处理各种异常，所以commit的信号内包含的信号flush不用再次包含
        }
        /* 
        exc
        此时处理来自执行单元的异常，并且更新uop，以便之后br_update
         */
        val br_miss_bank = GetBankIdx((br_info.uop.rob_idx))
        val br_miss_row  = GetRowIdx((br_info.uop.rob_idx))
        when(br_info.cfi_mispredicted){
            rob_entry(br_miss_bank)(br_miss_row).flush := true.B
            rob_entry(br_miss_bank)(br_miss_row).uop   := br_info.uop
        }
    }
    for(i <- 0 until issueParams.size){
        for(j <- 0 until coreWidth){
            val wb_rob_bank = GetBankIdx(io.wb_resp(i)(j).bits.uops.rob_idx)
            val wb_rob_idx  = GetRowIdx(io.wb_resp(i)(j).bits.uops.rob_idx)
            when(io.wb_resp(i)(j).valid){
                rob_entry(wb_rob_bank)(wb_rob_idx).finish := true.B
            }
        }
    }
    //指针更新逻辑
    when(do_enq){
        rob_enq_ptr := rob_enq_ptr + 1.U
    }
    when(can_commit){
        rob_deq_ptr := rob_deq_ptr + 1.U
    }
    //只有在提交阶段才会更新br——update，并且只有这时候才会去处理update信息，
    when(rob_state===s_normal){
        br_update := br_info
    }
    io.br_update := br_update
    //exc handle

    when(is_exc_oldest){
        io.flush.valid := true.B
        io.flush.bits.cause := 0.U
        io.flush.bits.flush_typ := 0.U
    }
    //两个周期的处理时间，
    //FSM
	switch (rob_state){
		is(s_idle){
			rob_state := s_normal
		}
        is(s_normal){
            when(flush){
                rob_state := s_wait_repair
            }
        }
        is(s_wait_repair){
            when(is_exc_oldest){
                rob_state := s_redirect
            }
        }
        is(s_redirect){
            rob_state := s_normal
        }
    }


}