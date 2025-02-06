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
    val rob_idx    = UInt(log2Ceil(ROBEntry+1).W)
    val pc_lob     = UInt(log2Ceil(ICacheParam.blockBytes).W)
    val cause      = UInt(32.W)
    val epc        = UInt(XLEN.W)//for mispred
    val flush_typ  = UInt(FLUSH_SZ.W)
}
class robEnqueue (implicit p: Parameters) extends GRVBundle{
    val uops  = Vec(coreWidth,Decoupled(new MicroOp))
    // val valid = Vec(coreWidth,Bool())
}
class Exception(implicit p: Parameters) extends GRVBundle{
    val uops  = (new MicroOp)
    val cause = UInt(32.W)
}
class ROBIO(val numWakeupPorts:Int)(implicit p: Parameters) extends GRVBundle{

    val enq         = Flipped(new robEnqueue)
    val enq_idxs    = Output(Vec(coreWidth,Valid(UInt(log2Ceil(ROBEntry+1).W))))
    val wb_resp     = Flipped(Vec(numWakeupPorts, Valid(new ExuResp)))

    val lsu_exc     = Flipped(Valid(new Exception))
    val br_info     = Flipped(Valid(new BrUpdateInfo))


    val br_update   = Valid(new BrUpdateInfo)
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
class ROB(val numWakeupPorts:Int)(implicit p: Parameters) extends GRVModule{
    val io = IO(new ROBIO(numWakeupPorts))
    val RobSz = log2Ceil(ROBEntry)-1
    val RobbankSz = log2Ceil((ROBEntry/coreWidth)+1)
    /* 
    ROB状态机 
    idle    ：rob初始状态， 开机之后如果不reset就不会进入，
    normal  ：rob正常状态
    redirect：rob进入重定向状态，根据输入的信号转换：主要有：
    来自EXU阶段的JMP UNIT，来自前端的异常信息，比如无效指令，非对齐等，以及执行阶段的load/store违例
     */
    val s_idle::s_normal::s_wait_repair::s_redirect::Nil = Enum(4)
    val rob_state = RegInit(s_idle)

    val rob_enq_ptr = RegInit(0.U(RobbankSz.W))
    val rob_deq_ptr = RegInit(0.U(RobbankSz.W))
    val full = Wire(Bool())
    
    val do_enq = WireInit(VecInit(io.enq.uops.map{i=>i.fire&&(rob_state=/=s_redirect)}))
    // val do_deq = Wire(Bool())

    

    def GetRowIdx(rob_idx: UInt): UInt = {
        if (coreWidth == 1) return rob_idx
        else return (rob_idx >> log2Ceil(coreWidth))(log2Ceil(ROBEntry)-1,0)
    }
    def GetBankIdx(rob_idx: UInt): UInt = {
        if(coreWidth == 1) { return 0.U }
        else           { return rob_idx(log2Ceil(coreWidth)-1, 0).asUInt }
    }
    def GetPtrMask(ptr:UInt,size:Int):UInt={
        return ptr(size-1)
    }
    def GetPtrVal(ptr:UInt,size:Int):UInt={
        return ptr(size-2,0)
    }
    def isOlder(idx1:UInt,idx2:UInt):Bool={
        val size = log2Ceil(ROBEntry+1)
        GetPtrMask(idx1,size)===GetPtrMask(idx2,size)&&
        GetPtrVal(idx1,size)<GetPtrVal(idx2,size)||
        GetPtrMask(idx1,size)=/=GetPtrMask(idx2,size)&&
        GetPtrVal(idx1,size)>GetPtrVal(idx2,size)
    }
    val rob_entry   = RegInit(VecInit.fill(coreWidth)(VecInit.fill(ROBEntry/coreWidth)(0.U.asTypeOf(new ROBEntryBundle))))
    
    val rob_enq_val = GetPtrVal(rob_enq_ptr,RobbankSz)
    val rob_deq_val = GetPtrVal(rob_deq_ptr,RobbankSz)
    val rob_enq_mask= GetPtrMask(rob_enq_ptr,RobbankSz)
    val rob_deq_mask= GetPtrMask(rob_deq_ptr,RobbankSz)
    dontTouch(rob_enq_mask)
    //enq
    val enq_info     = WireInit(io.enq)
    val br_info     = WireInit(io.br_info)
    val flush       = WireInit(false.B)
    val is_exc_inst_commit = WireInit(false.B)
    val br_update   = RegInit(0.U.asTypeOf(Valid(new BrUpdateInfo)))

    val deq_vld_mask    = WireInit(UInt(log2Ceil(ICacheParam.blockBytes).W),(VecInit(rob_entry.map{i=> i(rob_deq_val).valid}).asUInt))
    val deq_finish_mask = WireInit(UInt(log2Ceil(ICacheParam.blockBytes).W),(VecInit(rob_entry.map{i=>i(rob_deq_val).finish&&i(rob_deq_val).valid}).asUInt))
    
    val exc_msg = WireInit(0.U.asTypeOf(Valid(new CommitExcMsg)))


    //异常信息必须和之前的比较谁旧
    val commit_exc_msg   = RegInit(0.U.asTypeOf(Valid(new CommitExcMsg)))
    val is_exc_older  = (!commit_exc_msg.valid)&&((!br_update.valid)||
                        isOlder(io.lsu_exc.bits.uops.rob_idx,br_update.bits.uop.rob_idx))||
                        (commit_exc_msg.valid&&io.lsu_exc.valid&&isOlder(io.lsu_exc.bits.uops.rob_idx,commit_exc_msg.bits.rob_idx)&&
                        isOlder(io.lsu_exc.bits.uops.rob_idx,br_update.bits.uop.rob_idx))
    
    exc_msg.valid           := io.lsu_exc.valid&&is_exc_older
    exc_msg.bits.ftq_idx    := io.lsu_exc.bits.uops.ftq_idx
    exc_msg.bits.rob_idx    := io.lsu_exc.bits.uops.rob_idx
    exc_msg.bits.pc_lob     := io.lsu_exc.bits.uops.pc_off
    exc_msg.bits.cause      := 0.U
    exc_msg.bits.epc        := 0.U
    exc_msg.bits.flush_typ  := FLUSH_REFETCH
    /* 
    可以提交的情况：
    1.无异常，此时只要finfish的个数等于vld的个数，就说明可以提交
    2.发生异常，此时只要该异常指令变为最旧的指令，此时就可以进行提交
     */
    val is_commit_flush = WireInit(MaskLower(VecInit(rob_entry.map{i=>i(rob_deq_val).finish&&i(rob_deq_val).flush&&i(rob_deq_val).valid}).asUInt))
    val is_exc_oldest   = Wire(Bool())
    is_exc_oldest := (is_commit_flush=/=0.U)&&(((is_commit_flush)&(deq_finish_mask))===is_commit_flush)
    dontTouch(is_exc_oldest)
    dontTouch(deq_finish_mask)
    val can_commit      = ((((deq_vld_mask)===(deq_finish_mask)))||is_exc_oldest)&&(deq_vld_mask=/=0.U)

    
    full := (rob_enq_val===rob_deq_val)&&(rob_enq_mask=/=rob_deq_mask)
    dontTouch(full)
    io.enq.uops.foreach{i=>
        i.ready := (!full)&&(rob_state===s_normal)
        } 
    io.commit:= DontCare
    for(i<- 0 until coreWidth){
        io.commit.bits.valid(i) := false.B
    }
    io.commit.valid := false.B
    io.enq_idxs:= DontCare
    io.flush := DontCare
    io.flush.valid := false.B
    
    val is_flush_older = (!br_update.valid)&&(!commit_exc_msg.valid)||
                        (!br_update.valid)&&isOlder(br_info.bits.uop.rob_idx,commit_exc_msg.bits.rob_idx)||
                        (br_update.valid&&isOlder(br_info.bits.uop.rob_idx,br_update.bits.uop.rob_idx)&&
                        isOlder(br_info.bits.uop.rob_idx,commit_exc_msg.bits.rob_idx)) 
    flush := br_info.bits.cfi_mispredicted&&br_info.valid&&is_flush_older


    dontTouch(do_enq)
    dontTouch(rob_deq_val)
    dontTouch(is_flush_older)
    val enq_idxs    = VecInit.tabulate(coreWidth)(i => PopCount(do_enq.take(i)))
    val enq_offset  = VecInit(enq_idxs.map(_+rob_enq_val))
    assert(PopCount(do_enq)===0.U||PopCount(do_enq)===coreWidth.U,"rob enq not fully")
    for(i <- 0 until coreWidth){
        //enq
        when(do_enq(i)){
            // rob_entry(i)(rob_enq_val).finish := false.B
            rob_entry(i)(rob_enq_val).valid := enq_info.uops(i).valid
            rob_entry(i)(rob_enq_val).uop   := enq_info.uops(i).bits
            io.enq_idxs(i).bits := Cat(rob_enq_mask,(rob_enq_val<<log2Ceil(coreWidth)) + i.U)
            io.enq_idxs(i).valid:= true.B
        }
        
        //deq
        when(can_commit){
            val deq_rob_idx = WireInit(UInt(log2Ceil(ROBEntry+1).W),Cat(rob_deq_ptr,i.U))
            dontTouch(deq_rob_idx)
            rob_entry(i)(rob_deq_val).valid  := false.B
            rob_entry(i)(rob_deq_val).finish := false.B
            rob_entry(i)(rob_deq_val).flush  := false.B
            io.commit.valid := true.B
            io.commit.bits.valid(i) := Mux(is_exc_oldest,
            Mux(io.flush.bits.flush_typ===FLUSH_REFETCH,
            is_commit_flush(i)&&(io.flush.bits.rob_idx=/=deq_rob_idx),is_commit_flush(i))
            ,rob_entry(i)(rob_deq_val).valid)
            io.commit.bits.commit_uops(i):=rob_entry(i)(rob_deq_val).uop
            //由于采用提交处理各种异常，所以commit的信号内包含的信号flush不用再次包含
        }

    }
    /* 
    exc
    此时处理来自执行单元的异常，并且更新uop，以便之后br_update
        */
    val br_miss_bank = GetBankIdx((br_info.bits.uop.rob_idx))
    val br_miss_row  = GetRowIdx((br_info.bits.uop.rob_idx))
    when(flush){
        for(i <- 0 until coreWidth){
            rob_entry(i)(br_miss_row).flush := Mux(br_miss_bank===i.U,true.B,false.B)
        }
    }
    val exc_bank = GetBankIdx((exc_msg.bits.rob_idx))
    val exc_row  = GetRowIdx((exc_msg.bits.rob_idx))
    when(exc_msg.valid){
        rob_entry(exc_bank)(exc_row).flush := true.B
    }
    for(i<-0 until numWakeupPorts){
        val wb_rob_bank = GetBankIdx(io.wb_resp(i).bits.uop.rob_idx)
        val wb_rob_idx  = GetRowIdx(io.wb_resp(i).bits.uop.rob_idx)
        dontTouch(wb_rob_bank)
        dontTouch(wb_rob_idx)
        when(io.wb_resp(i).valid){
            rob_entry(wb_rob_bank)(wb_rob_idx).finish := true.B
            if(hasDebug){
                rob_entry(wb_rob_bank)(wb_rob_idx).uop.wb_data := io.wb_resp(i).bits.wb_data
                rob_entry(wb_rob_bank)(wb_rob_idx).uop.ctrl := io.wb_resp(i).bits.uop.ctrl
            }
        }
    }

    when(is_exc_oldest){
        commit_exc_msg.valid := false.B
    }.elsewhen(exc_msg.valid){
        commit_exc_msg := exc_msg
    }
    //指针更新逻辑
    when(is_exc_oldest){
        rob_enq_ptr := 0.U
    }
    .elsewhen(do_enq.reduce(_||_)){
        rob_enq_ptr := rob_enq_ptr + 1.U
    }
    when(is_exc_oldest){
        rob_deq_ptr := 0.U
    }.elsewhen(can_commit){
        rob_deq_ptr := rob_deq_ptr + 1.U
    }
    //只有在提交阶段才会更新br——update，并且只有这时候才会去处理update信息，
    when(is_exc_oldest){
        br_update.valid := false.B
    }
    .elsewhen(flush){
        br_update := br_info
    }

    io.br_update.bits := br_update.bits
    io.br_update.valid:= is_exc_oldest&&(br_update.bits.uop.fu_code===FU_JMP&&br_update.bits.uop.uopc=/=uopAUIPC)
    //exc handle
    // exc
    when(is_exc_oldest){
        for(i <- 0 until coreWidth){
            for(j<- 0 until ROBEntry/coreWidth){
                rob_entry(i)(j).valid := false.B
                rob_entry(i)(j).flush := false.B
                rob_entry(i)(j).finish := false.B
            }
        }
    }
    when(is_exc_oldest){
        io.flush.valid           := true.B
        io.flush.bits.cause      := 0.U                     
        io.flush.bits.flush_typ  := Mux(commit_exc_msg.valid,commit_exc_msg.bits.flush_typ,FLUSH_MISPRED)                     
        io.flush.bits.ftq_idx    := Mux(commit_exc_msg.valid,commit_exc_msg.bits.ftq_idx,0.U)  
        io.flush.bits.rob_idx    := Mux(commit_exc_msg.valid,commit_exc_msg.bits.rob_idx,0.U)  
        io.flush.bits.pc_lob     := Mux(commit_exc_msg.valid,commit_exc_msg.bits.pc_lob,0.U)   
        io.flush.bits.cause      := Mux(commit_exc_msg.valid,commit_exc_msg.bits.cause,0.U)    
        io.flush.bits.epc        := Mux(commit_exc_msg.valid,commit_exc_msg.bits.epc,br_update.bits.target)      
        // io.flush.bits.flush_typ  := Mux(commit_exc_msg.valid,commit_exc_msg.bits.flush_typ,)

    }
    dontTouch(io.flush)
    //两个周期的处理时间，
    //FSM
	switch (rob_state){
		is(s_idle){
			rob_state := s_normal
		}
        is(s_normal){
            when(flush||exc_msg.valid){
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