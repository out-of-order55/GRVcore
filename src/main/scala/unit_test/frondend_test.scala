package grvcore
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._


class Update1 (Open:Boolean)(implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{
    val io = IO(new Bundle {
        //from bp
        val f3_resp      = Input(new BranchPrediction)
        val f3_valid     = Input(Bool())
        val f3_pred_pc   = Input(UInt(32.W))
        val f3_meta      = Input(UInt(bpdMaxMetaLength.W))
        val f3_ftq_idx   = Input(UInt(log2Ceil(ftqentries).W))

        val get_ftq_pc   = Flipped(new GetPCFromFtqIO())

        val f0_valid     = Input(Bool())
        val s0_vpc       = Output(UInt(32.W))
        val update       = Output(Valid(new BranchPredictionUpdate))
        //to trace
        val s0_ptr       = Output(UInt(32.W))
        val s3_ptr       = Output(UInt(32.W))
        val valid3       = Output(Bool())
        val valid0       = Output(Bool())
        val s0_pc        = Input(UInt(32.W))
        val pc           = Input(UInt(32.W))
        val target       = Input(UInt(32.W))
        val br_type      = Input(UInt(2.W))
        val taken        = Input(Bool())
        val rs1          = Input(UInt(5.W))
        val rd           = Input(UInt(5.W))
    })
    val s0_ptr  =  RegInit(0.U(32.W))
    val s3_ptr  =  RegInit(0.U(32.W))
    io.s0_ptr:=s0_ptr
    io.s3_ptr:=s3_ptr

    val check_resp   = RegNext(io.f3_resp)
    val f3_valid      = WireInit(io.f3_valid)


    //FTQ
    val pred_pc       = RegNext(RegNext(io.f3_pred_pc))
    val ftq_entry     = io.get_ftq_pc.entry
    val ftq_pc        = io.get_ftq_pc.pc
    val ftq_next_pc   = io.get_ftq_pc.next_pc
    val ftq_next_val  = io.get_ftq_pc.next_pc_val

    
    val pred_is_br    = ftq_entry.cfi_type===1.U
    val pred_is_jal   = ftq_entry.is_jal
    val pred_is_jalr  = ftq_entry.is_jalr
    val pred_br_mask  = ftq_entry.br_mask
    val pred_br_taken = ftq_entry.cfi_taken
    val pred_br_pc    = pred_pc + (UIntToOH(pred_br_mask)<<2)

    dontTouch(pred_is_br)
    dontTouch(pred_is_jal)
    dontTouch(pred_is_jalr)
    dontTouch(pred_br_mask)
    dontTouch(pred_br_taken)
    dontTouch(pred_br_pc)
    //update
    val update_cfi_idx= WireInit(bankoffset(io.pc))
    val update_taken  = WireInit(io.taken)
    val update_is_jal = WireInit(io.br_type===0.U)
    val update_is_jalr= WireInit(io.br_type===1.U)
    val update_is_br  = WireInit(io.br_type===2.U)
    val update_is_call= WireInit(io.br_type=/=2.U&&(io.rd===1.U||io.rd===5.U))
    val update_is_ret = WireInit((io.br_type===1.U&&(io.rd===0.U)&&(io.rs1===1.U||io.rs1===5.U)))
    val update_br_type= WireInit(Mux(update_is_br,1.U,Mux(update_is_call,2.U,Mux(update_is_ret,3.U,0.U))))
    val update_br_mask= WireInit(UIntToOH(bankoffset(io.pc),bankNum))&Fill(bankNum,update_is_br)
    val update_target = WireInit(io.target)

    


    val timer        = RegInit(0.U(32.W))




    val mispred = Wire(Bool())
    mispred := false.B
    //br taken 但btb miss 也算
    when(update_is_br||update_is_jal){
        mispred := update_taken^pred_br_taken||(pred_br_taken&&(pred_pc===pred_br_pc+4.U))  
    }
    when(update_is_jalr){
        mispred := pred_pc=/=update_target
    }
    val total_cnt = RegInit(0.U(32.W))
    val mispred_cnt = RegInit(0.U(32.W))
    dontTouch(total_cnt)
    dontTouch(mispred_cnt)
    when(io.update.valid){
        total_cnt := total_cnt+1.U
    }
    when(mispred&io.update.valid ){
        mispred_cnt := mispred_cnt +1.U
    }
    //////////////////////performance/////////////

    val check = Module(new Checker)
    check.io.clock := clock
    check.io.reset := reset


    val check_cnt=RegInit(0.U(2.W))
    val check_open=  Mux(Open.B,check_cnt===2.U,check_cnt===1.U)
    val checkData=WireInit(check_open&&io.update.valid )
    dontTouch(checkData)
    when(check_open&&io.update.valid ){
        check_cnt:=0.U
    }.elsewhen(io.f0_valid){
        check_cnt := check_cnt+1.U
    }
    timer := timer + 1.U





    when(checkData){
        s0_ptr := s0_ptr +1.U
        s3_ptr := s3_ptr +1.U
    }

    check.io.finish:=false.B
    check.io.ret   :=false.B
    dontTouch(io.f3_resp)


    io.get_ftq_pc.ftq_idx               := RegNext(io.f3_ftq_idx)

    io.update.valid                     := RegNext(RegNext(f3_valid,false.B))
    io.update.bits.br_mask              := update_br_mask
    io.update.bits.cfi_idx.bits         := update_cfi_idx   
    io.update.bits.cfi_idx.valid        := RegNext(RegNext(f3_valid,false.B)) 
    io.update.bits.cfi_taken            := update_taken
    io.update.bits.cfi_type             := update_br_type
    io.update.bits.ghist                := 0.U                      
    io.update.bits.is_jal               := update_is_jal                            
    io.update.bits.is_jalr              := update_is_jalr                            
    io.update.bits.is_mispredict_update := mispred&io.update.valid    
    io.update.bits.is_commit_update     := io.update.valid                                            
    io.update.bits.meta                 := RegNext(RegNext(io.f3_meta))                        
    io.update.bits.pc                   := bankAlign(io.pc)                    
    io.update.bits.target               := Mux(update_br_type===3.U,update_target,update_target + io.pc)                               
    io.valid3                           := RegNext(RegNext(f3_valid,false.B))
    io.valid0                           := io.f0_valid
    io.s0_vpc                           := (io.s0_pc)
}



/* 

假如是miss_pred，需要延迟两个周期进行取指令，需要修复一些部件：
update_s0(commit 阶段)->|reg|update_s1(送入BP)->|reg|update_s2（完成修复）
延迟一个周期也可以，不过会引入commit到frontend的路径
 */
class FrontEndTest (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{

    val bp           = Module(new BranchPredictor)
    val ras          = Module(new GRVRAS)
    val ftq          = Module(new FetchTargetQueue)
    val update       = Module(new Update1(false))
    val trace        = Module(new TraceGen)

    // val f3_resp       = IO(Output(new BranchPredictionBundle))
    val s0_vpc       = WireInit(0.U(XLEN.W))
    val s0_valid     = WireInit(false.B)
    val s0_mask      = WireInit(MaskUpper(UIntToOH(bankoffset(s0_vpc),bankNum)))
    
    
    val s1_mask      = RegNext(s0_mask)
    val s1_vpc       = RegNext(s0_vpc)
    val s1_valid     = RegNext(s0_valid, false.B)

    val s2_mask      = RegNext(s1_mask)
    val s2_vpc       = RegNext(s1_vpc)
    val s2_valid     = RegNext(s1_valid, false.B)
    val start        = RegInit(false.B)
    
    val s3_vpc       = RegNext(s2_vpc)
    val s3_mask      = RegNext(s2_mask)
    val s3_valid     = RegNext(s2_valid, false.B)
    

    start := true.B
    val start1 = RegNext(start)

    
/////////////////Checker///////////////////

    trace.io.s0_ptr := update.io.s0_ptr
    trace.io.s3_ptr := update.io.s3_ptr

    update.io.f3_valid     := s3_valid
    update.io.f0_valid     := s0_valid
    dontTouch(start)
    dontTouch(start1)

    /* 
    s0          s1              s2              s3             s4          s5      s6       s7   ...s10 ...s1   
    req                                         resp                                         update   req     resp
                                                pred_wftq      check    update_wftq
     */                                        

    dontTouch(s0_mask)

////////////////////BP///////////////////
    bp.io.f0_req.valid     := s0_valid
    bp.io.f0_req.bits.pc   := bankAlign(s0_vpc)
    bp.io.f0_req.bits.mask := s0_mask

    bp.io.f0_req.bits.ghist:= DontCare
    bp.io.f3_fire          := DontCare
    bp.io.resp.f1          := DontCare
    bp.io.resp.f2          := DontCare
    bp.io.resp.f3          := DontCare
////////////////////S3//////////////////
    val s3_fetch_bundle = Wire(new FetchBundle)



//选出最旧的指令，这里测试只选择输入的pc
    val s3_taken_idx = WireInit(PopCount(~s3_mask(bankNum-1,0)))
    val s3_resp      = WireInit(bp.io.resp.f3.preds(s3_taken_idx))
    val s3_ret_addr  = WireInit(ras.io.rasResp.read_addr)
    val s3_meta      = WireInit(bp.io.resp.f3.meta)
    ras.io.rasResp.br_type.valid := s3_resp.predicted_pc.valid
    ras.io.rasResp.br_type.bits  := s3_resp.br_type
    ras.io.rasResp.write_addr    := bankAlign(s3_vpc) + (s3_taken_idx<<2.U) +4.U
    dontTouch(s3_fetch_bundle)
    dontTouch(s3_taken_idx)
    // dontTouch(s3_resp)
    //taken 同时也要BTB hit
    val s3_pred_pc = Mux(s3_resp.is_br,
                    Mux(s3_resp.taken&s3_resp.predicted_pc.valid,s3_resp.predicted_pc.bits,s3_vpc+4.U),
                    Mux(s3_resp.is_call,s3_resp.predicted_pc.bits,
                    Mux(s3_resp.is_ret,s3_ret_addr,s3_vpc+4.U)))
    
    s3_fetch_bundle.mask     :=  s3_mask
    s3_fetch_bundle.pc       :=  s3_vpc
    s3_fetch_bundle.bpd_meta :=  s3_meta
    s3_fetch_bundle.br_mask  :=  UIntToOH(s3_taken_idx) //需要预解码
    s3_fetch_bundle.bpSrc    :=  0.U
    s3_fetch_bundle.cfi_idx.bits  :=  s3_taken_idx
    s3_fetch_bundle.cfi_idx.valid :=  s3_resp.predicted_pc.valid
    s3_fetch_bundle.cfi_type :=  s3_resp.br_type
    s3_fetch_bundle.is_jal   :=  s3_resp.is_jal
    s3_fetch_bundle.is_jalr  :=  s3_resp.is_jalr
    s3_fetch_bundle.cfi_taken:=  s3_resp.taken
    s3_fetch_bundle.ftq_idx  :=  ftq.io.enq_idx
    s3_fetch_bundle.ghist    :=  DontCare
    s3_fetch_bundle.insts    :=  DontCare
///////////////Trace////////////////////


    trace.io.clock         := clock
    trace.io.reset         := reset
    trace.io.valid3        := update.io.valid3  
    trace.io.valid0        := update.io.valid0  
    update.io.pc           := (trace.io.pc   )
    update.io.s0_pc        := (trace.io.s0_pc)   
    update.io.target       := trace.io.target 
    update.io.br_type      := trace.io.br_type
    update.io.taken        := trace.io.taken  
    update.io.rs1          := trace.io.rs1    
    update.io.rd           := trace.io.rd  


    update.io.f3_pred_pc := s3_pred_pc
    update.io.f3_resp    := s3_resp
    update.io.f3_valid   := s3_valid

    update.io.f3_meta    := s3_meta
    update.io.f3_ftq_idx := ftq.io.enq_idx
/////////////////////////////////////////
    bp.io.update <> ftq.io.bpdupdate

    ras.io.rasUpdate <> ftq.io.ras_update
    ftq.io.redirect  := update.io.update.bits.is_mispredict_update
    ftq.io.get_ftq_pc <>update.io.get_ftq_pc
    ftq.io.enq.valid := s3_valid
    ftq.io.enq.bits  := s3_fetch_bundle
    
    ftq.io.deq.valid := update.io.update.valid
    ftq.io.deq.bits  := DontCare

    // ftq.io.brupdate.br_mask          := update.io.update.bits.br_mask
    // ftq.io.brupdate.cfi_idx          := update.io.update.bits.cfi_idx         
    // ftq.io.brupdate.cfi_taken        := update.io.update.bits.cfi_taken       
    // ftq.io.brupdate.cfi_mispredicted := update.io.update.bits.is_mispredict_update
    // // ftq.io.brupdate.cfi_is_call      := update.io.update.bits.cfi_is_call     
    // // ftq.io.brupdate.cfi_is_ret       := update.io.update.bits.cfi_is_ret      
    // ftq.io.brupdate.cfi_type         := update.io.update.bits.cfi_type        
    // ftq.io.brupdate.is_jal           := update.io.update.bits.is_jal          
    // ftq.io.brupdate.is_jalr          := update.io.update.bits.is_jalr         
    // ftq.io.brupdate.target           := update.io.update.bits.target          
/////////////////////////////////////////
    s0_valid := (start)&(!start1)|| ftq.io.bpdupdate.valid
    s0_vpc   := update.io.s0_vpc
    dontTouch(s0_vpc)
}
