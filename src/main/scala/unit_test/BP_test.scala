package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/*
验证：
1.测试单个模块
    1.ubtb
    2.RAS+BTB+BIM
2.链接各个模块，联调：大概66%的正确率，其中分支145条，66条命中，大概率是分支方向预测不太行,
BTB由于只存taken的，所以预测率低，现在将BTB支持没有taken的分支，bim预测率为91%
然后三个全部联调，大概为72%（通过测试预测的pc是否为实际的pc），
tips:更新后的为93%左右，性能计数器设置错了

ubtb/BTB：
每次banknum条指令，每次给出update信息，以及req信息，
预测在s0阶段，预测结果在s1阶段，实际结果在s2阶段
连续访问一个指令，测试预测率
s3阶段会给出实际的meta信息，update时将这些信息送回s0，
trace要满足的条件，得给出类型，跳转地址，br_mask

1.访问一个pc，然后等pc更新完后重新访问pc，要求内容必须一致
2.访问n个pc，不能未taken的pc写入，如果之前写过那不算
BIM：？
目前的问题是，commit更新写入可能与读冲突（单端口SRAM），更新优先级高，这样必然会导致一些请求读到错误的结果
tip：之后会换成双端口SRAM测试性能差异
*/

class TraceGen extends BlackBox{
    val io = IO(new Bundle {
        val clock   = Input(Clock())
        val reset   = Input(Bool())
        val valid3  = Input(Bool())
        val valid0  = Input(Bool())
        val s0_ptr  = Input(UInt(32.W))
        val s3_ptr  = Input(UInt(32.W))

        val pc      = Output(UInt(32.W))
        val s0_pc   = Output(UInt(32.W))
        val target  = Output(UInt(32.W))
        val br_type = Output(UInt(2.W))
        val taken   = Output(Bool())
        val rs1     = Output(UInt(5.W))
        val rd      = Output(UInt(5.W))
    })
}


class Update (Open:Boolean)(implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{
    val io = IO(new Bundle {
        //from bp
        val f3_resp      = Input(new BranchPredictionBundle)
        val f3_valid     = Input(Bool())
        val f3_pred_pc   = Input(UInt(32.W))
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
    val total_br = RegInit(0.U(32.W))
    val bim_br   = RegInit(0.U(32.W))
    val total_inst=RegInit(0.U(32.W))
    val pred    = RegInit(0.U(32.W))
    val btb_cnt = RegInit(0.U(32.W))
    val total_btb = RegInit(0.U(32.W))
    // val check =  Module(new Checker)

    // check.io.clock := clock
    // check.io.reset := reset
    
    // check.io.finish   := false.B
    // check.io.ret      := false.B 

    val f3_valid      = WireInit(io.f3_valid)

    
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

    val check_resp   = RegNext(io.f3_resp)
    val check_idx    = bankoffset(io.pc)
    val timer        = RegInit(0.U(32.W))
    val bim_right    = update_is_br&&(check_resp.preds(check_idx).is_br)&&
                    (!(update_taken^check_resp.preds(check_idx).taken))
    val total_right  = bim_right||(check_resp.preds(check_idx).is_call&&update_is_call)||
                    (check_resp.preds(check_idx).is_ret&&update_is_ret)
    val pred_right    = RegInit(0.U(32.W))
    val data_fail     = Wire(Bool())
    val comm_target   = Mux(update_is_jalr,io.target,Mux(update_taken,(io.target.asSInt+io.pc.asSInt).asUInt,io.pc+4.U))
    dontTouch(pred_right)

    //////////////////////performance/////////////
    dontTouch(bim_br)
    dontTouch(total_br)
    dontTouch(pred)
    dontTouch(total_inst)
    dontTouch(btb_cnt)
    dontTouch(total_btb)
    dontTouch(comm_target)
    when(comm_target===RegNext(io.f3_pred_pc)&&io.update.valid){
        pred_right := pred_right + 1.U
    }
    when(total_right){
        pred := pred +1.U
    }
    when(io.update.valid ){
        total_inst := total_inst + 1.U
    }
    when(bim_right){
        bim_br := bim_br +1.U
    }
    when((update_is_call||update_is_ret||update_is_br)){
        total_btb := total_btb +1.U
    }
    when((check_resp.preds(check_idx).is_call||
    check_resp.preds(check_idx).is_ret||check_resp.preds(check_idx).is_br)
    ){
        btb_cnt := btb_cnt +1.U
    }
    when(update_is_br){
        total_br := total_br +1.U
    }

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
        //for ubtb
        // data_fail:= Open.B&&io.update.bits.br_taken&&
        //         (check_resp.pc=/=io.update.bits.pc||check_resp.preds(check_idx).predicted_pc.bits=/=io.update.bits.target)&&
        //         (update_is_jal||update_is_br)
        //for BTB
        data_fail :=Open.B&&update_taken&&(
                (check_resp.pc=/=io.update.bits.pc)||
                (update_is_call^check_resp.preds(check_idx).is_call||
                update_is_ret^check_resp.preds(check_idx).is_ret||
                update_is_br^check_resp.preds(check_idx).is_br))
    }.otherwise{
        data_fail:=false.B
    }
    dontTouch(data_fail)
    when(checkData&(!data_fail)){
        s0_ptr := s0_ptr +1.U
        s3_ptr := s3_ptr +1.U
    }
    when(data_fail){
        check.io.finish:= true.B
        check.io.ret   := data_fail
    }.otherwise{
        check.io.finish:=false.B
        check.io.ret   :=false.B
    }
    dontTouch(io.f3_resp)

    io.update.valid                     := RegNext(f3_valid,false.B)
    io.update.bits.br_mask              := update_br_mask
    io.update.bits.cfi_idx.bits         := update_cfi_idx   
    io.update.bits.cfi_idx.valid        := RegNext(f3_valid,false.B) 
    io.update.bits.cfi_taken            := update_taken
    io.update.bits.cfi_type             := update_br_type
    io.update.bits.ghist                := 0.U                      
    io.update.bits.is_jal               := update_is_jal                            
    io.update.bits.is_jalr              := update_is_jalr                            
    io.update.bits.is_mispredict_update := false.B                                                 
    io.update.bits.meta                 := check_resp.meta                        
    io.update.bits.pc                   := bankAlign(io.pc)                    
    io.update.bits.target               := update_target + io.pc                               
    io.valid3                           := RegNext(f3_valid,false.B)
    io.valid0                           := io.f0_valid
    io.s0_vpc                           := (io.s0_pc)
}


class BPTest1 (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{

    val bp           = Module(new BranchPredictor)
    val ras          = Module(new GRVRAS)
    val update       = Module(new Update(false))
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
    // val data_fail = Wire(Bool())
    // val fail = WireInit(s2_valid&data_fail)
    
    // dontTouch(fail)
    
/////////////////Checker///////////////////



    dontTouch(start)
    dontTouch(start1)



    trace.io.s0_ptr := update.io.s0_ptr
    trace.io.s3_ptr := update.io.s3_ptr
    /* 
    s0          s1              s2              s3          s4          s5      s6       s7   ...s10 ...s1   
    req                                         resp                    update  req     resp
     */

    dontTouch(s0_mask)
////////////////////BP///////////////////
    // val   s1_bp_resp
    bp.io.f0_req.valid     := s0_valid
    bp.io.f0_req.bits.pc   := bankAlign(s0_vpc)
    bp.io.f0_req.bits.mask := s0_mask

    bp.io.f0_req.bits.ghist:= DontCare
    bp.io.f3_fire          := DontCare
    bp.io.resp.f1          := DontCare
    bp.io.resp.f2          := DontCare
    // dontTouch(bp.io.resp.f3)
//from bp
    update.io.f3_resp      := bp.io.resp.f3


    val s3_idx = (bankNum.U-PopCount(s3_mask(bankNum-1,0)))(log2Ceil(bankNum)-1,0)
    dontTouch(s3_idx)
    // dontTouch(num)
    val retAddr = ras.io.rasResp.read_addr 
    dontTouch(retAddr)
    val check_ras = RegNext(retAddr)=/=update.io.update.bits.target&&update.io.update.bits.cfi_is_ret
    dontTouch(check_ras)

    val pred_pc = Mux(bp.io.resp.f3.preds(s3_idx).is_br,
                    Mux(bp.io.resp.f3.preds(s3_idx).taken,bp.io.resp.f3.preds(s3_idx).predicted_pc.bits,s3_vpc+4.U),
                    Mux(bp.io.resp.f3.preds(s3_idx).is_call,bp.io.resp.f3.preds(s3_idx).predicted_pc.bits,
                    Mux(bp.io.resp.f3.preds(s3_idx).is_ret,retAddr,s3_vpc+4.U)))
    dontTouch(pred_pc)
    update.io.f3_pred_pc:= pred_pc
    // f3_resp := bp.io.resp.f3
    ras.io.rasResp.br_type.valid := bp.io.resp.f3.preds(s3_idx).predicted_pc.valid
    ras.io.rasResp.br_type.bits  := bp.io.resp.f3.preds(s3_idx).br_type
    ras.io.rasResp.write_addr    := bankAlign(s3_vpc) + (s3_idx<<2.U) +4.U
    ras.io.rasUpdate.update_type.valid    := update.io.update.valid
    ras.io.rasUpdate.update_type.bits     := update.io.update.bits.cfi_type
    ras.io.rasUpdate.update_addr          := Mux(update.io.update.bits.cfi_is_call,update.io.update.bits.pc+(RegNext(s3_idx<<2))+4.U,0.U)
    ras.io.rasUpdate.is_commit_update     := update.io.update.valid
    ras.io.rasUpdate.is_misspredict       := update.io.update.valid

    update.io.f3_valid     := s3_valid
    update.io.f0_valid     := s0_valid
    bp.io.update           := update.io.update
/// to Trace
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
    dontTouch(s0_vpc)
    dontTouch(s0_valid)
/////////////////////////////////////////
    // when (){
    //     s0_valid :=true.B
    // }

    // val s0_vpc_again   = Reg(UInt(XLEN.W))
    // val s0_valid_again =  
    // when(s0_valid){
    //     s0_vpc_again := update.io.s0_vpc 
    // }

    s0_valid := (start)&(!start1)||RegNext(RegNext(update.io.update.valid))
    s0_vpc   := (update.io.s0_vpc)
}