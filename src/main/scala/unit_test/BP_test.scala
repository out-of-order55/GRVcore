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
2.链接各个模块，联调
ubtb：
每次banknum条指令，每次给出update信息，以及req信息，
预测在s0阶段，预测结果在s1阶段，实际结果在s2阶段
连续访问一个指令，测试预测率
s3阶段会给出实际的meta信息，update时将这些信息送回s0，
trace要满足的条件，得给出类型，跳转地址，br_mask

1.访问一个pc，然后等pc更新完后重新访问pc，要求内容必须一致
2.访问n个pc，不能未taken的pc写入，如果之前写过那不算
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

class Update (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{
    val io = IO(new Bundle {
        //from bp
        val f3_resp      = Input(new BranchPredictionBundle)
        val f3_valid     = Input(Bool())
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
    // val check =  Module(new Checker)

    // check.io.clock := clock
    // check.io.reset := reset
    
    // check.io.finish   := false.B
    // check.io.ret      := false.B 
    val f3_taken      = (0 until bankNum).map{ w=>
        io.f3_resp.preds(w).taken
    }
    val f3_taken_idx  = WireInit(PriorityEncoder(f3_taken.asUInt))

    val f3_meta       = WireInit(io.f3_resp.meta)
    val f3_pc         = WireInit(io.f3_resp.pc)
    val f3_is_br      = WireInit(io.f3_resp.preds(f3_taken_idx).is_br)
    val f3_is_jal     = WireInit(io.f3_resp.preds(f3_taken_idx).is_jal)
    val f3_pred_pc    = WireInit(io.f3_resp.preds(f3_taken_idx).predicted_pc) 

    val check_meta    = RegNext(f3_meta      )   
    val check_pc      = RegNext(f3_pc        )   
    val check_is_br   = RegNext(f3_is_br     )   
    val check_is_jal  = RegNext(f3_is_jal    )   
    val check_pred_pc = RegNext(f3_pred_pc   )  
    val check_idx     = RegNext(f3_taken_idx) 
    val check = Module(new Checker)
    check.io.clock := clock
    check.io.reset := reset

    val check_cnt=RegInit(0.U(2.W))
    val checkData=WireInit(check_cnt===2.U&&io.update.valid )
    dontTouch(checkData)
    when(check_cnt===2.U&&io.update.valid ){
        check_cnt:=0.U
    }.elsewhen(io.f0_valid){
        check_cnt := check_cnt+1.U
    }
    // val check_resp = RegNext(bp.io.resp.f3)
    val data_fail  = Wire(Bool())
    when(checkData){
        data_fail:= io.update.bits.br_taken&&
                (check_pc=/=io.update.bits.pc||check_pred_pc.bits=/=io.update.bits.target)
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




    val f3_valid      = WireInit(io.f3_valid)

    val update_br_mask= WireInit(UIntToOH(bankoffset(io.pc),bankNum))
    val update_cfi_idx= WireInit(bankoffset(io.pc))
    val update_target = WireInit(io.target)
    val update_taken  = WireInit(io.taken)
    val update_is_jal = WireInit(io.br_type===0.U)
    val update_is_jalr= WireInit(io.br_type===1.U)
    val update_is_br  = WireInit(io.br_type===2.U)
    val update_is_call= WireInit(io.br_type=/=2.U&&(io.rd===1.U||io.rd===5.U))
    val update_is_ret = WireInit((io.br_type===1.U&&(io.rd===0.U)&&(io.rs1===1.U||io.rs1===5.U)))
    val update_br_type= WireInit(Mux(update_is_br,1.U,Mux(update_is_call,2.U,Mux(update_is_ret,3.U,0.U))))


    io.update.valid                     := RegNext(f3_valid,false.B)
    io.update.bits.br_mask              := update_br_mask
    io.update.bits.br_taken             := update_taken
    io.update.bits.cfi_idx.bits         := update_cfi_idx   
    io.update.bits.cfi_idx.valid        := RegNext(f3_valid,false.B) 
    io.update.bits.cfi_taken            := update_taken
    io.update.bits.cfi_type             := update_br_type
    io.update.bits.ghist                := 0.U    
    io.update.bits.is_br                := update_is_br        
    io.update.bits.is_call              := update_is_call                        
    io.update.bits.is_jal               := update_is_jal                            
    io.update.bits.is_jalr              := update_is_jalr                            
    io.update.bits.is_mispredict_update := false.B                            
    io.update.bits.is_ret               := update_is_ret                            
    io.update.bits.meta                 := RegNext(f3_meta)                        
    io.update.bits.pc                   := bankAlign(io.pc)                    
    io.update.bits.target               := update_target + io.pc                               
    io.valid3                           := RegNext(f3_valid,false.B)
    io.valid0                           := io.f0_valid
    io.s0_vpc                           := bankAlign(io.s0_pc)
}
class BPTest1 (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{

    
    val bp           = Module(new BranchPredictor)
    val update       = Module(new Update)
    val trace        = Module(new TraceGen)

    // val f3_resp       = IO(Output(new BranchPredictionBundle))
    val s0_vpc       = WireInit(0.U(XLEN.W))
    val s0_valid     = WireInit(false.B)
    val s0_mask = MaskUpper(UIntToOH(bankoffset(s0_vpc)))

    val timer        = RegInit(0.U(32.W))
    // val s1_clear     = WireInit(false.B)
    val s1_vpc       = RegNext(s0_vpc)
    val s1_valid     = RegNext(s0_valid, false.B)

    val s2_vpc       = RegNext(s1_vpc)
    val s2_valid     = RegNext(s1_valid, false.B)
    val start        = RegInit(false.B)
    
    val s3_valid     = RegNext(s2_valid, false.B)
    

    start := true.B
    val start1 = RegNext(start)
    // val data_fail = Wire(Bool())
    // val fail = WireInit(s2_valid&data_fail)
    
    // dontTouch(fail)
    timer := timer + 1.U
/////////////////Checker///////////////////



    dontTouch(start)
    dontTouch(start1)



    trace.io.s0_ptr := update.io.s0_ptr
    trace.io.s3_ptr := update.io.s3_ptr
    /* 
    s0          s1              s2              s3          s4          s5      s6       s7   ...s10 ...s1   
    req                                         resp                    update  req     resp
     */


////////////////////BP///////////////////
    // val   s1_bp_resp
    bp.io.f0_req.valid     := s0_valid
    bp.io.f0_req.bits.pc   := s0_vpc
    bp.io.f0_req.bits.mask := s0_mask

    bp.io.f0_req.bits.ghist:= DontCare
    bp.io.f3_fire          := DontCare
    bp.io.resp.f1          := DontCare
    bp.io.resp.f2          := DontCare
    // dontTouch(bp.io.resp.f3)
//from bp
    update.io.f3_resp      := bp.io.resp.f3
    // f3_resp := bp.io.resp.f3
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
    s0_vpc   := bankAlign(update.io.s0_vpc)
}