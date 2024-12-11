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
*/
class TraceGen extends BlackBox{
    val io = IO(new Bundle {
        val clock   = Input(Clock())
        val reset   = Input(Bool())
        val valid   = Input(Bool())
        val pc      = Output(UInt(32.W))
        val target  = Output(UInt(32.W))
        val br_type = Output(UInt(2.W))
        val taken   = Output(Bool())
        val rs1     = Output(UInt(5.W))
        val rd      = Output(UInt(5.W))
    })
}
class Update (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{
    val io = IO(new Bundle {
        val valid   = Output(Bool())
        
        val pc      = Input(UInt(32.W))
        val target  = Input(UInt(32.W))
        val br_type = Input(UInt(2.W))
        val taken   = Input(Bool())
        val rs1     = Input(UInt(5.W))
        val rd      = Input(UInt(5.W))
    })
}
class BPTest1 (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{

    val check        = Module(new Checker)
    val bp           = Module(new BranchPredictor)
    val s0_vpc       = WireInit(0.U(XLEN.W))
    val s0_valid     = WireInit(false.B)
    val s0_mask = MaskUpper(UIntToOH(bankoffset(s0_vpc)))

    val timer        = RegInit(0.U(32.W))
    val s1_clear     = WireInit(false.B)
    val s1_vpc       = RegNext(s0_vpc)
    val s1_valid     = RegNext(s0_valid, false.B)

    val s2_vpc       = RegNext(s1_vpc)
    val s2_valid     = RegNext(s1_valid && !s1_clear, false.B)
    val start        = RegInit(false.B)
    
    

    start := true.B
    val start1 = RegNext(start)
    val data_fail = Wire(Bool())
    val fail = WireInit(s2_valid&data_fail)
    
    dontTouch(fail)
/////////////////Checker///////////////////
    check.io.clock := clock
    check.io.reset := reset
    when(s0_vpc===(blockBytes*50).U){
        check.io.finish := true.B
        check.io.ret    := false.B 
    }.elsewhen(timer===50.U){
        // println("Bus fail")
        check.io.finish := true.B
        check.io.ret    := true.B 
    }.elsewhen(fail){
        // println("Data fail")
        check.io.finish := true.B
        check.io.ret    := true.B 
    }
    .otherwise{
        check.io.finish := false.B
        check.io.ret    := false.B 
    }
    dontTouch(start)
    dontTouch(start1)
    dontTouch(s1_clear)
////////////////////BP///////////////////
    // val   s1_bp_resp
    bp.io.f0_req.valid     := s0_valid
    bp.io.f0_req.bits.pc   := s0_vpc
    bp.io.f0_req.bits.mask := s0_mask


/////////////////////////////////////////
    when ((start)&(!start1)){
        s0_valid   := true.B
        s0_vpc     := 0.U
    }
    when(s1_valid&(!s1_clear)){
        s0_vpc   := s1_vpc + blockBytes.U 
        s0_valid := true.B
    }
    when(s2_valid){
        s1_clear := true.B
        s0_vpc   := s2_vpc
        s0_valid := true.B
    }
}