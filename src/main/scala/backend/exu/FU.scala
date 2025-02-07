package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.util._
// import freechips.rocketchip.tile.TileKey
import freechips.rocketchip.rocket.{PipelinedMultiplier,Causes,CSR}
import org.chipsalliance.cde.config.Parameters
/* 
ALU为可配置模块，可以选择：
1.ALU
2.MUL
3.JMP
 */
class SupportedFuncUnits(
    val alu: Boolean  = false,
    val jmp: Boolean  = false,
    val mem: Boolean  = false,
    val muld: Boolean = false,
    val csr: Boolean  = false)
{
}


/////////////////////////////////////////////////////////////////////////////////////////
abstract class FunctionalUnit(
val dataWidth: Int,
val isJmpUnit: Boolean = false,
val needsFcsr: Boolean = false)
(implicit p: Parameters) extends GRVModule 
{
    val io = IO(new Bundle {
        val kill   = Input(Bool())

        val req    = Flipped(new DecoupledIO(new ExuReq()))
        val resp   = (new DecoupledIO(new ExuResp()))

        val get_ftq_pc = if (isJmpUnit) Flipped(new GetPCFromFtqIO()) else null
        // only used by branch unit
        val brinfo     = if (isJmpUnit) Output(Valid(new BrUpdateInfo)) else null
    })


    
    val uop = io.req.bits.uop
    val killed = io.req.bits.kill

    val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)
    val op1_data = Wire(UInt(XLEN.W))
    dontTouch(imm_xprlen)
    io.resp.valid := false.B
    io.resp.bits.uop := uop
    io.resp.bits.wb_data:= DontCare
    val uop_pc = WireInit(0.U(XLEN.W))
    if (isJmpUnit) {
        val block_pc = AlignPCToBoundary(io.get_ftq_pc.pc, ICacheParam.blockBytes)
        uop_pc:= (block_pc | uop.pc_off)

        op1_data := Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                Mux(uop.ctrl.op1_sel.asUInt === OP1_PC  , Sext(uop_pc, XLEN),
                                                            0.U))
    } else {
        op1_data := Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                            0.U)
    }

    val op2_data = Mux(uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.asUInt, XLEN),
                    Mux(uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.prs1(4,0),
                    Mux(uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                    Mux(uop.ctrl.op2_sel === OP2_NEXT,4.U,0.U))))
    dontTouch(op1_data)
    dontTouch(op2_data)
}

class ALUUnit(dataWidth: Int)(implicit p: Parameters)
extends FunctionalUnit(
    isJmpUnit = false,
    dataWidth = dataWidth){


    val alu = Module(new ALU())
    io.req.ready:= true.B
    alu.io.in1 := op1_data.asUInt
    alu.io.in2 := op2_data.asUInt
    alu.io.fn  := uop.ctrl.op_fcn
    alu.io.dw  := uop.ctrl.fcn_dw

    io.resp.valid := io.req.fire&&(!killed)
    io.resp.bits.wb_data := alu.io.out
}

class CSRUnit(dataWidth: Int)(implicit p: Parameters)
extends FunctionalUnit(
    isJmpUnit = false,
    dataWidth = dataWidth){


    
    io.req.ready:= true.B


    io.resp.valid := io.req.fire&&(!killed)
    io.resp.bits.uop := io.req.bits.uop
    io.resp.bits.wb_data := 0.U
}



class JMPUnit(dataWidth: Int)(implicit p: Parameters)
extends FunctionalUnit(
    isJmpUnit = true,
    dataWidth = dataWidth){

    io.req.ready := true.B
    val brinfo = Wire(new BrUpdateInfo)

    val rs1 = io.req.bits.rs1_data
    val rs2 = io.req.bits.rs2_data
    val br_eq  = (rs1 === rs2)
    val br_ltu = (rs1.asUInt < rs2.asUInt)
    val br_lt  = (~(rs1(XLEN-1) ^ rs2(XLEN-1)) & br_ltu |
                    rs1(XLEN-1) & ~rs2(XLEN-1)).asBool
    dontTouch(br_lt)
    val pc_sel = MuxLookup(uop.ctrl.br_type, PC_PLUS4)(
                    Seq(    BJP_N   -> PC_PLUS4,
                            BJP_NE  -> Mux(!br_eq,  PC_BRJMP, PC_PLUS4),
                            BJP_EQ  -> Mux( br_eq,  PC_BRJMP, PC_PLUS4),
                            BJP_GE  -> Mux(!br_lt,  PC_BRJMP, PC_PLUS4),
                            BJP_GEU -> Mux(!br_ltu, PC_BRJMP, PC_PLUS4),
                            BJP_LT  -> Mux( br_lt,  PC_BRJMP, PC_PLUS4),
                            BJP_LTU -> Mux( br_ltu, PC_BRJMP, PC_PLUS4),
                            BJP_J   -> PC_BRJMP,
                            BJP_JR  -> PC_JALR
                            ))

    val is_taken = io.req.valid &&
                    !killed &&
                    (uop.is_br || uop.is_jalr || uop.is_jal) &&
                    (pc_sel =/= PC_PLUS4)

    // "mispredict" means that a branch has been resolved and it must be killed
    val mispredict = WireInit(false.B)

    val is_br          = io.req.valid && !killed && uop.is_br 
    val is_jal         = io.req.valid && !killed && uop.is_jal
    val is_jalr        = io.req.valid && !killed && uop.is_jalr

    when (is_br || is_jal) {
        if (!isJmpUnit) {
            assert (pc_sel =/= PC_JALR)
        }
        when (pc_sel === PC_PLUS4) {
            mispredict := uop.taken
        }
        when (pc_sel === PC_BRJMP) {
            mispredict := !uop.taken
        }
    }




    val target_offset = imm_xprlen(20,0).asSInt


    val target_base = Mux(uop.ctrl.br_type===BJP_JR,io.req.bits.rs1_data.asSInt,uop_pc.asSInt)
    val target_XLEN = Wire(UInt(XLEN.W))
    target_XLEN := (target_base + target_offset).asUInt
    

    val cfi_idx = (uop.pc_off/(XLEN/8).U)(log2Ceil(ICacheParam.blockBytes)-1,0)


    when (pc_sel === PC_JALR) {
        mispredict := 
                        (io.get_ftq_pc.next_pc =/= target_XLEN) ||
                        !io.get_ftq_pc.entry.cfi_idx.valid ||
                        (io.get_ftq_pc.entry.cfi_idx.bits =/= cfi_idx)
    }

    io.get_ftq_pc.ftq_idx   := uop.ftq_idx
    brinfo.uop              := uop
    brinfo.br_mask          := UIntToOH(uop.pc_off/(XLEN/8).U)
    brinfo.cfi_idx.valid    := (io.req.fire)&&(uop.uopc=/=uopAUIPC)
    brinfo.cfi_idx.bits     := cfi_idx
    brinfo.cfi_taken        := is_taken
    brinfo.cfi_mispredicted := mispredict
    brinfo.cfi_type         := uop.br_type
    brinfo.is_jal           := is_jal
    brinfo.is_jalr          := is_jalr
    brinfo.target           := target_XLEN

    io.brinfo.valid:= (io.req.fire)&&(uop.uopc=/=uopAUIPC)&&(!killed)
    io.brinfo.bits := brinfo

    io.resp.valid := io.req.fire&&(!killed)
    io.resp.bits.wb_data := Mux(uop.uopc===uopAUIPC,op1_data+op2_data,op1_data+4.U)
}
//rocketchip 的面积为14000左右
class MULUnit(dataWidth: Int,numStages:Int)(implicit p: Parameters)
extends FunctionalUnit(
    isJmpUnit = false,
    dataWidth = dataWidth){
    io.req.ready := true.B
    val imul = Module(new PipelinedMultiplier(XLEN, numStages))
    val uops = Reg(Vec(numStages, Valid(new MicroOp)))
    uops(0).valid := io.req.valid&(!killed)
    uops(0).bits  := uop 
    for (i <- 1 until numStages) {
        uops(i).valid := uops(i-1).valid &&(!killed)
        uops(i).bits  := uops(i-1).bits
    }

    imul.io.req.valid    := io.req.valid
    imul.io.req.bits.fn  := io.req.bits.uop.ctrl.fcn_dw
    imul.io.req.bits.dw  := io.req.bits.uop.ctrl.op_fcn
    imul.io.req.bits.in1 := io.req.bits.rs1_data
    imul.io.req.bits.in2 := io.req.bits.rs2_data
    imul.io.req.bits.tag := DontCare


    // response
    io.resp.valid        := uops(numStages-1).valid&&(!killed)
    io.resp.bits.uop     := uops(numStages-1).bits
    io.resp.bits.wb_data    := imul.io.resp.bits.data

}
//7500
class DIVUnit(dataWidth: Int)(implicit p: Parameters)
extends FunctionalUnit(
    isJmpUnit = false,
    dataWidth = dataWidth){

    val div = Module(new freechips.rocketchip.rocket.MulDiv(mulDivParams, width = dataWidth))

    val req = Reg(Valid(new MicroOp()))

    when (io.req.fire) {
        req.valid := !killed
        req.bits  := io.req.bits.uop
    } .otherwise {
        req.valid := !killed && req.valid
    }
    when (reset.asBool||io.resp.fire) {
    req.valid := false.B
    }

    // request
    div.io.req.valid    := io.req.valid &&(!killed)
    div.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
    div.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
    div.io.req.bits.in1 := io.req.bits.rs1_data
    div.io.req.bits.in2 := io.req.bits.rs2_data
    div.io.req.bits.tag := DontCare
    io.req.ready        := div.io.req.ready 

    // handle pipeline kills and branch misspeculations
    div.io.kill         := (killed)

    // response
    div.io.resp.ready      := io.resp.ready
    io.resp.valid          := div.io.resp.valid 
    io.resp.bits.wb_data   := div.io.resp.bits.data
    io.resp.bits.uop       := req.bits

}