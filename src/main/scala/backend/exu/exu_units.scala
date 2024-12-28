package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable.{ArrayBuffer}
/* 
目前EXU准备两个：
1.ALU+MUL
2.ALU+JMP+DIV

然后就是LSU
 */


class ExuResp(implicit p: Parameters) extends GRVBundle{
    val uop        = new MicroOp
    val wb_data     = UInt(XLEN.W)
    
}
class ExuReq(implicit p: Parameters) extends GRVBundle{
    val uop        = new MicroOp
    val kill        = Bool()
    val rs1_data     = UInt(XLEN.W)
    val rs2_data     = UInt(XLEN.W)
}
abstract class ExecutionUnit(
    val isJmpUnit:Boolean = false
)(implicit p: Parameters) extends GRVModule
{

    val io = IO(new Bundle {
        val fu_types = Output(Bits(FUC_SZ.W))

        val req      = Flipped(new DecoupledIO(new ExuReq()))

        val iresp    = new DecoupledIO(new ExuResp())

        val get_ftq_pc = if (isJmpUnit) Flipped(new GetPCFromFtqIO()) else null
        val brupdate = if (isJmpUnit) Output(Valid(new BrUpdateInfo)) else null
    })

    io.req.ready := false.B


    io.iresp.valid := false.B
    io.iresp.bits := DontCare
    assert(io.iresp.ready)

}
/* 
目前为简便处理，只允许ALU+JMP
MUL+DIV
 */
class ALUExuUnit(
    hasALU:Boolean=true,
    hasJMP:Boolean=false,
    hasMUL:Boolean=false,
    hasDIV:Boolean=false,
)(implicit p: Parameters)extends ExecutionUnit(hasJMP){
    val imulLatency = 3
    val iresp_fu_units = ArrayBuffer[FunctionalUnit]()

    val div_bsy = WireInit(false.B)
    io.fu_types := Mux(hasALU.B, FU_ALU, 0.U) |
                    Mux(hasMUL.B, FU_MUL, 0.U) |
                    Mux(!div_bsy && hasDIV.B, FU_DIV, 0.U)|
                    Mux(hasJMP.B, FU_JMP, 0.U)
    if (hasALU) {
        val alu = Module(new ALUUnit(XLEN))
        alu.io <> DontCare
        alu.io.req.valid := io.req.valid &&io.req.bits.uop.fu_code === FU_ALU 


        alu.io.req.bits.uop      := io.req.bits.uop
        alu.io.req.bits.kill     := io.req.bits.kill
        alu.io.req.bits.rs1_data := io.req.bits.rs1_data
        alu.io.req.bits.rs2_data := io.req.bits.rs2_data
        alu.io.resp.ready := DontCare


        iresp_fu_units += alu
    }


    if (hasJMP) {
        val jmp = Module(new JMPUnit(XLEN))
        jmp.io <> DontCare
        jmp.io.req.valid := io.req.bits.uop.fu_code === FU_JMP &&io.req.valid


        jmp.io.req.bits.uop      := io.req.bits.uop
        jmp.io.req.bits.kill     := io.req.bits.kill
        jmp.io.req.bits.rs1_data := io.req.bits.rs1_data
        jmp.io.req.bits.rs2_data := io.req.bits.rs2_data

        jmp.io.resp.ready := DontCare


        iresp_fu_units += jmp


        // branch unit is embedded inside the ALU
        io.brupdate := jmp.io.brinfo

        jmp.io.get_ftq_pc <> io.get_ftq_pc
        
    }


    if (hasMUL) {
        val mul = Module(new MULUnit(XLEN,imulLatency))
        mul.io <> DontCare
        mul.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code===(FU_MUL)
        mul.io.req.bits.uop      := io.req.bits.uop
        mul.io.req.bits.rs1_data := io.req.bits.rs1_data
        mul.io.req.bits.rs2_data := io.req.bits.rs2_data
        mul.io.req.bits.kill     := io.req.bits.kill
        iresp_fu_units += mul
    }


    val div_resp_val = WireInit(false.B)
    if (hasDIV) {
        val  div = Module(new DIVUnit(XLEN))
        div.io <> DontCare
        div.io.req.valid           := io.req.valid && io.req.bits.uop.fu_code===(FU_DIV)
        div.io.req.bits.uop        := io.req.bits.uop
        div.io.req.bits.rs1_data   := io.req.bits.rs1_data
        div.io.req.bits.rs2_data   := io.req.bits.rs2_data

        div.io.req.bits.kill       := io.req.bits.kill

        div.io.resp.ready := !(iresp_fu_units.map(_.io.resp.valid).reduce(_|_))

        div_resp_val := div.io.resp.valid
        div_bsy     := !div.io.req.ready ||
                        (io.req.valid && io.req.bits.uop.fu_code===(FU_DIV))

        iresp_fu_units += div
    }
    io.iresp.valid     := iresp_fu_units.map(_.io.resp.valid).reduce(_|_)
    io.iresp.bits.uop  := PriorityMux(iresp_fu_units.map(f =>
        (f.io.resp.valid, f.io.resp.bits.uop)).toSeq)
    io.iresp.bits.wb_data := PriorityMux(iresp_fu_units.map(f =>
        (f.io.resp.valid, f.io.resp.bits.wb_data)).toSeq)
}