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

/* 
目前为简便处理，只允许ALU+JMP
MUL+DIV
 */
class ALUExuUnit(
    hasALU:Boolean=true,
    hasJMP:Boolean=false,
    hasMUL:Boolean=false,
    hasDIV:Boolean=false,
    hasCSR:Boolean=false,
)(implicit p: Parameters)extends GRVModule{
    val imulLatency = 3
    val iresp_fu_units = ArrayBuffer[FunctionalUnit]()
    def length = iresp_fu_units.length
    val numIrfReadPorts = Seq(hasALU,hasDIV,hasMUL,hasJMP,hasCSR).count(identity)
        // (if (hasALU) 1 else 0) + (if (hasDIV) 1 else 0) + (if (hasMUL) 1 else 0) + (if (hasJMP) 1 else 0)
    val numIrfWritePorts = Seq(hasALU,hasDIV,hasJMP,hasMUL,hasCSR).count(identity)
    

    val io = IO(new Bundle {
        val fu_types = Output(Bits(FUC_SZ.W))

        val req      = Flipped(new DecoupledIO(new ExuReq()))

        val iresp    = Vec(numIrfWritePorts,(new DecoupledIO(new ExuResp())))

        val get_ftq_pc = if (hasJMP) Flipped(new GetPCFromFtqIO()) else null
        val brupdate = if (hasJMP) Output(Valid(new BrUpdateInfo)) else null
        val flush    = Input(Bool())
    })

    

    for(i <- 0 until numIrfWritePorts){
        io.iresp(i).valid := false.B
        io.iresp(i).bits := DontCare
        if(!hasJMP){
        assert(io.iresp(i).ready)
        }
    }

    val div_bsy = RegInit(false.B)
    io.fu_types := Mux(hasALU.B, FU_ALU, 0.U) |
                    Mux(hasMUL.B, FU_MUL, 0.U) |
                    Mux(!div_bsy && hasDIV.B, FU_DIV, 0.U)|
                    Mux(hasJMP.B, FU_JMP, 0.U)|
                    Mux(hasCSR.B, FU_CSR, 0.U)
    if (hasALU) {
        val alu = Module(new ALUUnit(XLEN))
        alu.io <> DontCare
        alu.io.req.valid := io.req.valid &&io.req.bits.uop.fu_code === FU_ALU 


        alu.io.req.bits.uop      := io.req.bits.uop
        alu.io.req.bits.kill     := io.req.bits.kill||io.flush
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
        jmp.io.req.bits.kill     := io.req.bits.kill||io.flush
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
        mul.io.req.bits.kill     := io.req.bits.kill||io.flush
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

        div.io.req.bits.kill       := io.req.bits.kill||io.flush

        div.io.resp.ready := !(iresp_fu_units.map(_.io.resp.valid).reduce(_|_))

        div_resp_val := div.io.resp.valid
        div_bsy     := Mux(div.io.resp.valid||div.io.req.bits.kill,false.B,
                        Mux(io.req.fire&&io.req.bits.uop.fu_code===(FU_DIV),true.B,div_bsy))
                        

        iresp_fu_units += div
    }
    if(hasCSR){
        val csr = Module(new CSRUnit(XLEN))
        csr.io <> DontCare
        csr.io.req.valid := io.req.valid &&io.req.bits.uop.fu_code === FU_CSR 


        csr.io.req.bits.uop      := io.req.bits.uop
        csr.io.req.bits.kill     := io.req.bits.kill||io.flush
        csr.io.req.bits.rs1_data := io.req.bits.rs1_data
        csr.io.req.bits.rs2_data := io.req.bits.rs2_data
        csr.io.resp.ready := DontCare
        iresp_fu_units += csr
    }
    // io.iresp.bits.wb_data := PriorityMux(iresp_fu_units.map(f =>
    //     (f.io.resp.valid, f.io.resp.bits.wb_data)).toSeq)
    if(numIrfWritePorts==1){
        io.iresp(0).valid := iresp_fu_units(0).io.resp.valid
        io.iresp(0).bits  := iresp_fu_units(0).io.resp.bits
    }else{
        for(i <- 0 until numIrfWritePorts){
            if(hasJMP){
                require((!hasDIV)&&(!hasMUL)&&(hasALU))
            }
            require(numIrfWritePorts>=1)
            
            io.iresp(i).valid := iresp_fu_units(i).io.resp.valid
            io.iresp(i).bits  := iresp_fu_units(i).io.resp.bits
        }
    }
    def supportedFuncUnits = {
        new SupportedFuncUnits(
        alu = hasALU,
        jmp = hasJMP,
        mem = false,
        muld = hasMUL || hasDIV,
        csr = hasCSR
        )
    }
    io.req.ready := Mux(hasDIV.B,!div_bsy,true.B)
}