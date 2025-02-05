package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.CSR
import freechips.rocketchip.rocket.ALU._
/* 
本模块引用于boom，为了在执行阶段更好的选择操作数

 */
class RRdCtrlSigs(implicit p: Parameters) extends GRVBundle
{
    val br_type          = UInt(BJP_N.getWidth.W)
    val op_fcn      = Bits(SZ_ALU_FN.W)
    val fcn_dw      = Bool()
    val op1_sel     = UInt(OP1_X.getWidth.W)
    val op2_sel     = UInt(OP2_X.getWidth.W)
    val imm_sel     = UInt(IS_X.getWidth.W)
    val rf_wen      = Bool()
    val csr_cmd     = Bits(CSR.SZ.W)

    def decode(uopc: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
        val decoder = freechips.rocketchip.rocket.DecodeLogic(uopc, AluRRdDecode.default, table)
        val sigs = Seq(br_type, op_fcn,
                    fcn_dw, op1_sel, op2_sel, imm_sel, rf_wen, csr_cmd)
        sigs zip decoder map {case(s,d) => s := d}
        this
    }
}

/**
 * Default register read constants
 */
abstract trait RRdDecodeConstants
{
    val default: List[BitPat] =
                List[BitPat](BR_N , FN_ADD , dw_X  , OP1_X   , OP2_X   , IS_X, REN_0, CSR.N)
    val table: Array[(BitPat, List[BitPat])]
}

/**
 * ALU register read constants
 */
object AluRRdDecode extends RRdDecodeConstants
{
    val table: Array[(BitPat, List[BitPat])] =
                Array[(BitPat, List[BitPat])](
                                    // br type
                                    // |      pipe                    op1 sel   op2 sel
                                    // |     muldiv pipe              |         |         immsel       csr_cmd
                                    // |     se mem pipe              |         |         |     rf wen |
                                    // |       alu fcn        wd/word?|         |         |     |      |
                                    // |       |              |       |         |         |     |      |
            BitPat(uopLUI)   -> List(BJP_N ,  FN_ADD , dw_XPR, OP1_ZERO, OP2_IMM , IS_U, REN_1, CSR.N),
            BitPat(uopADDI)  -> List(BJP_N ,  FN_ADD , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopANDI)  -> List(BJP_N ,  FN_AND , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopORI)   -> List(BJP_N ,  FN_OR  , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopXORI)  -> List(BJP_N ,  FN_XOR , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopSLTI)  -> List(BJP_N ,  FN_SLT , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopSLTIU) -> List(BJP_N ,  FN_SLTU, dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopSLLI)  -> List(BJP_N ,  FN_SL  , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopSRAI)  -> List(BJP_N ,  FN_SRA , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopSRLI)  -> List(BJP_N ,  FN_SR  , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_1, CSR.N),
            BitPat(uopADD)   -> List(BJP_N ,  FN_ADD , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSLL)   -> List(BJP_N ,  FN_SL  , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSUB)   -> List(BJP_N ,  FN_SUB , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSLT)   -> List(BJP_N ,  FN_SLT , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSLTU)  -> List(BJP_N ,  FN_SLTU, dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopAND)   -> List(BJP_N ,  FN_AND , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopOR)    -> List(BJP_N ,  FN_OR  , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopXOR)   -> List(BJP_N ,  FN_XOR , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSRA)   -> List(BJP_N ,  FN_SRA , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopSRL)   -> List(BJP_N ,  FN_SR  , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N))
}

object JmpRRdDecode extends RRdDecodeConstants
{
    val table: Array[(BitPat, List[BitPat])] =
                Array[(BitPat, List[BitPat])](
                                // br type
                                // |      use alu pipe                    op1 sel   op2 sel
                                // |      |  use muldiv pipe              |         |         immsel       csr_cmd
                                // |      |  |  use mem pipe              |         |         |     rf wen |
                                // |      |  |  |  alu fcn        wd/word?|         |         |     |      |
                                // |      |  |  |  |              |       |         |         |     |      |
            BitPat(uopJAL)   -> List(BJP_J ,  FN_ADD , dw_XPR, OP1_PC  , OP2_NEXT, IS_J, REN_1, CSR.N),
            BitPat(uopJALR)  -> List(BJP_JR,  FN_ADD , dw_XPR, OP1_PC  , OP2_NEXT, IS_I, REN_1, CSR.N),
            BitPat(uopAUIPC) -> List(BJP_N ,  FN_ADD , dw_XPR, OP1_PC  , OP2_IMM , IS_U, REN_1, CSR.N),
            BitPat(uopBEQ)   -> List(BJP_EQ , FN_SUB , dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
            BitPat(uopBNE)   -> List(BJP_NE , FN_SUB , dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
            BitPat(uopBGE)   -> List(BJP_GE , FN_SLT , dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
            BitPat(uopBGEU)  -> List(BJP_GEU, FN_SLTU, dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
            BitPat(uopBLT)   -> List(BJP_LT , FN_SLT , dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N),
            BitPat(uopBLTU)  -> List(BJP_LTU, FN_SLTU, dw_XPR, OP1_X   , OP2_X   , IS_B, REN_0, CSR.N))
}

/**
 * Multiply divider register read constants
 */
object MulDivRRdDecode extends RRdDecodeConstants
{
    val table: Array[(BitPat, List[BitPat])] =
                Array[(BitPat, List[BitPat])](
                                // br type
                                // |      use alu pipe                    op1 sel   op2 sel
                                // |      |  use muldiv pipe              |         |         immsel       csr_cmd
                                // |      |  |  use mem pipe              |         |         |     rf wen |
                                // |      |  |  |  alu fcn        wd/word?|         |         |     |      |
                                // |      |  |  |  |              |       |         |         |     |      |
            BitPat(uopMUL)   -> List(BJP_N ,  FN_MUL,   dw_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
            BitPat(uopMULH)  -> List(BJP_N ,  FN_MULH,  dw_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
            BitPat(uopMULHU) -> List(BJP_N ,  FN_MULHU, dw_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
            BitPat(uopMULHSU)-> List(BJP_N ,  FN_MULHSU,dw_XPR,OP1_RS1 , OP2_RS2 , IS_X,  REN_1,CSR.N),
            BitPat(uopDIV)   -> List(BJP_N ,  FN_DIV , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopDIVU)  -> List(BJP_N ,  FN_DIVU, dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopREM)   -> List(BJP_N ,  FN_REM , dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N),
            BitPat(uopREMU)  -> List(BJP_N ,  FN_REMU, dw_XPR, OP1_RS1 , OP2_RS2 , IS_X, REN_1, CSR.N))
}

/**
 * Memory unit register read constants
 */
object MemRRdDecode extends RRdDecodeConstants
{
    val table: Array[(BitPat, List[BitPat])] =
                Array[(BitPat, List[BitPat])](
                                // br type
                                // |      use alu pipe                    op1 sel   op2 sel
                                // |      |  use muldiv pipe              |         |         immsel       csr_cmd
                                // |      |  |  use mem pipe              |         |         |     rf wen |
                                // |      |  |  |  alu fcn        wd/word?|         |         |     |      |
                                // |      |  |  |  |              |       |         |         |     |      |
            BitPat(uopLD)    -> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_IMM , IS_I, REN_0, CSR.N),
            BitPat(uopSTA)   -> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_IMM , IS_S, REN_0, CSR.N),
            BitPat(uopSTD)   -> List(BJP_N , FN_X   , dw_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, CSR.N),
            BitPat(uopSFENCE)-> List(BJP_N , FN_X   , dw_X  , OP1_RS1 , OP2_RS2 , IS_X, REN_0, CSR.N),

            BitPat(uopAMO_AG)-> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_ZERO, IS_X, REN_0, CSR.N))
}

/**
 * CSR register read constants
 */
object CsrRRdDecode extends RRdDecodeConstants
{
    val table: Array[(BitPat, List[BitPat])] =
                Array[(BitPat, List[BitPat])](
                                // br type
                                // |      use alu pipe                    op1 sel   op2 sel
                                // |      |  use muldiv pipe              |         |         immsel       csr_cmd
                                // |      |  |  use mem pipe              |         |         |     rf wen |
                                // |      |  |  |  alu fcn        wd/word?|         |         |     |      |
                                // |      |  |  |  |              |       |         |         |     |      |
            BitPat(uopCSRRW) -> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.W),
            BitPat(uopCSRRS) -> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.S),
            BitPat(uopCSRRC) -> List(BJP_N , FN_ADD , dw_XPR, OP1_RS1 , OP2_ZERO, IS_I, REN_1, CSR.C),

            BitPat(uopCSRRWI)-> List(BJP_N , FN_ADD , dw_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.W),
            BitPat(uopCSRRSI)-> List(BJP_N , FN_ADD , dw_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.S),
            BitPat(uopCSRRCI)-> List(BJP_N , FN_ADD , dw_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_1, CSR.C),

            BitPat(uopWFI)   -> List(BJP_N , FN_ADD , dw_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_0, CSR.I),
            BitPat(uopERET)  -> List(BJP_N , FN_ADD , dw_XPR, OP1_ZERO, OP2_IMMC, IS_I, REN_0, CSR.I))
}

/* 
和issue width挂钩，每个issuewidth都对应一个
 */
class RegisterReadDecode(supportedUnits: SupportedFuncUnits)(implicit p: Parameters) extends GRVModule
with freechips.rocketchip.rocket.constants.MemoryOpConstants
{
    val io = IO(new Bundle {
        val iss_valid = Input(Bool())
        val iss_uop   = Input(new MicroOp())

        val rrd_valid = Output(Bool())
        val rrd_uop   = Output(new MicroOp())
    })

    // Issued Instruction
    val rrd_valid = io.iss_valid
    io.rrd_uop   := io.iss_uop

    var dec_table = AluRRdDecode.table
    if (supportedUnits.jmp) dec_table ++= JmpRRdDecode.table
    if (supportedUnits.mem) dec_table ++= MemRRdDecode.table
    if (supportedUnits.muld) dec_table ++= MulDivRRdDecode.table
    if (supportedUnits.csr) dec_table ++= CsrRRdDecode.table
    val rrd_cs = Wire(new RRdCtrlSigs()).decode(io.rrd_uop.uopc, dec_table)

    // rrd_use_alupipe is unused
    io.rrd_uop.ctrl.br_type := rrd_cs.br_type
    io.rrd_uop.ctrl.op1_sel := rrd_cs.op1_sel
    io.rrd_uop.ctrl.op2_sel := rrd_cs.op2_sel
    io.rrd_uop.ctrl.imm_sel := rrd_cs.imm_sel
    io.rrd_uop.ctrl.op_fcn  := rrd_cs.op_fcn.asUInt
    io.rrd_uop.ctrl.fcn_dw  := rrd_cs.fcn_dw.asBool
    io.rrd_uop.ctrl.is_load := io.rrd_uop.uopc === uopLD
    io.rrd_uop.ctrl.is_sta  := io.rrd_uop.uopc === uopSTA || io.rrd_uop.uopc === uopAMO_AG
    io.rrd_uop.ctrl.is_std  := io.rrd_uop.uopc === uopSTD || (io.rrd_uop.ctrl.is_sta && io.rrd_uop.lrs2_rtype === RT_FIX)

    when (io.rrd_uop.uopc === uopAMO_AG || (io.rrd_uop.uopc === uopLD && io.rrd_uop.mem_cmd === M_XLR)) {
        io.rrd_uop.imm_packed := 0.U
    }

    val raddr1 = io.rrd_uop.prs1 // although renamed, it'll stay 0 if lrs1 = 0
    val csr_ren = (rrd_cs.csr_cmd === CSR.S || rrd_cs.csr_cmd === CSR.C) && raddr1 === 0.U
    io.rrd_uop.ctrl.csr_cmd := Mux(csr_ren, CSR.R, rrd_cs.csr_cmd)

    //-------------------------------------------------------------
    // set outputs

    io.rrd_valid := rrd_valid
}