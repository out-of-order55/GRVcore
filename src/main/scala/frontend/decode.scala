package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

import freechips.rocketchip.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.{CSR,Causes}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}
import grvcore.common._

/* 
对boom的decode table进行了裁剪：
1.目前没有打算做FPU，所以不用去管rs3
2.AMO指令目前不支持，但预留了控制信号
3.仅支持32位的指令
CtrlSigs也来自boom，本身信号命名就不错，所以直接拿来用
 */
abstract trait DecodeConstants
extends freechips.rocketchip.rocket.constants.ScalarOpConstants
with freechips.rocketchip.rocket.constants.MemoryOpConstants  
{
    val DC2 = BitPat.dontCare(2)
    def decode_default: List[BitPat] =
            //                                                                                    wakeup_delay
            //   is val inst?                                           imm sel                  |    bypassable (aka, known/fixed latency)
            //   |                                                      |  uses_ldq              |    |  is_br
            //   |                                      rs1 regtype     |  |  uses_stq           |    |  |
            //   |  micro-code                          |       rs2 type|  |  |  is_amo          |    |  |
            //   |  |         iq-type  func unit        |       |       |  |  |  |  is_fence     |    |  |
            //   |  |         |        |                |       |       |  |  |  |  |  is_fencei |    |  |  is breakpoint or ecall?
            //   |  |         |        |        dst     |       |       |  |  |  |  |  |  mem    |    |  |  |  is unique? (clear pipeline for it)
            //   |  |         |        |        regtype |       |       |  |  |  |  |  |  cmd    |    |  |  |  |  flush on commit
            //   |  |         |        |        |       |       |       |  |  |  |  |  |  |      |    |  |  |  |  |  csr cmd
            //   |  |         |        |        |       |       |       |  |  |  |  |  |  |      |    |  |  |  |  |  |
            List(N, uopX    , IQT_INT, FU_X   , RT_X  , DC2    ,DC2 ,IS_X, X, X, X, X, N, M_X,   DC2, X, X, N, N, X, CSR.X)

    val table: Array[(BitPat, List[BitPat])]
}
//get from boom
object XDecode extends DecodeConstants
{

    val table: Array[(BitPat, List[BitPat])] = Array(
    LW      -> List(Y, uopLD   , IQT_LD, FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N,N, N, N, M_XRD, 3.U, N, N, N, N, N, CSR.N),
    LH      -> List(Y, uopLD   , IQT_LD, FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N,N, N, N, M_XRD, 3.U, N, N, N, N, N, CSR.N),
    LHU     -> List(Y, uopLD   , IQT_LD, FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N,N, N, N, M_XRD, 3.U, N, N, N, N, N, CSR.N),
    LB      -> List(Y, uopLD   , IQT_LD, FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N,N, N, N, M_XRD, 3.U, N, N, N, N, N, CSR.N),
    LBU     -> List(Y, uopLD   , IQT_LD, FU_MEM , RT_FIX, RT_FIX, RT_X  , IS_I, Y, N,N, N, N, M_XRD, 3.U, N, N, N, N, N, CSR.N),

    SW      -> List(Y, uopSTA  , IQT_ST, FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y,N, N, N, M_XWR, 0.U, N, N, N, N, N, CSR.N),
    SH      -> List(Y, uopSTA  , IQT_ST, FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y,N, N, N, M_XWR, 0.U, N, N, N, N, N, CSR.N),
    SB      -> List(Y, uopSTA  , IQT_ST, FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_S, N, Y,N, N, N, M_XWR, 0.U, N, N, N, N, N, CSR.N),

    LUI     -> List(Y, uopLUI  , IQT_INT, FU_ALU , RT_FIX, RT_X  , RT_X  , IS_U, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),

    ADDI    -> List(Y, uopADDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    ANDI    -> List(Y, uopANDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    ORI     -> List(Y, uopORI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    XORI    -> List(Y, uopXORI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SLTI    -> List(Y, uopSLTI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SLTIU   -> List(Y, uopSLTIU, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),

    SLL     -> List(Y, uopSLL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    ADD     -> List(Y, uopADD  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SUB     -> List(Y, uopSUB  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SLT     -> List(Y, uopSLT  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SLTU    -> List(Y, uopSLTU , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    AND     -> List(Y, uopAND  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    OR      -> List(Y, uopOR   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    XOR     -> List(Y, uopXOR  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SRA     -> List(Y, uopSRA  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_I, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),
    SRL     -> List(Y, uopSRL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 1.U, Y, N, N, N, N, CSR.N),

    MUL     -> List(Y, uopMUL  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    MULH    -> List(Y, uopMULH , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    MULHU   -> List(Y, uopMULHU, IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    MULHSU  -> List(Y, uopMULHSU,IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),

    DIV     -> List(Y, uopDIV  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    DIVU    -> List(Y, uopDIVU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    REM     -> List(Y, uopREM  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),
    REMU    -> List(Y, uopREMU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, N, N, CSR.N),

    AUIPC   -> List(Y, uopAUIPC, IQT_INT, FU_JMP , RT_FIX, RT_X  , RT_X  , IS_U, N, N,N, N, N, M_X  , 1.U, N, N, N, N, N, CSR.N), // use BRU for the PC read
    JAL     -> List(Y, uopJAL  , IQT_INT, FU_JMP , RT_FIX, RT_X  , RT_X  , IS_J, N, N,N, N, N, M_X  , 1.U, N, N, N, N, N, CSR.N),
    JALR    -> List(Y, uopJALR , IQT_INT, FU_JMP , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 1.U, N, N, N, N, N, CSR.N),
    BEQ     -> List(Y, uopBEQ  , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),
    BNE     -> List(Y, uopBNE  , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),
    BGE     -> List(Y, uopBGE  , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),
    BGEU    -> List(Y, uopBGEU , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),
    BLT     -> List(Y, uopBLT  , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),
    BLTU    -> List(Y, uopBLTU , IQT_INT, FU_JMP , RT_X  , RT_FIX, RT_FIX, IS_B, N, N,N, N, N, M_X  , 0.U, N, Y, N, N, N, CSR.N),

    // I-type, the imma2 holds the CSR register.N,
    CSRRW   -> List(Y, uopCSRRW, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.W),
    CSRRS   -> List(Y, uopCSRRS, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.S),
    CSRRC   -> List(Y, uopCSRRC, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.C),

    CSRRWI  -> List(Y, uopCSRRWI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.W),
    CSRRSI  -> List(Y, uopCSRRSI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.S),
    CSRRCI  -> List(Y, uopCSRRCI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.C),

    // SFENCE_VMA->List(Y,uopSFENCE,IQT_LD, FU_MEM , RT_X  , RT_FIX, RT_FIX, IS_X, N, N,N, N, N,M_SFENCE,0.U,N, N, N, Y, Y, CSR.N),
    ECALL   -> List(Y, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, Y, Y, Y, CSR.I),
    EBREAK  -> List(Y, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, Y, Y, Y, CSR.I),
    SRET    -> List(Y, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.I),
    MRET    -> List(Y, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.I),
    DRET    -> List(Y, uopERET  ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_I, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.I),

    WFI     -> List(Y, uopWFI   ,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , IS_X, N, N,N, N, N, M_X  , 0.U, N, N, N, Y, Y, CSR.I),

    FENCE_I -> List(Y, uopNOP  , IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , IS_X, N, N,N, N, Y, M_X  , 0.U, N, N, N, Y, Y, CSR.N),
    FENCE   -> List(Y, uopFENCE, IQT_INT, FU_MEM , RT_X  , RT_X  , RT_X  , IS_X, N, Y,N, Y, N, M_X  , 0.U, N, N, N, Y, Y, CSR.N)
    )
}

class CtrlSigs(implicit p :Parameters)extends GRVBundle  
{
    val legal           = Bool()

    val uopc            = UInt(UOPC_SZ.W)
    val iq_type         = UInt(IQT_SZ.W)
    val fu_code         = UInt(FUC_SZ.W)
    val dst_type        = UInt(2.W)
    val rs1_type        = UInt(2.W)
    val rs2_type        = UInt(2.W)

    val imm_sel         = UInt(IS_X.getWidth.W)
    val uses_ldq        = Bool()
    val uses_stq        = Bool()
    val is_amo          = Bool()
    val is_fence        = Bool()
    val is_fencei       = Bool()
    val mem_cmd         = UInt(freechips.rocketchip.rocket.M_SZ.W)
    val wakeup_delay    = UInt(2.W)
    val bypassable      = Bool()
    val is_br           = Bool()
    val is_ecall        = Bool()
    val inst_unique     = Bool()
    val flush_on_commit = Bool()
    val csr_cmd         = UInt(freechips.rocketchip.rocket.CSR.SZ.W)


    def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
        //rocket的译码器
        val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decode_default, table)
        val sigs =
            Seq(legal, uopc, iq_type, fu_code, dst_type, rs1_type,
                rs2_type, imm_sel, uses_ldq, uses_stq, is_amo,
                is_fence, is_fencei, mem_cmd, wakeup_delay, bypassable,
                is_br, is_ecall, inst_unique, flush_on_commit, csr_cmd)
            sigs zip decoder map {case(s,d) => s := d}
            this
    }
}
class DecodeIO(implicit p:Parameters) extends GRVBundle{
    val enq = new Bundle { val uop = Input(new MicroOp()) }
    val deq = new Bundle { val uop = Output(new MicroOp()) }
}
/* 
area:870  
decode应该给出的信号：
1.寄存器类型，是否有效，idx
2.特殊指令的类型
3.mem指令的类型和访存大小
4.这个指令会被分配到哪个IQ，要进入哪个EXU
5.指令是否可以bypass
6.指令的执行延迟，推测唤醒可能有用
 */
class DecodeUnit(implicit p:Parameters) extends GRVModule
{
    val io   = IO(new DecodeIO())

    val uop = Wire(new MicroOp())
    uop := io.enq.uop

    val inst = uop.inst
    val cs = Wire(new CtrlSigs()).decode(inst, XDecode.table)
    uop.uopc       := cs.uopc
    uop.bypassable := cs.bypassable
    uop.dst_rtype  := cs.dst_type
    uop.exc_cause  := 0.U
    uop.exception  := false.B
    uop.flush_on_commit:= cs.flush_on_commit
    
    uop.fu_code    := cs.fu_code
    uop.iq_type    := cs.iq_type

    uop.is_amo     := cs.is_amo
    uop.is_ecall   := cs.is_ecall
    uop.is_fence   := cs.is_fence
    uop.is_fencei  := cs.is_fencei
    uop.is_unique  := cs.inst_unique

    uop.ldst       := inst(RD_MSB,RD_LSB)
    uop.lrs1       := inst(RS1_MSB,RS1_LSB)
    uop.lrs2       := inst(RS2_MSB,RS2_LSB)


    uop.ldst_val   := cs.dst_type =/= RT_X && !(uop.ldst === 0.U && uop.dst_rtype === RT_FIX)
    uop.dst_rtype  := cs.dst_type
    uop.lrs1_rtype := cs.rs1_type
    uop.lrs2_rtype := cs.rs2_type

    uop.mem_cmd    := cs.mem_cmd
    uop.mem_size   := Mux(cs.mem_cmd.isOneOf(M_SFENCE), Cat(uop.lrs2 =/= 0.U, uop.lrs1 =/= 0.U), inst(13,12))
    uop.mem_signed := !inst(14)
    uop.uses_ldq   := cs.uses_ldq
    uop.uses_stq   := cs.uses_stq
    uop.flush_on_commit := cs.flush_on_commit 

    uop.bypassable   := cs.bypassable
    val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, inst(11,7), inst(24,20))
    uop.imm_packed := Cat(inst(31,25), di24_20, inst(19,12))
    uop.is_br      := cs.is_br
    uop.is_jal     := uop.uopc===uopJAL
    uop.is_jalr    := uop.uopc===uopJALR
    uop.br_type    := Mux(uop.is_br,BR.U,Mux((uop.ldst===1.U||uop.ldst===5.U)&&(uop.is_jal||uop.is_jalr),
                    CALL.U,
                    Mux(uop.is_jalr&&(uop.ldst===0.U)&&(uop.lrs1===1.U||uop.lrs1===5.U),RET.U,0.U)))

    io.deq.uop := uop

}