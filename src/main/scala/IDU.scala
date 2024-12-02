package grvcore
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.UIntTransform
import INST.Instructions._
// import freechips.rocketchip.rocket._

// import CoreIO._
abstract trait DecodeConstants
{
  val table: Array[(BitPat, List[UInt])]
}
trait ScalarOpConstants {
  val SZ_BR = 3
  
  def BR_EQ   = 0.U(3.W)
  def BR_NE   = 1.U(3.W)
  def BR_J    = 2.U(3.W)
  def BR_N    = 3.U(3.W)
  def BR_LT   = 4.U(3.W)
  def BR_GE   = 5.U(3.W)
  def BR_LTU  = 6.U(3.W)
  def BR_GEU  = 7.U(3.W)
  def BR_X    = BR_EQ
  
  val SZ_A1 = 2
  def A1_ZERO = 0.U(2.W)
  def A1_RS1  = 1.U(2.W)
  def A1_PC   = 2.U(2.W)
  def A1_X    = 3.U(2.W)

  val SZ_IMM = 3
  def IMM_S  = 0.U(3.W)
  def IMM_SB = 1.U(3.W)
  def IMM_U  = 2.U(3.W)
  def IMM_UJ = 3.U(3.W)
  def IMM_I  = 4.U(3.W)
  def IMM_Z  = 5.U(3.W)
  def IMM_X  = 6.U(3.W)

  val SZ_A2 = 3
  def A2_ZERO = 0.U(3.W)
  def A2_SIZE = 1.U(3.W)
  def A2_RS2  = 2.U(3.W)
  def A2_IMM  = 3.U(3.W)
  def A2_X    = 4.U(3.W)

  val SZ_MEM = 4
  def M_X  = 0.U(4.W)
  def M_SB = 1.U(4.W)
  def M_SH = 2.U(4.W)
  def M_SW = 3.U(4.W)
  def M_LB = 4.U(4.W)
  def M_LH = 5.U(4.W)
  def M_LW = 6.U(4.W)
  def M_LBU= 7.U(4.W)
  def M_LHU= 8.U(4.W)


  def N = 0.U
  def Y = 1.U
}

trait ALUFN {
  val SZ_ALU_FN = 4
  def FN_X    = 0.U
  def FN_ADD  = 1.U
  def FN_SL   = 2.U
  def FN_SEQ  = 3.U
  def FN_SNE  = 4.U
  def FN_XOR  = 5.U
  def FN_SR   = 6.U
  def FN_OR   = 7.U
  def FN_AND  = 8.U
  def FN_SUB  = 9.U
  def FN_SRA  = 10.U
  def FN_SLT  = 11.U
  def FN_SGE  = 12.U
  def FN_SLTU = 13.U
  def FN_SGEU = 14.U

  // Mul/div reuse some integer FNs
  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}
class RegCtrlIO extends Bundle with ALUFN with ScalarOpConstants{
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = UInt(SZ_A2.W)
  val sel_alu1 = UInt(SZ_A1.W)
  val sel_imm = UInt(SZ_IMM.W)
}

object IDecode extends DecodeConstants with ScalarOpConstants with ALUFN
{
  val default=
                //           jal                                                                 renf1               fence.i
                //   val     | jalr                                                              | renf2             |
                //   | fp_val| | renx2                                                           | | renf3           |
                //   | | rocc| | | renx1       s_alu1                              mem_val       | | | wfd           |
                //   | | | br| | | |   s_alu2  |       imm    dw     alu           | mem_cmd     | | | | mul         |
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | div       | fence
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | | wxd     | | amo
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | | |       | | | dp
                List(N,N,N,N,N,N,  A2_X,   A1_X,   IMM_X,  FN_X,   N,M_X        )
  val table: Array[(BitPat, List[UInt])] = Array(
    BNE->       List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SNE,   N,M_X        ),
    BEQ->       List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SEQ,   N,M_X        ),
    BLT->       List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SLT,   N,M_X        ),
    BLTU->      List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SLTU,  N,M_X        ),
    BGE->       List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SGE,   N,M_X        ),
    BGEU->      List(Y,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,FN_SGEU,  N,M_X        ),

    JAL->       List(Y,N,Y,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,FN_ADD,   N,M_X        ),
    JALR->      List(Y,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   N,M_X        ),
    AUIPC->     List(Y,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, FN_ADD,   N,M_X        ),

    LB->        List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   Y,M_LB      ),
    LH->        List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   Y,M_LH      ),
    LW->        List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   Y,M_LW      ),
    LBU->       List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   Y,M_LBU      ),
    LHU->       List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   Y,M_LHU      ),
    SB->        List(Y,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, FN_ADD,   Y,M_SB      ),
    SH->        List(Y,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, FN_ADD,   Y,M_SH      ),
    SW->        List(Y,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, FN_ADD,   Y,M_SW      ),

    LUI->       List(Y,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, FN_ADD,   N,M_X        ),
    ADDI->      List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_ADD,   N,M_X        ),
    SLTI ->     List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_SLT,   N,M_X        ),
    SLTIU->     List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_SLTU,  N,M_X        ),
    ANDI->      List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_AND,   N,M_X        ),
    ORI->       List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_OR,    N,M_X        ),
    XORI->      List(Y,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, FN_XOR,   N,M_X        ),
    ADD->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_ADD,   N,M_X        ),
    SUB->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SUB,   N,M_X        ),
    SLT->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SLT,   N,M_X        ),
    SLTU->      List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SLTU,  N,M_X        ),
    AND->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_AND,   N,M_X        ),
    OR->        List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_OR,    N,M_X        ),
    XOR->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_XOR,   N,M_X        ),
    SLL->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SL,    N,M_X        ),
    SRL->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SR,    N,M_X        ),
    SRA->       List(Y,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, FN_SRA,   N,M_X        )
    // ECALL->     List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X        ),
    // EBREAK->    List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X        ),
    // MRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X        ),
    // CSRRW->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X        ),
    // CSRRS->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X        ),
    // CSRRC->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X        ),
    // CSRRWI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X        ),
    // CSRRSI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X        ),
    // CSRRCI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X        )
    )
}

object ImmGen extends ScalarOpConstants{
  def apply(sel:UInt,inst:UInt)={
    val sign = inst(31).asSInt
    val imm30_20 = Mux(sel===IMM_U,inst(30,20).asSInt,sign)
    val imm19_12 = Mux(sel===IMM_U||sel===IMM_UJ,inst(19,12).asSInt,sign)
    val imm11    = Mux(sel===IMM_SB,inst(7).asSInt,
                  Mux(sel===IMM_UJ,inst(20).asSInt,
                  Mux(sel===IMM_I||sel===IMM_S,sign,0.S)))
    val imm10_5  = Mux(sel===IMM_U,0.S,inst(30,25).asSInt)

    val imm4_1   = Mux(sel===IMM_S||sel===IMM_SB,inst(11,8).asSInt,
                  Mux(sel===IMM_I||sel===IMM_UJ,inst(24,21).asSInt,0.S))
    val imm0 = Mux(sel===IMM_I,inst(20).asSInt,
              Mux(sel===IMM_S,inst(7).asSInt,0.S))
    Cat(sign, imm30_20, imm19_12, imm11, imm10_5, imm4_1, imm0).asUInt
  }
}

class DecodeReg extends Module with ScalarOpConstants{
  val io = IO(new Bundle {
      val inst  = Input(UInt(32.W))
      val pc    = Input(UInt(32.W))
      val imm   = Input(UInt(32.W)) 
      val rd1   = Input(UInt(32.W))
      val rd2   = Input(UInt(32.W)) 
      val sel_alu2 = Input(UInt(SZ_A2.W))
      val sel_alu1 = Input(UInt(SZ_A1.W))
      val src1  = Output(UInt(32.W))
      val src2  = Output(UInt(32.W))  
  })
  io.src1 := Mux(io.sel_alu1===A1_PC,io.pc,
            Mux(io.sel_alu1===A1_RS1,io.rd1,0.U))
  io.src2 := Mux(io.sel_alu2===A2_RS2,io.rd2,
            Mux(io.sel_alu2===A2_IMM,io.imm,0.U))
}

class DecodeCtrl extends Module with ScalarOpConstants{
  val io = IO(new Bundle {
      val inst  = Input(UInt(32.W))
      // val vaild = Input(Bool())

      val issue = Output(new RegselMessage())
      val sel_imm   = Output(UInt(SZ_IMM.W))
      val out = Output(new DecodeCtrlIO)
  })
  val ctrlSignals =ListLookup(io.inst,IDecode.default,IDecode.table)
  
  io.out.legal := ctrlSignals(0)
  io.out.branch := ctrlSignals(1)
  io.out.jal :=ctrlSignals(2)
  io.out.jalr:=ctrlSignals(3)

  io.issue.RegfileReq.rxs1:=ctrlSignals(4)
  io.issue.RegfileReq.rxs2:=ctrlSignals(5)
  io.issue.RegfileReq.raddr1 := io.inst(19,15)
  io.issue.RegfileReq.raddr2 := io.inst(24,20)

  io.issue.sel_alu2:=ctrlSignals(6)
  io.issue.sel_alu1:=ctrlSignals(7)
  io.sel_imm:=ctrlSignals(8)

  io.out.alu_fn :=ctrlSignals(9)
  io.out.mem:=ctrlSignals(10)
  io.out.mem_cmd:=ctrlSignals(11)
  val mem_wen = io.out.mem&&(io.out.mem_cmd===M_SW||io.out.mem_cmd===M_SH||io.out.mem_cmd===M_SB)


  io.out.wen := (~(io.out.branch||mem_wen))&&(io.inst(11,7)=/=0.U)
  io.out.mul := false.B
  io.out.div := false.B
}


class IDU(xlen:Int)extends Module{
  val io = IO(new Bundle {
    val in   = (Flipped(Decoupled(new IfuMessage)))
    val out   = (Decoupled(new IduMessage))
  })
  val decodeCtrl = Module(new DecodeCtrl)
  decodeCtrl.io.inst    := io.in.bits.inst
  val imm = ImmGen(decodeCtrl.io.sel_imm,io.in.bits.inst)
  val id_ready_go = true.B
  
  io.out.valid := io.in.valid&&id_ready_go
  io.in.ready  := (!io.in.valid)||(io.out.ready&&id_ready_go)
  
  io.out.bits.pc   := io.in.bits.pc
  io.out.bits.inst := io.in.bits.inst
  io.out.bits.imm  := imm
  io.out.bits.issue <>decodeCtrl.io.issue
  io.out.bits.ctrl<>(decodeCtrl.io.out)

}