package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// from boom


//from boom
trait RISCVConstants
{
    // abstract out instruction decode magic numbers
    val RD_MSB  = 11
    val RD_LSB  = 7
    val RS1_MSB = 19
    val RS1_LSB = 15
    val RS2_MSB = 24
    val RS2_LSB = 20
    val RS3_MSB = 31
    val RS3_LSB = 27

    val CSR_ADDR_MSB = 31
    val CSR_ADDR_LSB = 20
    val CSR_ADDR_SZ = 12

    // location of the fifth bit in the shamt (for checking for illegal ops for SRAIW,etc.)
    val SHAMT_5_BIT = 25
    val LONGEST_IMM_SZ = 20
    val X0 = 0.U
    val RA = 1.U // return address register

  // memory consistency model
  // The C/C++ atomics MCM requires that two loads to the same address maintain program order.
  // The Cortex A9 does NOT enforce load/load ordering (which leads to buggy behavior).
    val MCM_ORDER_DEPENDENT_LOADS = true

    val jal_opc = (0x6f).U
    val jalr_opc = (0x67).U

    def GetUop(inst: UInt): UInt = inst(6,0)
    def GetRd (inst: UInt): UInt = inst(RD_MSB,RD_LSB)
    def GetRs1(inst: UInt): UInt = inst(RS1_MSB,RS1_LSB)


}

trait GRVOpConstants
{

    val reset_vector = "h80000000"
    
    // val X = BitPat("b?")
    // val Y = BitPat("b1")
    // val N = BitPat("b0")
    val maxPregSz = 64
    val lregSz    = 5
    val BR   = 1
    val CALL = 2
    val RET  = 3

    val FUC_SZ = 10
    val FU_X   = BitPat.dontCare(FUC_SZ)
    val FU_ALU =   1.U(FUC_SZ.W)
    val FU_JMP =   2.U(FUC_SZ.W)
    val FU_MEM =   4.U(FUC_SZ.W)
    val FU_MUL =   8.U(FUC_SZ.W)
    val FU_DIV =  16.U(FUC_SZ.W)
    val FU_CSR =  32.U(FUC_SZ.W)
    val FU_FPU =  64.U(FUC_SZ.W)
    val FU_FDV = 128.U(FUC_SZ.W)
    val FU_I2F = 256.U(FUC_SZ.W)
    val FU_F2I = 512.U(FUC_SZ.W)

    val IQT_SZ  = 3
    val IQT_INT = 1.U(IQT_SZ.W)
    val IQT_MEM = 2.U(IQT_SZ.W)
    val IQT_FP  = 4.U(IQT_SZ.W)
    val IQT_MFP = 6.U(IQT_SZ.W)

    val IS_I   = 0.U(3.W)  // I-Type  (LD,ALU)
    val IS_S   = 1.U(3.W)  // S-Type  (ST)
    val IS_B   = 2.U(3.W)  // SB-Type (BR)
    val IS_U   = 3.U(3.W)  // U-Type  (LUI/AUIPC)
    val IS_J   = 4.U(3.W)  // UJ-Type (J/JAL)
    val IS_X   = BitPat("b???")

    val RT_FIX   = 0.U(2.W)
    val RT_FLT   = 1.U(2.W)
    val RT_PAS   = 3.U(2.W) // pass-through (prs1 := lrs1, etc)
    val RT_X     = 2.U(2.W) 


    val UOPC_SZ 	= 7
    val uopX    	= BitPat.dontCare(UOPC_SZ)
    val uopNOP  	=  0.U(UOPC_SZ.W)
    val uopLD   	=  1.U(UOPC_SZ.W)
    val uopSTA  	=  2.U(UOPC_SZ.W)  // store address generation
    val uopSTD  	=  3.U(UOPC_SZ.W)  // store data generation
    val uopLUI  	=  4.U(UOPC_SZ.W)

    val uopADDI 	=  5.U(UOPC_SZ.W)
    val uopANDI 	=  6.U(UOPC_SZ.W)
    val uopORI  	=  7.U(UOPC_SZ.W)
    val uopXORI 	=  8.U(UOPC_SZ.W)
    val uopSLTI 	=  9.U(UOPC_SZ.W)
    val uopSLTIU	= 10.U(UOPC_SZ.W)
    val uopSLLI 	= 11.U(UOPC_SZ.W)
    val uopSRAI 	= 12.U(UOPC_SZ.W)
    val uopSRLI 	= 13.U(UOPC_SZ.W)

    val uopSLL  	= 14.U(UOPC_SZ.W)
    val uopADD  	= 15.U(UOPC_SZ.W)
    val uopSUB  	= 16.U(UOPC_SZ.W)
    val uopSLT  	= 17.U(UOPC_SZ.W)
    val uopSLTU 	= 18.U(UOPC_SZ.W)
    val uopAND  	= 19.U(UOPC_SZ.W)
    val uopOR   	= 20.U(UOPC_SZ.W)
    val uopXOR  	= 21.U(UOPC_SZ.W)
    val uopSRA  	= 22.U(UOPC_SZ.W)
    val uopSRL  	= 23.U(UOPC_SZ.W)

    val uopBEQ  	= 24.U(UOPC_SZ.W)
    val uopBNE  	= 25.U(UOPC_SZ.W)
    val uopBGE  	= 26.U(UOPC_SZ.W)
    val uopBGEU 	= 27.U(UOPC_SZ.W)
    val uopBLT  	= 28.U(UOPC_SZ.W)
    val uopBLTU 	= 29.U(UOPC_SZ.W)
    val uopCSRRW	= 30.U(UOPC_SZ.W)
    val uopCSRRS	= 31.U(UOPC_SZ.W)
    val uopCSRRC	= 32.U(UOPC_SZ.W)
    val uopCSRRWI	=33.U(UOPC_SZ.W)
    val uopCSRRSI	=34.U(UOPC_SZ.W)
    val uopCSRRCI	=35.U(UOPC_SZ.W)

    val uopJ   	 	= 36.U(UOPC_SZ.W)
    val uopJAL 	 	= 37.U(UOPC_SZ.W)
    val uopJALR 	= 38.U(UOPC_SZ.W)
    val uopAUIPC	= 39.U(UOPC_SZ.W)

    //val uopSRET = 40.U(UOPC_SZ.W)
    val uopCFLSH	= 41.U(UOPC_SZ.W)
    val uopFENCE	= 42.U(UOPC_SZ.W)

    // val uopADDIW= 43.U(UOPC_SZ.W)
    // val uopADDW = 44.U(UOPC_SZ.W)
    // val uopSUBW = 45.U(UOPC_SZ.W)
    // val uopSLLIW= 46.U(UOPC_SZ.W)
    // val uopSLLW = 47.U(UOPC_SZ.W)
    // val uopSRAIW= 48.U(UOPC_SZ.W)
    // val uopSRAW = 49.U(UOPC_SZ.W)
    // val uopSRLIW= 50.U(UOPC_SZ.W)
    // val uopSRLW = 51.U(UOPC_SZ.W)
    val uopMUL  	= 52.U(UOPC_SZ.W)
    val uopMULH 	= 53.U(UOPC_SZ.W)
    val uopMULHU	= 54.U(UOPC_SZ.W)
    val uopMULHSU	=55.U(UOPC_SZ.W)
    // val uopMULW = 56.U(UOPC_SZ.W)
    val uopDIV  	= 57.U(UOPC_SZ.W)
    val uopDIVU 	= 58.U(UOPC_SZ.W)
    val uopREM  	= 59.U(UOPC_SZ.W)
    val uopREMU 	= 60.U(UOPC_SZ.W)
    // val uopDIVW = 61.U(UOPC_SZ.W)
    // val uopDIVUW= 62.U(UOPC_SZ.W)
    // val uopREMW = 63.U(UOPC_SZ.W)
    // val uopREMUW= 64.U(UOPC_SZ.W)

    val uopFENCEI    =  65.U(UOPC_SZ.W)
    //               =  66.U(UOPC_SZ.W)
    val uopAMO_AG    =  67.U(UOPC_SZ.W) 


    val uopWFI       = 105.U(UOPC_SZ.W) 
    val uopERET      = 106.U(UOPC_SZ.W) 
    val uopSFENCE    = 107.U(UOPC_SZ.W)


}