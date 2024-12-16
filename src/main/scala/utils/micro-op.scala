package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class MicroOp(implicit p: Parameters) extends GRVBundle with 
freechips.rocketchip.rocket.constants.MemoryOpConstants
{
    //译码信息
    val uopc             = UInt(UOPC_SZ.W)       
    //pc的偏移
    val pc_off           = UInt(log2Ceil(blockBytes).W)
    val inst             = UInt(32.W)

    //分支预测和类型
    val is_br            = Bool()                      
    val is_jalr          = Bool()                      
    val is_jal           = Bool()                     
    val taken            = Bool()
    //指令唯一标示
    val ftq_idx          = UInt(log2Ceil(ftqentries).W)
    val iq_type          = UInt(IQT_SZ.W)        // which issue unit do we use?
    val fu_code          = UInt(FUC_SZ.W) // which functional unit do we use?
    // val ctrl             = new CtrlSignals// for exu







    val imm_packed       = UInt(LONGEST_IMM_SZ.W) 

    // val rob_idx          = UInt(robAddrSz.W)
    // val ldq_idx          = UInt(ldqAddrSz.W)
    // val stq_idx          = UInt(stqAddrSz.W)
    // val rxq_idx          = UInt(log2Ceil(numRxqEntries).W)

    val pdst             = UInt(maxPregSz.W)
    val prs1             = UInt(maxPregSz.W)
    val prs2             = UInt(maxPregSz.W)


    val prs1_busy        = Bool()
    val prs2_busy        = Bool()

    val stale_pdst       = UInt(maxPregSz.W)
    val exception        = Bool()
    val exc_cause        = UInt(XLEN.W)         
    val bypassable       = Bool()                     
    val mem_cmd          = UInt(M_SZ.W)
    val mem_size         = UInt(2.W)
    val mem_signed       = Bool()
    val is_fence         = Bool()
    val is_fencei        = Bool()
    val is_amo           = Bool()
    val uses_ldq         = Bool()
    val uses_stq         = Bool()
    val is_ecall         = Bool()                      
    val is_unique        = Bool()                      
    val flush_on_commit  = Bool()                     



    val ldst             = UInt(lregSz.W)
    val lrs1             = UInt(lregSz.W)
    val lrs2             = UInt(lregSz.W)
    val lrs3             = UInt(lregSz.W)

    val ldst_val         = Bool()              
    val dst_rtype        = UInt(2.W)
    val lrs1_rtype       = UInt(2.W)
    val lrs2_rtype       = UInt(2.W)


    // def rf_wen           = dst_rtype =/= RT_X


}
