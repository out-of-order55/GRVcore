package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class MicroOp(implicit p: Parameters) extends GRVBundle
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

}
