package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

trait HasFrontendParameters extends HasICacheParameters{
    override val bankWidth: Int = fetchWidth
    def bankoffset(addr:UInt) = addr(offsetWidth-1,offsetWidth-log2Ceil((XLEN/8)))
    def bankAlign(addr:UInt) =(addr>>bankWidth)<<bankWidth
}
class FetchBundle(implicit p: Parameters) extends GRVBundle with HasFrontendParameters
{
//指令信息
    val pc            = UInt(XLEN.W)
    val insts         = Vec(fetchWidth, Bits(32.W))


//分支信息
    val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
    val cfi_type      = UInt(2.W)
    val cfi_taken     = Bool()
    val is_jal        = Bool()
    val is_jalr       = Bool()
    val br_mask       = UInt(fetchWidth.W)
    val ghist         = UInt(globalHistoryLength.W)
    val bpd_meta      = UInt(bpdMaxMetaLength.W)

    val ftq_idx       = UInt(log2Ceil(ftqentries).W)
    val mask          = UInt(fetchWidth.W) // mark which words are valid instructions


    val bpSrc    = UInt(2.W)

}