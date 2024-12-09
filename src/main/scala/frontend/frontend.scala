package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._


trait HasFrontendParameters extends HasICacheParameters{
    override val bankWidth: Int = fetchWidth
    def bankoffset(addr:UInt) = addr(offsetWidth-1,offsetWidth-bankWidth)
}