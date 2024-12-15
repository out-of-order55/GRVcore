package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

trait GRVOpConstants
{
    val reset_vector = "h80000000".U
    val UOPC_SZ = 7

    val BR   = 1.U
    val CALL = 2.U
    val RET  = 3.U
}