package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

trait GRVOpConstants
{
    val reset_vector = "h80000000"
    val UOPC_SZ = 7

    val BR   = 1
    val CALL = 2
    val RET  = 3
}