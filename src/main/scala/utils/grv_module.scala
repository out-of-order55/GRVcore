package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

///////////////////////////Parameters//////////////////////
import org.chipsalliance.cde.config._

abstract class GRVModule(implicit val p:Parameters) extends Module with HasGRVParameters 
abstract class GRVBundle(implicit val p:Parameters) extends Bundle with HasGRVParameters 
object CPUAXI4BundleParameters {
    def apply() = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
}
