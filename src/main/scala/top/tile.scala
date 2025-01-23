package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.DontTouch
class Tile(implicit p: Parameters) extends LazyModule{
    lazy val module = new TileImp(this)
    // val icache = LazyModule(new ICacheWrapper)
    // val masterNode = icache.masterNode
    val xbar = AXI4Xbar()
    val lfrontend = LazyModule(new FrontEnd)
    val llsu      = LazyModule(new LSU)
    xbar := lfrontend.masterNode
    xbar := llsu.masterNode
    val masterNode = AXI4IdentityNode()
    masterNode :=* xbar
}
class TileImp(val outer: Tile)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasFrontendParameters with GRVOpConstants with DontTouch{
    val frontend = outer.lfrontend.module
    val lsu      = outer.llsu.module
    val core     = Module(new GRVCore)

    core.io.lsu <> lsu.io
    core.io.ifu <>frontend.io.cpu
    override def toString: String =
    (GRVString("====Overall Params====") + "\n"
    + frontend.toString+core.toString)
    print(toString)
}
