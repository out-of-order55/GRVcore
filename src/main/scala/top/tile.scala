package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket.CSR
// import org.chipsalliance.cde.config._
import freechips.rocketchip.util.DontTouch
import chisel3.util.circt.dpi._
class DifftestWrapper (implicit p:Parameters)extends GRVModule{

  val io = IO(new Bundle {
    val commit = Input(new CommitMsg)
  })
  val commit_wen  = WireInit(UInt(8.W),VecInit((io.commit.commit_uops zip io.commit.valid map{ case(a,b)=>
                      a.ldst_val&&b})).asUInt)
  dontTouch(commit_wen)
  val commit_cnt= RegInit(0.U(32.W))
  val commit_finish = (0 until coreWidth).map{i=>
    io.commit.commit_uops(i).ctrl.csr_cmd===CSR.I&&io.commit.valid(i)
  }.reduce(_||_)
  val commit_addr = WireInit(UInt(32.W),VecInit(io.commit.commit_uops.map(_.ldst)).asUInt)
  val commit_valid = WireInit(UInt(8.W),(io.commit.valid).asUInt)
  val commit_data = WireInit(UInt((XLEN*coreWidth).W),Cat(io.commit.commit_uops.map(_.wb_data).reverse))
  val commit_pc   = WireInit(UInt((XLEN*coreWidth).W),Cat(io.commit.commit_uops.map(_.pc).reverse))
  val commit_str  =  Seq("commit_wen","commit_addr","commit_valid","commit_data","commit_pc","commit_timeout","commit_finish")
  commit_cnt := Mux(commit_valid=/=0.U,0.U,commit_cnt+1.U)
  val commit_timeout = commit_cnt===2000.U
  val result      = RawClockedVoidFunctionCall("commit_event",Some(commit_str))(clock, true.B,commit_wen,commit_addr,commit_valid,commit_data,commit_pc,commit_timeout,commit_finish)

}
class SimTop (implicit p:Parameters)extends Module{
  val tile = LazyModule(new TileTest)
  val m = Module(tile.module)
  m.dontTouchPorts()
}
class Tile(implicit p: Parameters) extends LazyModule{
    lazy val module = new TileImp(this)
    // val icache = LazyModule(new ICacheWrapper)
    // val masterNode = icache.masterNode
    val xbar = GRVXbar(maxFlightPerId=1)
    val lfrontend = LazyModule(new FrontEnd)
    val llsu      = LazyModule(new LSU)
    xbar := lfrontend.masterNode
    // dontTouch(lfrontend.masterNode.)
    xbar := llsu.masterNode
    val masterNode = AXI4IdentityNode()
    masterNode :=* xbar
}

class TileImp(val outer: Tile)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasFrontendParameters with GRVOpConstants with DontTouch{
    val frontend = outer.lfrontend.module
    val lsu      = outer.llsu.module
    val core     = Module(new GRVCore)
    val (f_master, _) = outer.lfrontend.masterNode.out(0)
    dontTouch(f_master)
    core.io.lsu <> lsu.io
    core.io.ifu <>frontend.io.cpu
    override def toString: String =
    (GRVString("====Overall Params====") + "\n"
    + frontend.toString+core.toString)
    print(toString)
}
