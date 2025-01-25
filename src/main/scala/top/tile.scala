package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.DontTouch
import difftest._

class SimTop (implicit p:Parameters)extends Module{


  // val difftest = DifftestModule(new DiffInstrCommit, delay = 1, dontCare = true)
  // difftest.valid  := 1.U
  // difftest.pc     := 1.U
  // difftest.instr  := 1.U
  // difftest.skip   := 1.U
  // difftest.isRVC  := 1.U
  // difftest.rfwen  := 1.U
  // difftest.wdest  := 1.U
  // difftest.wpdest := 1.U
    // for differential testing
    class CSRDiffWrapper extends Module {
      val io = IO(new Bundle {
        val csrState = Input(new DiffCSRState)
        val archEvent = Input(new DiffArchEvent)
      })

      val difftest = DifftestModule(new DiffCSRState)
      difftest := RegNext(io.csrState)
      difftest.coreid := 0.U // TODO

      val difftestArchEvent = DifftestModule(new DiffArchEvent)
      difftestArchEvent := RegNext(RegNext(io.archEvent))
      difftestArchEvent.coreid := 0.U // TODO
    }

    val difftestreg = DifftestModule(new DiffArchIntRegState)
    difftestreg.coreid := 0.U // TODO
    difftestreg.value  := VecInit.fill(32)(0.U)

    val difftesttrap = DifftestModule(new DiffTrapEvent)
    difftesttrap.coreid   := 0.U
    difftesttrap.hasTrap  := 0.U
    difftesttrap.code     := 0.U
    difftesttrap.pc       := 0.U
    difftesttrap.cycleCnt := 0.U
    difftesttrap.instrCnt := 0.U
    difftesttrap.hasWFI   := 0.U

    val difftest_commit = DifftestModule(new DiffInstrCommit, delay = 1, dontCare = true)
    difftest_commit.coreid := 0.U
    difftest_commit.index  := 0.U
    difftest_commit.valid  := 0.U
    difftest_commit.pc     := 0.U
    difftest_commit.instr  := 0.U
    difftest_commit.skip   := 0.U
    difftest_commit.isRVC  := 0.U
    difftest_commit.rfwen  := 0.U
    difftest_commit.fpwen  := 0.U
    difftest_commit.wdest  := 0.U
    difftest_commit.wpdest := 0.U
    val diffWrapper = Module(new CSRDiffWrapper).io
    diffWrapper := DontCare

    val difftestcsr = diffWrapper.csrState
    difftestcsr.privilegeMode := 0.U
    difftestcsr.mstatus     := 0.U
    difftestcsr.sstatus     := 0.U
    difftestcsr.mepc        := 0.U
    difftestcsr.sepc        := 0.U
    difftestcsr.mtval       := 0.U
    difftestcsr.stval       := 0.U
    difftestcsr.mtvec       := 0.U
    difftestcsr.stvec       := 0.U
    difftestcsr.mcause      := 0.U
    difftestcsr.scause      := 0.U
    difftestcsr.satp        := 0.U
    difftestcsr.mip         := 0.U
    difftestcsr.mie         := 0.U
    difftestcsr.mscratch    := 0.U
    difftestcsr.sscratch    := 0.U
    difftestcsr.mideleg     := 0.U
    difftestcsr.medeleg     := 0.U

    val difftestArchEvent = diffWrapper.archEvent
    difftestArchEvent.valid         := 0.U
    difftestArchEvent.interrupt     := 0.U
    difftestArchEvent.exception     := 0.U
    difftestArchEvent.exceptionPC   := 0.U
    difftestArchEvent.exceptionInst := 0.U

    val difftest = DifftestModule.finish("test")
    // difftest.uart := DontCare

}
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
