import grvcore._
import circt.stage._
import org.chipsalliance.cde.config._

import chisel3._
import chisel3.{RawModule}
// import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SimTop (implicit p:Parameters)extends Module{
  
  val io = IO(new Bundle {})

  val cache = LazyModule(new CacheTest())
  val c = Module(cache.module)
  c.dontTouchPorts()



}
object Elaborate extends App {

    println("-----------------Generate Verilog--------------------")
    implicit val p:Parameters = new BaseConfig() 
    // val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x20000000, 0x1000)))
    val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _))
    // val add = LazyModule(new AdderTestHarness()(Parameters.empty))
    circt.stage.ChiselStage.emitSystemVerilogFile( new SimTop(), args, firtoolOptions)
  // DifftestModule.finish("Demo", false)

}