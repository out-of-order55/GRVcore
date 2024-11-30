import Cache._
import ysyx._
import chisel3._

import freechips.rocketchip.diplomacy._
// import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
object Elaborate extends App {
  val config = grvcoreConfig()
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).reduce(_ + "," + _))
  val add = LazyModule(new AdderTestHarness()(Parameters.empty))
  circt.stage.ChiselStage.emitSystemVerilogFile( new top, args, firtoolOptions)

}
