// package  top
import Cache._
import ysyx._
import chisel3._
// import difftest._


import circt.stage._
object Elaborate extends App {
  
  println("-----------------Generate Verilog--------------------")
  val config = grvcoreConfig()
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).reduce(_ + "," + _))
  // val add = LazyModule(new AdderTestHarness()(Parameters.empty))
  circt.stage.ChiselStage.emitSystemVerilogFile( new SimTop, args, firtoolOptions)
  // DifftestModule.finish("Demo", false)

}
