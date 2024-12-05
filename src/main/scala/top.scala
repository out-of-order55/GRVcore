import grvcore._
import circt.stage._
import org.chipsalliance.cde.config._


// class SimTop (implicit p:Parameters)extends GRVBundle{
//     val io = IO(new Bundle {})
//     val m = Module(new top)
//     m.dontTouchPorts()
//     val zeroVec = RegInit(VecInit(Seq.fill(4)(0.U(32.W))))
//     val n = zeroVec.map(b=> b+1.U)
//     zeroVec  := n
//     m.io.in  := 0.U
//     m.io.in1 := n
//     m.io.out := DontCare
//     m.io.out1:= DontCare
// }
object Elaborate extends App {

    println("-----------------Generate Verilog--------------------")
    implicit val p:Parameters = new BaseConfig() 
    val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _))
    // val add = LazyModule(new AdderTestHarness()(Parameters.empty))
    circt.stage.ChiselStage.emitSystemVerilogFile( new ICache(), args, firtoolOptions)
  // DifftestModule.finish("Demo", false)

}