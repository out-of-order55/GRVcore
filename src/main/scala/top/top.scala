import grvcore._
import circt.stage._
import org.chipsalliance.cde.config._

import chisel3._
import grvcore.common.SimTable
// import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

object Elaborate extends App {
    SimTable.printLookupTables()
    println("-----------------Generate Verilog--------------------")
    implicit val p:Parameters = new Test1Config() 
    // val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x20000000, 0x1000)))

    val firtoolOptions = Array("--disable-annotation-unknown" ,"--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
    
    ).reduce(_ + "," + _))

    
    circt.stage.ChiselStage.emitSystemVerilogFile(new SimTop, args, firtoolOptions)
  

}