
// import circt.stage.ChiselStage
import chisel3._

import chisel3.util.{Cat,Decoupled,DecoupledIO}
// import chisel3.stage._
import org.chipsalliance.cde.config.Parameters
// import freechips.rocketchip.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.bundlebridge._
import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.util._
import java.io.PrintWriter


trait DontOmitGraphML{this:LazyModule=>
    override def omitGraphML:Boolean = false

}
class  DecoupledBct[T<:Data]()(implicit p :Parameters) extends LazyModule{
    val  node = BundleBridgeNexusNode[DecoupledIO[T]]()
    lazy val module = new LazyModuleImp(this){
        require(node.in.size==1)
        require(node.out.nonEmpty)
        val input = node.in.head._1
        val output = node.out.map(_._1)
        input.ready :=  (output.map(_.ready)).reduce(_||_)
        for(o <- output){
            o.valid := input.valid
            o.bits:=input.bits 
        }
    }
}
object DecoupledBct {
    def apply[T<:Data]()(implicit p:Parameters):BundleBridgeNexusNode[DecoupledIO[T]]={
        val  bct = LazyModule(new DecoupledBct[T]())
        bct.node
    }
} 
class eoutput extends Bundle
class einput extends Bundle

abstract class Exu(implicit p :Parameters)extends SimpleLazyModule with DontOmitGraphML {
    val input_node = BundleBridgeSink[DecoupledIO[einput]]()

    val output_node = BundleBridgeSource(() => DecoupledIO(new eoutput))
}

class ALU(implicit p:Parameters)extends Exu with DontOmitGraphML
class MLU(implicit p:Parameters)extends Exu with DontOmitGraphML

class arbiter(implicit p:Parameters)extends SimpleLazyModule with DontOmitGraphML{
    val input_node = BundleBridgeNexusNode[DecoupledIO[eoutput]]()
}
class iq(implicit p:Parameters)extends SimpleLazyModule with DontOmitGraphML{
    override lazy val desiredName = s"iq"
    val issue_node = BundleBridgeNexusNode(Some(()=>Decoupled(new einput)))

    val wk_node = BundleBridgeNexusNode[DecoupledIO[eoutput]]()
} 

class EXU(implicit p:Parameters)extends SimpleLazyModule with DontOmitGraphML{
    override lazy val desiredName = s"EXU"
    val iq = LazyModule(new iq())  
    val exus = Seq(LazyModule(new ALU()),LazyModule(new MLU()))
    for (exu <- exus){
        exu.input_node := iq.issue_node 
    } 
}
  
class Backend(implicit p:Parameters)extends SimpleLazyModule  {
    val FU = Seq.fill(2){LazyModule(new EXU())}
    val wb = LazyModule(new arbiter())
    for(bl <- FU){
        for(exu <- bl.exus){
            val bct =  DecoupledBct[eoutput]()
            bct := exu.output_node
            wb.input_node := bct
            for(iq <- FU.map(_.iq))(
                iq.wk_node := bct 
            )
        }
    }
}

object Backend extends App{

  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).reduce(_ + "," + _))
  val backend = LazyModule(new Backend()(Parameters.empty))
  circt.stage.ChiselStage.emitSystemVerilogFile(backend.module, args, firtoolOptions)
    val writer = new PrintWriter("output.graphml")
    writer.write(backend.graphML)
    writer.close()  // 刷新并关闭文件
}
