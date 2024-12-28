package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable.ArrayBuffer

class RegFileReadIO(implicit p: Parameters) extends GRVBundle{
    val addr = Input(UInt(pregSz.W))
    val data = Output(UInt(XLEN.W))
}
class RegFileWriteIO(implicit p: Parameters) extends GRVBundle{
    val addr = Input(UInt(pregSz.W))
    val wen  = Input(Bool())
    val data = Input(UInt(XLEN.W))
}
/* 
由于执行完直接写入寄存器，所以bypass网络就是从写入端到读出端
 */
class RegFile
(
val numReadPorts :Int,
val numWritePorts :Int,
val Bypass:Seq[Boolean]
)(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle{
        val readports  = Vec(numReadPorts,new RegFileReadIO)
        val writeports = Vec(numWritePorts,new RegFileWriteIO) 
    })
    val regfile = Reg(Vec(maxPregSz,UInt(XLEN.W)))
    regfile(0):= 0.U

    val rdata = Wire(Vec(numReadPorts,UInt(XLEN.W)))
    val raddr = WireInit(VecInit(io.readports.map(_.addr)))
    for(i <- 0 until numReadPorts){
        rdata(i) := regfile(raddr(i))
    }
    val waddr = WireInit(VecInit(io.writeports.map(_.addr)))
    val wdata = WireInit(VecInit(io.writeports.map(_.data)))
    val wen   = WireInit(VecInit(io.writeports.map(_.wen)))
    
    for(i <- 0 until numWritePorts){
        regfile(waddr(i)) := Mux(wen(i),wdata(i),regfile(waddr(i)))
    }
    if (Bypass.reduce(_||_)) {
        val bypassable_wports = ArrayBuffer[RegFileWriteIO]()
        io.writeports zip Bypass map { case (wport, b) => if (b) { bypassable_wports += wport} }

        for (i <- 0 until numReadPorts) {
            val bypass_ens = bypassable_wports.map(x => x.wen &&
            x.addr === raddr(i))
            val bypass_data = Mux1H(VecInit(bypass_ens.toSeq), VecInit(bypassable_wports.map(_.data).toSeq))
            io.readports(i).data := Mux(bypass_ens.reduce(_|_), bypass_data, rdata(i))
        }
        } else {
        for (i <- 0 until numReadPorts) {
            io.readports(i).data := rdata(i)
        }
    }


    for (i <- 0 until (numWritePorts - 1)) {
        for (j <- (i + 1) until numWritePorts) {
            assert(!io.writeports(i).wen ||
                !io.writeports(j).wen ||
                (io.writeports(i).addr =/= io.writeports(j).addr) ||
                (io.writeports(i).addr === 0.U), 
            "[regfile] too many writers a register")
        }
    }

}