package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

///////////////////////////Parameters//////////////////////
import org.chipsalliance.cde.config._

class GRVModule(implicit val p:Parameters) extends Module with HasGRVParameters 
class GRVBundle(implicit val p:Parameters) extends Bundle with HasGRVParameters 
object CPUAXI4BundleParameters {
    def apply() = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
}
// class SRAMIO(width:Int,Depth:Int)(implicit p:Parameters)extends GRVBundle{
//     val enable = Input(Bool())
//     val write = Input(Bool())
//     val addr = Input(UInt(log2Ceil(Depth).W))
//     val dataIn = if(width==1) Input(Bool()) else Input(UInt(width.W))
//     val dataOut = if(width==1) Output(Bool()) else Output(UInt(width.W))
// }
// class SRAM(width:Int,Depth:Int)(implicit p:Parameters) extends GRVModule {
//     val io = IO(new SRAMIO(width,Depth))
//     val mem = SyncReadMem(Depth, UInt(width.W))
//     io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
// }
class SRAMHelper[T <: Data]( depth: Int,dataType:T)(implicit p: Parameters) extends GRVModule {

    val io = IO(new SRAMIO( depth, dataType))  
    val mem = SyncReadMem(depth, dataType) 
    io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
    class SRAMIO[T <: Data]( depth: Int, dataType: T) extends Bundle {
        val addr = Input(UInt(log2Ceil(depth).W))
        val dataIn = Input(dataType)
        val dataOut = Output(dataType)
        val enable = Input(Bool())
        val write = Input(Bool())
    }

}

class DualSRAMHelper[T <: Data]( depth: Int,dataType:T)(implicit p: Parameters) extends GRVModule {

    val mem = SyncReadMem(depth, dataType) 
    def write(en: Bool, waddr: UInt, dataIn: T): Unit = {
        when(en) {
            mem.write(waddr, dataIn)
        }
    }
    def read(en: Bool, raddr: UInt): T = {
        mem.read(raddr,en)
    }
    // val io = IO(new SRAMIO( depth, dataType))  
    
    // io.dataOut := mem.read(io.raddr,io.ren)
    // when(io.wen){
    //     mem.write(io.waddr,io.dataIn)
    // }
    // class SRAMIO[T <: Data]( depth: Int, dataType: T) extends Bundle {
    //     val wen   = Input(Bool())
    //     val waddr = Input(UInt(log2Ceil(depth).W))
    //     val dataIn = Input(dataType)

        
    //     val ren = Input(Bool())
    //     val raddr = Input(UInt(log2Ceil(depth).W))
    //     val dataOut = Output(dataType)
    // }

}