package Cache
import chisel3._
import chisel3.util._

class CacheWriteIO(addrWidth: Int, dataWidth: Int) extends Bundle{
    val wen   = Input(Bool())
    val wdata = Input(UInt(dataWidth.W))
    val waddr = Input(UInt(addrWidth.W))
}
class CacheReadIO(addrWidth: Int, dataWidth: Int,blk_size:Int) extends Bundle{
    val ren   = Input(Bool())
    val raddr = Input(UInt(addrWidth.W))
    // val rdata = Output(Vec(blk_size/(dataWidth/8),UInt(dataWidth.W)))
}
class MissMsg(addrWidth: Int, dataWidth: Int,blk_size:Int) extends Bundle{
    val miss   = Input(Bool())
    val addr   = Input(UInt(addrWidth.W))
}
class CacheMsg(tagWidth: Int,indexWidth:Int,offsetWidth:Int,bankWidth:Int)extends Bundle{
    val tag    = UInt(tagWidth.W)
    val index  = UInt(indexWidth.W)
    val offset = UInt(offsetWidth.W)
    val bank   = UInt(bankWidth.W)
    val ren    = (Bool())
}

class SRAMIO(addrWidth: Int, dataWidth: Int)extends Bundle{
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(10.W))
    val dataIn = Input(UInt(dataWidth.W))
    val dataOut = Output(UInt(dataWidth.W))
}

object AddressSplitter{
    def apply(tagWidth: Int,indexWidth:Int,offsetWidth:Int,bankWidth:Int,readport:CacheReadIO,addrWidth:Int)={
        val res = Wire(new CacheMsg(tagWidth,indexWidth,offsetWidth,bankWidth))
        // val addrWire = Wire(UInt(addrWidth.W))

        // addrWire := addr
        res.tag  := (readport.raddr(addrWidth-1,(indexWidth+offsetWidth)))
        res.index:= (readport.raddr((indexWidth+offsetWidth)-1,offsetWidth))
        res.offset := readport.raddr((offsetWidth)-1,0)
        res.bank := readport.raddr(offsetWidth-1,offsetWidth-bankWidth)
        res.ren  := readport.ren
        res
    }
}
