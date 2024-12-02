package  grvcore
import chisel3._
import chisel3.util._

class CacheReq(addrWidth: Int, dataWidth: Int) extends Bundle {
    val addr = UInt(addrWidth.W)
    val data = UInt(dataWidth.W)
    val mask = UInt((dataWidth / 8).W)
}

class CacheResp(dataWidth: Int) extends Bundle {
    val data = UInt(dataWidth.W)
}

// req.valid->ren or wen  resp.valid ->data_ok
class CacheIO(addrWidth: Int, dataWidth: Int) extends Bundle {
    val req = Flipped(Decoupled(new CacheReq(addrWidth, dataWidth)))
    val resp = Decoupled(new CacheResp(dataWidth))
}