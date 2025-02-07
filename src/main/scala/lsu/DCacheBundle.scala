package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// import freechips.rocketchip.util.Annotated.resetVector
import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config._
class DCacheMissMsg(implicit  p:Parameters) extends GRVBundle with HasICacheParameters{
	val memtype = Bool()
    val miss   = (Bool())
    val addr   = (UInt(XLEN.W))
	val data = Vec(bankNum,UInt(XLEN.W))
	val mask = Vec(bankNum,UInt((XLEN/8).W))
    
}
class ReadReq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    
    val addr = UInt(XLEN.W)
}
class ReadResp(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val data = UInt(XLEN.W)
    val hit  = Bool()
    val replay = Bool()
}
class WriteReq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val data = Vec(bankNum,UInt(XLEN.W))
    val addr = UInt(XLEN.W)
    val mask = Vec(bankNum,UInt((XLEN/8).W))
}
class WriteResp(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val hit = Bool()
    val addr= UInt(XLEN.W)
    val replay = Bool()
}

class DCacheReadIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val req = Flipped(Decoupled(new ReadReq))
    val resp= Valid(new ReadResp)
}
class DCacheWriteIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val req = Flipped(Decoupled(new WriteReq))
    val resp= Valid(new WriteResp)
}
class DCacheBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val read = Vec(numReadport,new DCacheReadIO)
    val write = new DCacheWriteIO
    val refillMsg= Output(new RefillMsg())
    val flush   = Input(Bool())
    val s1_kill = Input(Bool())
    val s2_kill = Input(Bool())
}
//mshr的队列数据mask不一定是总是1
class MSHREnq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val valid   = Bool()
    val data    = Vec(bankNum,UInt(XLEN.W))
    val addr    = UInt(XLEN.W)
	val mask    = Vec(bankNum,UInt((XLEN/8).W))

    val mem_type= Bool()//true:load 
}
//写回队列的数据总是mask全为1
class WBQEntry(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
	val valid   = Bool()
    val way     = UInt(nWays.W)
    val data    = Vec(bankNum,UInt(XLEN.W))
    val addr    = UInt(XLEN.W)
}

class RefillMsg(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val refill_addr   = UInt(XLEN.W)
    val refilled      = Bool()
    val refillData    = Vec(bankNum,UInt(XLEN.W))
    val refillWay     = UInt(nWays.W)
} 

class replaceReq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{

	val idx         = Output(UInt(indexWidth.W))
} 
class replaceResp(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
	val addr 		= UInt(XLEN.W)
    val way         = UInt(nWays.W)
	val dirty   	= Bool()
	val data        = Vec(bankNum,UInt(XLEN.W))
}

