package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
class TileTest (implicit p:Parameters)extends LazyModule with HasDCacheParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x00000000L, 0x050000)))
    val ltile = LazyModule(new Tile())
    lsram.node:=ltile.masterNode 
    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch 
    with freechips.rocketchip.rocket.constants.MemoryOpConstants{
        
    }
}

