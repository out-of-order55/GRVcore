package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SRAMHelper extends BlackBox {
    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Bool())
        val raddr = Input(UInt(32.W))
        val ren   = Input(Bool())
        val rdata = Output(UInt(32.W))
    })

}

//只用于仿真
class AXI4SRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule{
    val beatBytes = 8
    val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
        Seq(AXI4SlaveParameters(
            address       = address,
            executable    = true,
            supportsWrite = TransferSizes.none,
            supportsRead  = TransferSizes(1, beatBytes),
            interleavedId = Some(0))
        ),
        beatBytes  = beatBytes)))

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        val (in, _) = node.in(0)

        val sram = Module(new SRAMHelper)
        sram.io.clock := clock
        sram.io.reset := reset
        val s_idle :: s_wait_ack :: Nil = Enum(2)

        val state       = RegInit(s_idle)
        val state_n     = WireInit(state)
        val dataCnt     = Reg(UInt(log2Ceil(in.ar.bits.params.lenBits).W))
        val burstEnable = WireInit(!(in.ar.bits.burst.orR))//只支持fixed突发
        // burstEnable:=()

        val beatBytes   = WireInit((in.ar.bits.params.dataBits).U)
        val transLen    = WireInit((in.ar.bits.len))
        val raddr       = Reg(UInt(32.W))
        dontTouch(burstEnable)
        dontTouch(beatBytes)
        state := state_n
        // assert((in.ar.fire)&(burstEnable)&(transLen=/=0.U),"if burst close,AXLEN must be 0")
        raddr := Mux(state===s_idle,in.ar.bits.addr,raddr + beatBytes)
        switch(state){
            is(s_idle){
                when(in.ar.fire){
                    state_n := s_wait_ack
                }
            }
            is(s_wait_ack){
                when(!burstEnable){
                    state_n := s_idle 
                }.elsewhen(dataCnt===transLen){
                    state_n := s_idle
                }
            }
        }
        when(state===s_idle&state_n===s_wait_ack){
            dataCnt := 0.U
        }.elsewhen(state===s_wait_ack&in.r.fire){
            dataCnt := dataCnt + 1.U
        }

        
        sram.io.raddr := raddr
        sram.io.ren := state===s_wait_ack

        in.ar.ready := (state === s_idle)
        
        assert(!(in.ar.fire && in.ar.bits.size === 3.U), "do not support 8 byte transfter")

        in.r.bits.data := sram.io.rdata
        in.r.bits.id := RegEnable(in.ar.bits.id, in.ar.fire)
        in.r.bits.resp := 0.U
        in.r.bits.last := state===s_wait_ack&(dataCnt===transLen)
        in.r.valid := (state === s_wait_ack)

        in.aw.ready := false.B
        in. w.ready := false.B
        in. b.valid := false.B

        assert(!in.aw.valid, "do not support write operations now")
        assert(!in. w.valid, "do not support write operations now")
    }
}

class ICacheWrapper(implicit p: Parameters) extends LazyModule with HasICacheParameters{
    val masterNode = AXI4MasterNode(Seq(
    AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
        name = "ICache")))))
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        val (master, _) = masterNode.out(0)
        val icache = Module(new ICache)
        val io     = IO(new ICacheBundle)
        io <> icache.io
        icache.imaster <>master
        dontTouch(icache.imaster.ar)
        dontTouch(icache.imaster.r)
        dontTouch(icache.io)
    
}
}
class CacheTest (implicit p:Parameters)extends LazyModule with HasICacheParameters{


    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x1000)))
    val icache = LazyModule(new ICacheWrapper())


    lsram.node:=icache.masterNode 


    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch {
        val s0_vaddr      = WireInit(0.U(XLEN.W))
        val s0_valid     = WireInit(false.B)
        val f1_clear     = WireInit(false.B)

        val start = RegInit(false.B)
        start := true.B
        val start1 = RegNext(start)
        dontTouch(start)
        dontTouch(start1)
        when ((start)&(!start1)){
            s0_valid     := true.B
            s0_vaddr     := 0.U
        }
        dontTouch(f1_clear)
        val s1_valid     = RegNext(s0_valid, false.B)
        val s2_valid     = RegNext(s1_valid && !f1_clear, false.B)
        when(s2_valid&(!icache.module.io.resp.valid)){
            f1_clear := true.B
            s0_vaddr := 0.U
            s0_valid := true.B
        }

        icache.module.io.req.valid      := s0_valid
        icache.module.io.req.bits.raddr := s0_vaddr 
        icache.module.io.s1_kill        := f1_clear
        icache.module.io.s2_kill        := false.B
        icache.module.io.invalidate     := false.B
        icache.module.io.resp.bits.data := DontCare
        dontTouch(icache.module.io)
    }
  // s.dontTouchPorts()
}