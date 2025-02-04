package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class DecoderTest (implicit p:Parameters) extends LazyModule with HasFrontendParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x1000)))
    val frontend = LazyModule(new FrontEnd)
    

    lsram.node:=frontend.masterNode
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        // val (slave, _) = lsram.node.in(0)
        val front = (frontend.module)
        val decoders = Seq.fill(coreWidth)(Module(new DecodeUnit())) 
        val check = Module(new Checker)
        val timer = RegInit(0.U(32.W))
        val fail  = WireInit(false.B)
        val dataCnt = RegInit(0.U(32.W))
        dontTouch(dataCnt)
        dontTouch(fail)
        when(front.io.cpu.fetchpacket.fire){
            dataCnt  := dataCnt + 2.U
            fail :=  (0 until coreWidth).map{i=>
                front.io.cpu.fetchpacket.bits.uops(i).bits.inst=/=dataCnt+i.U
            }.reduce(_||_) 
        }

        timer := timer +1.U
        check.io.finish := false.B
        check.io.ret := false.B
        when(timer===500.U){
            check.io.finish := true.B
            check.io.ret := false.B
        }
        // .elsewhen(fail){
        //     check.io.finish := true.B
        //     check.io.ret := true.B
        // }
        check.io.clock := clock
        check.io.reset := reset
        front.io.cpu := DontCare
        dontTouch(front.io.cpu)
        val front_r = RegNext(front.io.cpu.fetchpacket.fire)
        front.io.cpu.commit                     := front_r&&(!front.io.cpu.fetchpacket.fire)
        front.io.cpu.fetchpacket.ready          := true.B
        front.io.cpu.redirect_flush             := false.B
        front.io.cpu.redirect_val               := false.B
        front.io.cpu.get_pc                     := DontCare
        front.io.cpu.flush_icache               := false.B
        // front.io.cpu.brupdate.cfi_mispredicted  := false.B

        
        // val fetchpacket = 
        (decoders zip front.io.cpu.fetchpacket.bits.uops).foreach{case(a,b)=>
            a.io.enq.uop := b.bits
            dontTouch(a.io.deq.uop)
        }

    
    
    }
}