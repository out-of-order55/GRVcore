package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import grvcore.common._
class softRename extends BlackBox {
    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Bool())
        val reqs  = Input(UInt(2.W))
        val lrs1  = Input(UInt(10.W))
        val lrs2  = Input(UInt(10.W))
        val ldst  = Input(UInt(10.W))
        val old_pdst  = Output(UInt(12.W))
        val pdst  = Output(UInt(12.W))
        val prs1  = Output(UInt(12.W))
        val prs2  = Output(UInt(12.W))
    })

}
class FreeListTest (implicit  p:Parameters) extends GRVModule{
    val freelist    = Module(new FreeList)
    val check       = Module(new Checker)
    val reqs        = RegInit(0.U(coreWidth.W))
    val timer       = RegInit(0.U(32.W))

    freelist.io.reqs := reqs.asBools
    dontTouch(freelist.io.alloc_pregs)
    
    // freelist.io.dealloc_pregs:= RegNext(RegNext(freelist.io.alloc_pregs))
    // val deqNum   = PopCount(freelist.io.dealloc_pregs.map(_.valid))
    val enqNum   = PopCount(reqs)
    val startNum = RegInit(63.U(10.W))
    startNum := startNum-enqNum

    reqs := reqs + 1.U
    dontTouch(startNum)
    // dontTouch(enqNum)
    // dontTouch(deqNum)
    timer := timer +1.U
    check.io.clock := clock
    check.io.reset := reset
    check.io.finish := false.B
    check.io.ret := false.B
    when(timer===500.U){
        check.io.finish := true.B
        check.io.ret := false.B
    }
    .elsewhen(startNum=/=PopCount(freelist.io.debug_freelist)){
        check.io.finish := true.B
        check.io.ret := true.B
    }
}
/* 目前测试均是在没有异常的情况下的测试
对于异常，仅测试了redirect部件是否清零
 */
class RenameTest (implicit p:Parameters) extends LazyModule with HasFrontendParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x1000)))
    val frontend = LazyModule(new FrontEnd)
    

    lsram.node:=frontend.masterNode
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        // val (slave, _) = lsram.node.in(0)
        val front = (frontend.module)
        val decoders = Seq.fill(coreWidth)(Module(new DecodeUnit())) 
        val rename = Module(new RenameStage)
        val softrename = Module(new softRename)
        val check = Module(new Checker)
        val timer = RegInit(0.U(32.W))
        val fail  = WireInit(false.B)
        val dataCnt = RegInit(0.U(32.W))
        val dec_uops = Wire(Vec(coreWidth,new MicroOp))
        dontTouch(dataCnt)
        dontTouch(fail)
        // when(front.io.cpu.fetchpacket.fire){
        //     dataCnt  := dataCnt + 2.U
        //     fail :=  (0 until coreWidth).map{i=>
        //         front.io.cpu.fetchpacket.bits.uops(i).bits.inst=/=dataCnt+i.U
        //     }.reduce(_||_) 
        // }
        for(i <- 0 until coreWidth){
            dec_uops(i) := decoders(i).io.deq.uop
        }
        timer := timer +1.U
        check.io.finish := false.B
        check.io.ret := false.B
        when(timer===500.U){
            check.io.finish := true.B
            check.io.ret := false.B
        }
        .elsewhen(RegNext(fail)){
            check.io.finish := true.B
            check.io.ret := true.B
        }
        check.io.clock := clock
        check.io.reset := reset
        softrename.io.clock := clock
        softrename.io.reset := reset
        front.io.cpu := DontCare
        dontTouch(front.io.cpu)

        val dec_valid = WireInit(front.io.cpu.fetchpacket.valid&&(!front.io.cpu.redirect_val))
        dontTouch(dec_valid)
        val dec2rename = withReset(reset.asBool || front.io.cpu.redirect_val ) {
            Module(new Queue(Vec(coreWidth,new MicroOp), 1, pipe=true, flow=false)) }

        val front_r = RegNext(front.io.cpu.fetchpacket.fire)
        front.io.cpu.commit                     := front_r&&(!front.io.cpu.fetchpacket.fire)
        front.io.cpu.fetchpacket.ready          := dec2rename.io.enq.ready
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
    

        dec2rename.suggestName("dec2rename_pipe")
        dec2rename.io.enq.bits  := dec_uops
        dec2rename.io.enq.valid := dec_valid
        rename.io.dec_uops  <> dec2rename.io.deq 
        rename.io.commit   := DontCare
        rename.io.redirect  := false.B
        rename.io.dis_uops := DontCare
        rename.io.dis_uops.ready := true.B
        dontTouch(rename.io.dis_uops)

        val lrs1      = WireInit(VecInit(dec2rename.io.deq.bits.map(_.lrs1)))
        val lrs1_val  = WireInit(VecInit(dec2rename.io.deq.bits.map(_.lrs1_rtype=/=RT_X)))
        val lrs2      = WireInit(VecInit(dec2rename.io.deq.bits.map(_.lrs2)))
        val lrs2_val  = WireInit(VecInit(dec2rename.io.deq.bits.map(_.lrs2_rtype=/=RT_X)))
        val ldst      = WireInit(VecInit(dec2rename.io.deq.bits.map(_.ldst)))
        val ldst_val  = WireInit(VecInit(dec2rename.io.deq.bits.map(_.ldst_val)))
        dontTouch(lrs1_val)
        dontTouch(lrs2_val)
        dontTouch((ldst_val))
        softrename.io.lrs1  := Cat(lrs1(1),lrs1(0))
        softrename.io.lrs2  := Cat(lrs2(1),lrs2(0))
        softrename.io.ldst  := Cat(ldst(1),ldst(0))
        softrename.io.reqs  := Cat(ldst_val(1)&dec2rename.io.deq.fire,ldst_val(0)&dec2rename.io.deq.fire)
        dontTouch(softrename.io)

        val res_prs1        = Wire(Vec(coreWidth,UInt((pregSz).W)))
        val res_prs2        = Wire(Vec(coreWidth,UInt((pregSz).W)))
        val res_pdst        = Wire(Vec(coreWidth,UInt((pregSz).W)))
        val res_old_pdst    = Wire(Vec(coreWidth,UInt((pregSz).W)))
        dontTouch(res_prs1)
        dontTouch(res_prs2)
        dontTouch(res_pdst)
        dontTouch(res_old_pdst)

        for(i <-0 until coreWidth){
            res_pdst(i)     := (softrename.io.pdst)(pregSz*(i+1)-1,pregSz*i)        
            res_prs1(i)     := (softrename.io.prs1)(pregSz*(i+1)-1,pregSz*i)      
            res_prs2(i)     := (softrename.io.prs2)(pregSz*(i+1)-1,pregSz*i)     
            res_old_pdst(i) := (softrename.io.old_pdst)(pregSz*(i+1)-1,pregSz*i)  
        }
        fail := dec2rename.io.deq.fire&&((0 until coreWidth).map{i=>
            (res_pdst(i)=/=rename.io.dis_uops.bits(i).pdst&&ldst_val(i))||
            (res_prs1(i)=/=rename.io.dis_uops.bits(i).prs1&&lrs1_val(i))||
            (res_prs2(i)=/=rename.io.dis_uops.bits(i).prs2&&lrs2_val(i))||
            (res_old_pdst(i)=/=rename.io.dis_uops.bits(i).old_pdst&&ldst_val(i))
        }).reduce(_|_)
    }

}