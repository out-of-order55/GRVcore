package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

import grvcore.common._

class CommitResp(implicit p: Parameters) extends GRVBundle{
    val commit               = Bool()
    val rat_write_dst        = Vec(coreWidth,Valid(new ReMapReq))
    val freelist_write_dst   = Vec(coreWidth, Valid(UInt(pregSz.W)))
}
/* 
decode 送来的指令一定为2条有效指令，所以如果freelist分配不了两个寄存器，说明list空了，得暂停了
 */
class RenameStage(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle{
        val dec_uops     = Flipped(Decoupled(Vec(coreWidth,new MicroOp)))
        val dis_uops     = (Decoupled(Vec(coreWidth,new MicroOp)))
        val redirect     = Input(Bool())
        val commit       = Input(new CommitResp)
    })
    val dec_ready = Wire(Bool())
    val dec_fire  = Wire(Bool())
    // val dis_valid = Wire(Bool())
    val rat       = Module(new RAT)
    val freelist  = Module(new FreeList)
    val commit    = WireInit(io.commit.commit)
    val ratResps  = WireInit(rat.io.resps)
    val freelistResp = WireInit(freelist.io.alloc_pregs)
    val uops      = Wire(Vec(coreWidth,new MicroOp))
    uops := io.dec_uops.bits

    val lrs1      = WireInit(VecInit(io.dec_uops.bits.map(_.lrs1)))
    val lrs1_val  = WireInit(VecInit(io.dec_uops.bits.map(_.lrs1_rtype=/=RT_X)))
    val lrs2      = WireInit(VecInit(io.dec_uops.bits.map(_.lrs2)))
    val lrs2_val  = WireInit(VecInit(io.dec_uops.bits.map(_.lrs2_rtype=/=RT_X)))
    val ldst      = WireInit(VecInit(io.dec_uops.bits.map(_.ldst)))
    val ldst_val  = WireInit(VecInit(io.dec_uops.bits.map(_.ldst_val)))
    //read
    dec_fire     := io.dec_uops.fire
    dontTouch(dec_fire)
    
    //不加redirect信号是因为送入dis的uop遇到redirect会无效
    dec_ready    := io.dis_uops.ready&&(!freelist.io.empty)
    io.dec_uops.ready := dec_ready
    for(i <- 0 until coreWidth){
        //req
        rat.io.reqs(i).lrs1          := lrs1(i)
        rat.io.reqs(i).lrs2          := lrs2(i)
        rat.io.reqs(i).ldst          := ldst(i)

        freelist.io.reqs(i)          := ldst_val(i)
        
        //remap
        rat.io.remapReqs(i).valid    := freelist.io.alloc_pregs(i).valid
        rat.io.remapReqs(i).bits.ldst:= ldst(i)
        rat.io.remapReqs(i).bits.pdst:= freelist.io.alloc_pregs(i).bits
        //to dis
        uops(i).pdst  := freelistResp(i).bits
        uops(i).prs1  := ratResps(i).prs1
        uops(i).prs2  := ratResps(i).prs2
        uops(i).old_pdst:= ratResps(i).old_pdst
        
    }
    //commit
    rat.io.commitReqs := io.commit.rat_write_dst
    freelist.io.dealloc_pregs := io.commit.freelist_write_dst
    //redirect
    rat.io.redirect := io.redirect
    freelist.io.redirect := io.redirect
    freelist.io.redirect_freelist := rat.io.redirect_freelist


    io.dis_uops.valid := (!io.redirect)&(!freelist.io.empty)
    io.dis_uops.bits  := uops



}