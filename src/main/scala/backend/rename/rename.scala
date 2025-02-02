package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

import grvcore.common._

// class CommitResp(implicit p: Parameters) extends GRVBundle{
//     val commit               = Bool()
//     val rat_write_dst        = Vec(coreWidth,Valid(new ReMapReq))
//     val freelist_write_dst   = Vec(coreWidth, Valid(UInt(pregSz.W)))
// }
/* 
decode 送来的指令一定为2条有效指令，所以如果freelist分配不了两个寄存器，说明list空了，得暂停了
 */
class RenameStage(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle{
        val dec_uops     = Flipped(Decoupled(Vec(coreWidth,new MicroOp)))
        val dis_uops     = (Decoupled(Vec(coreWidth,new MicroOp)))
        val redirect     = Input(Bool())
        val commit       = Input(new CommitMsg)
    })
    val dec_ready = Wire(Bool())
    val dec_fire  = Wire(Bool())
    // val dis_valid = Wire(Bool())
    val rat       = Module(new RAT)
    val freelist  = Module(new FreeList)
    val rat_write_dst        = WireInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new ReMapReq))))

    val ratResps  = WireInit(rat.io.resps)
    val freelistResp = WireInit(freelist.io.alloc_pregs)
    val uops      = Wire(Vec(coreWidth,new MicroOp))
    uops := io.dec_uops.bits

    val lrs1      = WireInit(VecInit(io.dec_uops.bits.map(_.lrs1)))
    val lrs1_val  = WireInit(VecInit(io.dec_uops.bits.map{i=>i.lrs1_rtype=/=RT_X&&i.lrs1=/=0.U}))
    val lrs2      = WireInit(VecInit(io.dec_uops.bits.map(_.lrs2)))
    val lrs2_val  = WireInit(VecInit(io.dec_uops.bits.map{i=>i.lrs2_rtype=/=RT_X&&i.lrs2=/=0.U}))
    val ldst      = WireInit(VecInit(io.dec_uops.bits.map(_.ldst)))
    val ldst_val  = WireInit(VecInit(io.dec_uops.bits.map(_.ldst_val)))
    //read
    
    
    //不加redirect信号是因为送入dis的uop遇到redirect会无效
    dec_ready    := io.dis_uops.ready&&((!freelist.io.empty)&&(!io.redirect))
    dontTouch(dec_ready)
    io.dec_uops.ready := dec_ready
    dec_fire     := io.dec_uops.fire
    dontTouch(dec_fire)
    for(i <- 0 until coreWidth){
        //req
        rat.io.reqs(i).lrs1          := Mux(lrs1_val(i),lrs1(i),0.U)
        rat.io.reqs(i).lrs2          := Mux(lrs2_val(i),lrs2(i),0.U)
        rat.io.reqs(i).ldst          := Mux(ldst_val(i),ldst(i),0.U)

        freelist.io.reqs(i)          := ldst_val(i)&&dec_fire
        
        //remap
//to handle WAW
        rat.io.remapReqs(i).valid    := (freelist.io.alloc_pregs(i).valid)
        rat.io.remapReqs(i).bits.ldst:= ldst(i)
        rat.io.remapReqs(i).bits.pdst:= freelist.io.alloc_pregs(i).bits
        //to dis
        uops(i).pdst  := freelistResp(i).bits
        uops(i).prs1  := ratResps(i).prs1
        uops(i).prs2  := ratResps(i).prs2
        uops(i).old_pdst:= ratResps(i).old_pdst
        
    }
    //commit
    
    for(i<- 0 until coreWidth){
        rat_write_dst(i).valid := io.commit.valid(i)
        rat_write_dst(i).bits.ldst:= io.commit.commit_uops(i).ldst
        rat_write_dst(i).bits.pdst:= io.commit.commit_uops(i).pdst

        freelist.io.commit(i).valid := io.commit.valid(i)
        freelist.io.commit(i).bits.pdst := io.commit.commit_uops(i).pdst
        freelist.io.commit(i).bits.old_pdst := io.commit.commit_uops(i).old_pdst
    }
    rat.io.commitReqs := rat_write_dst
    // dealloc_pregs := freelist_write_dst
    //redirect
    rat.io.redirect := io.redirect
    freelist.io.redirect := io.redirect
    // freelist.io.redirect_freelist := rat.io.redirect_freelist


    io.dis_uops.valid := (!io.redirect)&(!freelist.io.empty)&&io.dec_uops.valid
    io.dis_uops.bits  := uops



}