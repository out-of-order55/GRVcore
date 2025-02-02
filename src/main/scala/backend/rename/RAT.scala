package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
/* 
RAT需要完成的是：
1.读出源寄存器对应的寄存器地址,以及目的寄存器对应的旧物理寄存器（为了在commit释放）
2.写入目的寄存器对应的寄存器地址
3.设置commit table，以便异常恢复
area:13000->9100
需要优化
 */
class MapReq(implicit p: Parameters) extends GRVBundle
{
    val lrs1 = UInt(lregSz.W)
    val lrs2 = UInt(lregSz.W)
    val ldst = UInt(lregSz.W)
}

class MapResp(implicit p: Parameters) extends GRVBundle
{
    val prs1 = UInt(pregSz.W)
    val prs2 = UInt(pregSz.W)
    val old_pdst = UInt(pregSz.W)
}
class ReMapReq(implicit p: Parameters) extends GRVBundle
{
    val ldst = UInt(lregSz.W)
    val pdst = UInt(pregSz.W)
}
/* 
写逻辑需要优化
 */
class RAT(implicit p: Parameters) extends GRVModule{
    val io = IO(new Bundle{
        val reqs         = Input(Vec(coreWidth,new MapReq))
        val resps        = Output(Vec(coreWidth,new MapResp))
        val remapReqs    = Input(Vec(coreWidth,Valid(new ReMapReq)))
        val commitReqs   = Input(Vec(coreWidth,Valid(new ReMapReq)))
        val redirect     = Input(Bool())
        // val redirect_freelist = Output(UInt(numPregs.W))
    })
    val spec_rat    = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
    val commit_rat  = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
//for waw
    val rat_wen = (0 until coreWidth).map{i=>
        val rat_valid = VecInit(
        (i until coreWidth).map{k=>
            io.reqs(i).ldst===io.reqs(k).ldst
        })
        PopCount(rat_valid)===1.U
    }

    val commit_rat_wen = (0 until coreWidth).map{i=>
        val rat_valid = VecInit(
        (i until coreWidth).map{k=>
            io.commitReqs(i).bits.ldst===io.commitReqs(k).bits.ldst
        })
        PopCount(rat_valid)===1.U
    }
    //write logic
    // val spec_wmask = WireInit(VecInit((0 until coreWidth).map{i=>
    //     UIntToOH(io.remapReqs(i).bits.ldst,numLregs)&Fill(numLregs,io.remapReqs(i).valid&rat_wen(i))}))
    // val spec_wdata = WireInit(VecInit(io.remapReqs.map(_.bits.pdst)))
    // val commit_wmask = WireInit(VecInit((0 until coreWidth).map{i=>
    //     UIntToOH(io.commitReqs(i).bits.ldst,numLregs)&Fill(numLregs,io.commitReqs(i).valid)}))
    // val commit_wdata = WireInit(VecInit(io.commitReqs.map(_.bits.pdst)))
    // dontTouch(spec_wmask)
    // dontTouch(spec_wdata)
    // for(i <- 0 until numLregs){
    //     // val spec_data = Mux1H()
    //     for(j<- 0 until coreWidth){
    //         spec_rat(i) := Mux(spec_wmask(j)(i)===1.U,spec_wdata(j),spec_rat(i))
    //         commit_rat(i) := Mux(commit_wmask(j)(i)===1.U,commit_wdata(j),commit_rat(i)) 
    //     }
    // }

    (spec_rat.zipWithIndex zip commit_rat).foreach{case((rat,idx),com_rat)=>
        val spec_wenOH = (0 until coreWidth).map{i=>
            io.remapReqs(i).valid&&idx.U===io.remapReqs(i).bits.ldst&rat_wen(i)
        }
        val remapData = Mux1H(spec_wenOH,io.remapReqs.map(_.bits.pdst))

        val commit_wenOH = (0 until coreWidth).map{i=>
            io.commitReqs(i).valid&&idx.U===io.commitReqs(i).bits.ldst&commit_rat_wen(i)
        }
        val commitData = Mux1H(commit_wenOH,io.commitReqs.map(_.bits.pdst))
        assert(PopCount(commit_wenOH)<=1.U,"multi commit port write one rat entry")
        assert(PopCount(spec_wenOH)<=1.U,"multi port write one rat entry")
        rat := Mux(io.redirect,
                Mux(commit_wenOH.reduce(_||_),commitData,com_rat),
                Mux(spec_wenOH.reduce(_||_),remapData,rat))
        com_rat := Mux(commit_wenOH.reduce(_||_),commitData,com_rat)
    }
    // val 
    for(i <- 0 until coreWidth){
        
        io.resps(i).prs1 := (0 until i).foldLeft(spec_rat(io.reqs(i).lrs1)) ((p,k) =>
            Mux(io.remapReqs(k).valid && io.remapReqs(k).bits.ldst === io.reqs(i).lrs1, io.remapReqs(k).bits.pdst, p))

        io.resps(i).prs2 := (0 until i).foldLeft(spec_rat(io.reqs(i).lrs2)) ((p,k) =>
            Mux(io.remapReqs(k).valid && io.remapReqs(k).bits.ldst === io.reqs(i).lrs2, io.remapReqs(k).bits.pdst, p))

        io.resps(i).old_pdst := (0 until i).foldLeft(spec_rat(io.reqs(i).ldst)) ((p,k) =>
            Mux(io.remapReqs(k).valid && io.remapReqs(k).bits.ldst === io.reqs(i).ldst, io.remapReqs(k).bits.pdst, p))

        }
    // io.redirect_freelist := comm_list&(~commit_listmask)

}
