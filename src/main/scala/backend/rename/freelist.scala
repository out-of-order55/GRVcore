
package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters
/* 
area:2800
 */
class CommitReq(implicit p: Parameters) extends GRVBundle
{

    val pdst = UInt(pregSz.W)
	val old_pdst = UInt(pregSz.W)
}
class FreeList(implicit p: Parameters) extends GRVModule
{
	private val pregSz = log2Ceil(numPregs)
	private val n = numPregs

	val io = IO(new Bundle {
    // 从freelist读出
		val reqs          = Input(Vec(coreWidth, Bool()))
		val redirect      = Input(Bool())
		val alloc_pregs   = Output(Vec(coreWidth, Valid(UInt(pregSz.W))))

		// 写入freelist(ROB提交阶段)旧的寄存器
		// val dealloc_pregs  = Input(Vec(coreWidth, Valid(UInt(pregSz.W))))
		val commit   	   = Input(Vec(coreWidth, Valid(new CommitReq)))
		val empty		   = Output(Bool())
		val debug_freelist = Output(UInt(numPregs.W))
	})
	def SelectFirstN(in: UInt, n: Int) = {
		val sels = Wire(Vec(n, UInt(in.getWidth.W)))
		var mask = in

		for (i <- 0 until n) {
			sels(i) := PriorityEncoderOH(mask)
			mask = mask & ~sels(i)
		}

		sels
	}
// 初始化freelist,最低位0,表示0号寄存器
	val free_list = RegInit(UInt(numPregs.W),~(1.U(numPregs.W)))

    val comm_list = RegInit(UInt(numPregs.W),~(1.U(numPregs.W)))

    val commit_pdst 	= WireInit(VecInit(io.commit.map(_.bits.pdst)))
	val commit_old_pdst = WireInit(VecInit(io.commit.map(_.bits.old_pdst)))
    val commit_mask = WireInit((0 until coreWidth).map{i=>
        UIntToOH(commit_pdst(i),numPregs)&Fill(numPregs,io.commit(i).valid)
    }.reduce(_|_))
    val commit_old_mask = WireInit((0 until coreWidth).map{i=>
        UIntToOH(commit_old_pdst(i),numPregs)&Fill(numPregs,io.commit(i).valid)
    }.reduce(_|_))
	val commit_valid = WireInit(io.commit.map(_.valid).reduce(_||_))
	dontTouch(commit_mask)
	dontTouch(commit_old_mask)
	val commit_wb_mask = ((comm_list&(~commit_mask))|commit_old_mask)&(~(1.U(numPregs.W)))
    comm_list  := Mux(commit_valid,commit_wb_mask,comm_list)

	val empty			= WireInit(false.B)
	empty:=PopCount(free_list)<coreWidth.U

	dontTouch(empty)

	val sels 		   = SelectFirstN(free_list, coreWidth)
	val sel_valid  = Wire(Vec(coreWidth, Bool()))



	val sel_mask = (sels zip sel_valid) map { case (s,f) => s & Fill(n,f) } reduce(_|_)

	val dealloc_mask = io.commit.map(d => UIntToOH(d.bits.old_pdst)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_) 

	dontTouch(dealloc_mask)
	dontTouch(sel_mask)
	free_list := Mux(io.redirect,commit_wb_mask,(free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W)))


	for (w <- 0 until coreWidth) {
		val can_sel = sels(w).orR
		sel_valid(w) := io.reqs(w) && can_sel
		io.alloc_pregs(w).bits  := Mux(sel_valid(w),OHToUInt(sels(w)),0.U)
		io.alloc_pregs(w).valid := sel_valid(w)
	}

	io.debug_freelist:=free_list
	io.empty := PopCount(free_list)<coreWidth.U
}
