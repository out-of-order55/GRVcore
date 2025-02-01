
package grvcore

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
/* 
area:2800
 */
class FreeList(implicit p: Parameters) extends GRVModule
{
	private val pregSz = log2Ceil(numPregs)
	private val n = numPregs

	val io = IO(new Bundle {
    // 从freelist读出
		val reqs          = Input(Vec(coreWidth, Bool()))
		val redirect      = Input(Bool())
		val redirect_freelist = Input(UInt(numPregs.W))
		val alloc_pregs   = Output(Vec(coreWidth, Valid(UInt(pregSz.W))))

		// 写入freelist(ROB提交阶段)旧的寄存器
		val dealloc_pregs  = Input(Vec(coreWidth, Valid(UInt(pregSz.W))))
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

	val empty			= WireInit(false.B)
	empty:=PopCount(free_list)<=coreWidth.U

	dontTouch(empty)

	val sels 		   = SelectFirstN(free_list, coreWidth)
	val sel_valid  = Wire(Vec(coreWidth, Bool()))



	val sel_mask = (sels zip sel_valid) map { case (s,f) => s & Fill(n,f) } reduce(_|_)

	val dealloc_mask = io.dealloc_pregs.map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_) 



	free_list := Mux(io.redirect,io.redirect_freelist,(free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W)))


	for (w <- 0 until coreWidth) {
		val can_sel = sels(w).orR
		sel_valid(w) := io.reqs(w) && can_sel
		io.alloc_pregs(w).bits  := Mux(sel_valid(w),OHToUInt(sels(w)),0.U)
		io.alloc_pregs(w).valid := sel_valid(w)
	}

	io.debug_freelist:=free_list
	io.empty := PopCount(free_list)<coreWidth.U
}
