
package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

class RenameFreeList(
  val plWidth: Int,
  val numPregs: Int,
  val numLregs: Int)
  (implicit p: Parameters) extends Module
{
  private val pregSz = log2Ceil(numPregs)
  private val n = numPregs

  val io = IO(new Bundle {
    // 从freelist读出
    val reqs          = Input(Vec(plWidth, Bool()))
    val alloc_pregs   = Output(Vec(plWidth, Valid(UInt(pregSz.W))))

    // 写入freelist(ROB提交阶段)
    val dealloc_pregs = Input(Vec(plWidth, Valid(UInt(pregSz.W))))
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
  val free_list = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))

  // Select pregs from the free list.
  val sels = SelectFirstN(free_list, plWidth)
  val sel_fire  = Wire(Vec(plWidth, Bool()))


  // Masks that modify the freelist array.
  val sel_mask = (sels zip sel_fire) map { case (s,f) => s & Fill(n,f) } reduce(_|_)

  val dealloc_mask = io.dealloc_pregs.map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_) 




  // Update the free list.
  free_list := (free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W))

  // Pipeline logic | hookup outputs.
  for (w <- 0 until plWidth) {
    val can_sel = sels(w).orR
    val r_valid = RegInit(false.B)
    val r_sel   = RegEnable(OHToUInt(sels(w)), sel_fire(w))

    r_valid := r_valid && !io.reqs(w) || can_sel
    sel_fire(w) := (!r_valid || io.reqs(w)) && can_sel

    io.alloc_pregs(w).bits  := r_sel
    io.alloc_pregs(w).valid := r_valid
  }


}
