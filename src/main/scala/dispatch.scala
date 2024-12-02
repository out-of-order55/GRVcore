package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
object Transpose
{
  def apply[T <: chisel3.Data](in: Vec[Vec[T]]) = {
    val n = in(0).size
    VecInit((0 until n).map(i => VecInit(in.map(row => row(i)))))
  }
}

/**
 * Connect the first k of n valid input interfaces to k output interfaces.
 */

class Compactor[T <: chisel3.Data](n: Int, k: Int, gen: T) extends Module
{
  require(n >= k)

  val io = IO(new Bundle {
    val in  = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(k,         DecoupledIO(gen))
    val s1 = Input(Vec(3,Bool()))
    val s2 = Input(Vec(2,Bool()))
    val s3 = Output(Vec(2,Bool()))
  })

  if (n == k) {
    io.out <> io.in
  } else {

    io.s3 := (io.s1 zip io.s2) map {case(a,b)=>a&b}

    val counts = io.in.map(_.valid).scanLeft(1.U(k.W)) ((c,e) => Mux(e, (c<<1)(k-1,0), c))
    val sels = Transpose(VecInit(counts map (c => VecInit(c.asBools)))) map (col =>
                 (col zip io.in.map(_.valid)) map {case (c,v) => c && v})
    val in_readys = counts map (row => (row.asBools zip io.out.map(_.ready)) map {case (c,r) => c && r} reduce (_||_))
    val out_valids = sels map (col => col.reduce(_||_))
    val out_data = sels map (s => Mux1H(s, io.in.map(_.bits)))

    in_readys zip io.in foreach {case (r,i) => i.ready := r}
    out_valids zip out_data zip io.out foreach {case ((v,d),o) => o.valid := v; o.bits := d}
  }
}