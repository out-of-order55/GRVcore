package grvcore
import chisel3._
import chisel3.util._

object MaskUpper
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => (in << i.U)(n-1,0)).reduce(_|_)
  }
}
// object Num1{
//   def apply(in: UInt) = {
//     val n = in.getWidth
//     val num = UInt(n.W)
//     num=(0 until n).map{i=>
//       in(i).asUInt
//     }.reduce(_+_)
//     num
//   }
// }
object Transpose
{
  def apply[T <: chisel3.Data](in: Vec[Vec[T]]) = {
    val n = in(0).size
    VecInit((0 until n).map(i => VecInit(in.map(row => row(i)))))
  }
}
object MaskLower
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_|_)
  }
}