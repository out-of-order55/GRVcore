package grvcore
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
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
object AlignPCToBoundary
{
  def apply(pc: UInt, b: Int): UInt = {
    // Invert for scenario where pc longer than b
    //   (which would clear all bits above size(b)).
    ~(~pc | (b-1).U)
  }
}

object Sext
{
  def apply(x: UInt, length: Int): UInt = {
    if (x.getWidth == length) return x
    else return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
  }
}
object SelectFirstN
{
  def apply(in: UInt, n: Int) = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & ~sels(i)
    }

    sels
  }
}
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
object GRVString
{
  /**
  * Add prefix to BOOM strings (currently only adds the hartId)
  *
  * @param strs list of strings
  * @return String combining the list with the prefix per line
  */
  def apply(strs: String*)(implicit p: Parameters) = {
    strs.map(str =>  str + "\n").mkString("")
  }
}
object ImmGen extends RISCVConstants with GRVOpConstants
{

  def apply(ip: UInt, isel: UInt): SInt = {
    val sign = ip(LONGEST_IMM_SZ-1).asSInt
    val i30_20 = Mux(isel === IS_U, ip(18,8).asSInt, sign)
    val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).asSInt, sign)
    val i11    = Mux(isel === IS_U, 0.S,
                  Mux(isel === IS_J || isel === IS_B, ip(8).asSInt, sign))
    val i10_5  = Mux(isel === IS_U, 0.S, ip(18,14).asSInt)
    val i4_1   = Mux(isel === IS_U, 0.S, ip(13,9).asSInt)
    val i0     = Mux(isel === IS_S || isel === IS_I, ip(8).asSInt, 0.S)

    return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).asSInt
  }
}

class Compactor[T<:Data](n:Int,k:Int,gen:T) extends  Module{
  val io = IO(new Bundle{
    val in  = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(k,         DecoupledIO(gen))
  })
  if(n==k){
    io.in<>io.out
  }else{
    val sels = VecInit.tabulate(n)((io.in.map(_.valid).scan(1.U(k.W)) ((a,b)=>Mux(b.asBool,(a<<1)(k-1,0),a))))
    val selsOH = Transpose(VecInit(sels.map{i=>VecInit(i.asBools)})).map{col=>
      (col zip io.in.map(_.valid)).map{case(a,b)=>a&&b}
    }
    val in_readys  = sels.map{row=>VecInit((row.asBools zip io.out.map(_.ready)).map{case(c,r)=>(c&&r)})}
    val out_valids = selsOH.map(col=>col.reduce(_||_))
    val out_datas  = selsOH.map{s=>Mux1H(s,io.in.map(_.bits))}
    in_readys zip io.in foreach{case(c,r)=>r.ready := c}
    out_valids zip out_datas zip io.out foreach{case((v,d),o)=> o.valid := v;o.bits:=d} 
  }

}