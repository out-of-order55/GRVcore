package ysyx

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config._
// import difftest._
// import org.chipsalliance.cde.config.Parameters
// import freechips.rocketchip.system.DefaultConfig
// import freechips.rocketchip.diplomacy.LazyModule
class ghist extends Bundle{
    val old_ghist = UInt(32.W)
    val in_bank0 = Bool()
    def update(addr:UInt,en:Bool):ghist={
        val new_hist = Wire(new ghist)
        val in_bank0 = addr(3)===1.U
        
        new_hist.old_ghist := Mux(en,old_ghist<<1.U|1.U,old_ghist<<1.U)
        new_hist.in_bank0 := in_bank0
        new_hist
    }
}
// class Test extends Module  {
//     val io = IO(new Bundle{
//         val in = Input(UInt(32.W))
//         val enable = Input(Bool())
//         val out = Output(new ghist)
//     })
//     val ghist = RegInit((0.U).asTypeOf(new ghist))
//     val new_hist = ghist.update(io.in,io.enable)
//     when(io.enable){
//         ghist := new_hist
//     }
//     io.out <>new_hist
//     // val data=Reg(UInt(2.W))


//     // data := 0.U
//     // when(io.enable1){
//     //     data := 1.U    
//     // }
//     // io.out := data
// }

case object Key extends Field[BaseParams]
trait BaseParams{
    val p1:Params1
    val p2:Params2
}
trait NoneParams {
    implicit val p: Parameters
    def baseparm = p(Key)
}
trait Params1 {
    val a: Boolean
    val b: Boolean
}
trait HasParams1 extends NoneParams{
    val p1 :Params1 = baseparm.p1
    val a = p1.a
    val b = p1.b
}
trait HasParams2 extends NoneParams{
    val p2 :Params2 = baseparm.p2
    val c = p2.c
    val d = p2.d
}
trait Params2 {
    val c: Boolean
    val d: Boolean
}
class DefaultConfig extends Config((site, here, up) => {
  case Key => new BaseParams {
    // 在这里定义 p1 和 p2 的具体实现
    val p1 = new Params1 {
      val a = true  // 或者根据实际需要设定
      val b = false
    }

    val p2 = new Params2 {
      val c = true
      val d = false
    }
  }
})

class test extends  BlackBox {
    val io = IO(new Bundle {
        val cnt    = Input(UInt(32.W))
    })
}

class top extends Module with  DontTouch {
    val io = IO(new Bundle{
        val in = Input(UInt(32.W))
        val in1 = Input(Vec(4,UInt(32.W)))
        // val enable = Input(Bool())
        // val enable1 = Input(Bool())
        val out = Output(UInt(32.W))
        val out1 = Output(Vec(4,UInt(32.W)))
    })
    def widthMap[T <: Data](f: Int => T) = VecInit((0 until 4).map(f))
    // var enq_ptr = 0.U
    // enq_ptr = Mux(io.enbale,enq_ptr+1.U,enq_ptr)

    // io.out := enq_ptr
    // val mask = RegInit((0.U)(32.W))
    // var new_mask = Mux(io.enable,mask<<1.U|1.U,mask)

    // new_mask = Mux(io.enable1,new_mask<<1.U|1.U,new_mask)
    // val test = Module(new test())
    // dontTouch(test.io.cnt)
    // test.io.cnt := io.in1(0)
    // val difftest = DifftestModule.finish("Demo")
    val temp = widthMap(i=>io.in1(i))
    io.out1 := temp
    // mask:=new_mask
    io.out := 0.U

}
