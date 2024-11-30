package ysyx

import chisel3._
import chisel3.util._
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
class test extends  BlackBox {
    val io = IO(new Bundle {
        val cnt    = Input(UInt(32.W))
    })
}
class top extends Module  {
    val io = IO(new Bundle{
        val in = Input(UInt(32.W))
        val in1 = Input(Vec(4,UInt(32.W)))
        val enable = Input(Bool())
        val enable1 = Input(Bool())
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
    val test = Module(new test())
    dontTouch(test.io.cnt)
    test.io.cnt := io.in
    val temp = widthMap(i=>io.in1(i))
    io.out1 := temp
    // mask:=new_mask
    io.out := 0.U

}