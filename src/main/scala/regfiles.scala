package grvcore

import chisel3._
import chisel3.util._

class regfiles extends Module {
    val io = IO(new Bundle {
        val rreq   = Input(new RegfileReq())
        val rresp  = Output(new RegfileResp())
        val wreq   = Input(new RegfileMessage())
    })
    val data = Reg(Vec(32,UInt(32.W)))
    when(io.wreq.wen){
        data(io.wreq.waddr) := io.wreq.data
    }
    io.rresp.data1 := Mux(io.rreq.raddr1===0.U,0.U,data(io.rreq.raddr1))
    io.rresp.data2 := Mux(io.rreq.raddr2===0.U,0.U,data(io.rreq.raddr2))
}
