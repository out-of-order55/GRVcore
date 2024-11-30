package grvcore
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.UIntTransform



class IFU(xlen:Int)extends Module{
    val io = IO(new Bundle {
    val branch   = Input(new BjpMessage())


    val icache      = Flipped(new CacheIO(xlen,xlen))

    val out   = (Decoupled(new IfuMessage))
    })


    val if_ready = Wire(Bool())

    val nextpc = Mux(io.branch.taken,io.branch.target,io.out.bits.pc+4.U)
    val pre_if_ready_go = true.B
    val pre_if_vaild  = pre_if_ready_go

    val if_ready_go = io.icache.resp.valid
    
    val if_vaild =  RegEnable(pre_if_vaild,if_ready)

    if_ready   := (!if_vaild)||(io.out.ready&&if_ready_go)
    
    io.icache.req.valid := if_ready
    io.icache.req.bits.addr:= nextpc
    io.icache.req.bits.data:= 0.U
    io.icache.req.bits.mask:= Fill(4,1.U)

    io.out.valid := if_vaild&&if_ready_go

    io.icache.resp.ready := true.B

    io.out.bits.pc := RegEnable(nextpc,0.U,pre_if_vaild && if_ready)
    io.out.bits.inst:= io.icache.resp.bits.data
}
