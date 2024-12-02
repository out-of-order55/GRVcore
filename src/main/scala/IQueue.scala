package IQueue

import chisel3._
import chisel3.util._


/////////////2W2R/////////
class WriterIO(size: Int) extends Bundle {
    val write = Input(Bool())
    val full = Output(Bool())
    val din = Input(UInt(size.W))
}

class ReaderIO(size: Int) extends Bundle {
    val read = Input(Bool())
    val empty = Output(Bool())
    val dout = Output(UInt(size.W))
}
class FIFO(size: Int,depth:Int) extends Module {

    val enq = IO(new WriterIO(size))
    val deq = IO(new ReaderIO(size))

    val dataReg = RegInit(VecInit(Seq.fill(depth)(0.U(size.W))))

    val wptr = RegInit(0.U((log2Ceil(depth)+1).W))
    val rptr = RegInit(0.U((log2Ceil(depth)+1).W))
    when(enq.write&&(!enq.full)){
        dataReg(wptr):= enq.din
        wptr := wptr+1.U
    }
    when(deq.read&&(!deq.empty)){
        
        rptr := rptr+1.U
    }
    deq.dout := dataReg(rptr)
    enq.full := (wptr((log2Ceil(depth)))===rptr((log2Ceil(depth))))&&(wptr((log2Ceil(depth)-1),0)===rptr((log2Ceil(depth)-1),0))
    deq.empty := (wptr((log2Ceil(depth)))=/=rptr((log2Ceil(depth))))&&(wptr((log2Ceil(depth)-1),0)===rptr((log2Ceil(depth)-1),0))
}
class IQWPort(size: Int) extends Bundle{
    val wen   = Input(Bool())
    val wdata = Input(UInt(size.W)) 
} 

class IQRPort(size: Int) extends Bundle{
    val ren   = Input(Bool())
    val rdata = Output(UInt(size.W)) 
} 


class IQueue(size:Int) extends Module{

    val wport0 = IO(new IQWPort(size))
    val wport1 = IO(new IQWPort(size))
    val rport0 = IO(new IQRPort(size))
    val rport1 = IO(new IQRPort(size))
    val full   = IO(Output(Bool()))
    val empty  = IO(Output(Bool()))
//   val bank0 = new Queue()
    val bank0 = Module(new FIFO(size,4))
    val bank1 = Module(new FIFO(size,4))
    val woffset = RegInit(0.U(1.W))
    val roffset = RegInit(0.U(1.W))

    val wcnt = Mux(wport0.wen&&wport1.wen,2.U,Mux(wport0.wen||wport1.wen,1.U,0.U))
    val rcnt = Mux(rport0.ren&&rport1.ren,2.U,Mux(rport0.ren||rport1.ren,1.U,0.U))

    woffset := woffset+wcnt
    roffset := woffset+rcnt

    bank0.enq.din := Mux(woffset===0.U,wport0.wdata,wport1.wdata)
    bank0.enq.write   := Mux(woffset===0.U,wport0.wen,wport1.wen)
    bank1.enq.din := Mux(woffset===0.U,wport1.wdata,wport0.wdata)
    bank1.enq.write   := Mux(woffset===0.U,wport1.wen,wport0.wen)

    
    bank0.deq.read :=  Mux(roffset===0.U,rport0.ren,rport1.ren)
    bank1.deq.read :=  Mux(roffset===0.U,rport1.ren,rport0.ren)

    rport0.rdata := Mux(roffset===0.U,bank0.deq.dout,bank1.deq.dout) 
    rport1.rdata := Mux(roffset===0.U,bank1.deq.dout,bank0.deq.dout) 
    full := bank0.enq.full&&bank1.enq.full
    empty:= bank0.deq.empty&&bank1.deq.empty
}
