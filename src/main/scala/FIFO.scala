package FIFO
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.UIntTransform
abstract class Fifo[T <: Data](gen: T, val depth: Int) extends Module {
    val io = IO(new FifoIO(gen))
    assert(depth > 0, "Number of buffer elements needs to be larger than 0.")
}
class FifoIO[T <: Data](private val gen: T) extends Bundle {
    val enq = Flipped(new DecoupledIO(gen))
    val deq = new DecoupledIO(gen)
}


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
class FIFORegister(size: Int) extends Module {
    val io = IO(new Bundle {
        val enq = new WriterIO(size)
        val deq = new ReaderIO(size)
    })
    
    val empty :: full :: Nil = Enum(2)
    val stateReg = RegInit(empty)
    val dataReg = RegInit(0.U(size.W))

    when(stateReg === empty) {
        when(io.enq.write) {
            stateReg := full
            dataReg := io.enq.din
        }
    } .elsewhen(stateReg === full) {
        when(io.deq.read) {
            stateReg := empty
            dataReg := 0.U      // 单纯方便在波形图中看是不是空了
        }
    } .otherwise {
        // 也应该没有“否则”的状态了
    }

    io.enq.full := stateReg === full
    io.deq.empty := stateReg === empty
    io.deq.dout := dataReg
}

class BubbleFifo(size: Int, depth: Int) extends Module {
    val io = IO(new Bundle {
        val enq = new WriterIO(size)
        val deq = new ReaderIO(size)
    })

    val buffers = Array.fill(depth) {
        Module(new FIFORegister(size))
    }

    for (i <- 0 until depth - 1) {
        buffers(i + 1).io.enq.din := buffers(i).io.deq.dout
        buffers(i + 1).io.enq.write := buffers(i).io.deq.read
        buffers(i).io.deq.read := ~buffers(i + 1).io.enq.full
    }

    io.enq <> buffers(0).io.enq
    io.deq <> buffers(depth - 1).io.deq
}


class MyQueue extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(8.W))) // 输入端口
    val deq = Decoupled(UInt(8.W)) // 输出端口
  })

  // 创建一个深度为4的FIFO队列
  val queue = Module(new Queue(UInt(8.W), 4,pipe=true,useSyncReadMem=true))
  
  // 连接输入端口到队列的写端口
  queue.io.enq <> io.enq

  // 连接输出端口到队列的读端口
  queue.io.deq <> io.deq
}


