package grvcore
import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class RAS(nras: Int) {
    def push(addr: UInt): Unit = {
        when (count < nras.U) { count := count + 1.U }
            val nextPos = Mux((isPow2(nras)).B || pos < (nras-1).U, pos+1.U, 0.U)
            stack(nextPos) := addr
            pos := nextPos
    }
    def peek: UInt = stack(pos)
    def pop(): Unit = when (!isEmpty) {
        count := count - 1.U
        pos := Mux((isPow2(nras)).B || pos > 0.U, pos-1.U, (nras-1).U)
    }
    def clear(): Unit = count := 0.U
    def isEmpty: Bool = count === 0.U
    private val count = RegInit(0.U(log2Up(nras+1).W))
    private val pos = RegInit(0.U(log2Up(nras).W))
    private val stack = Reg(Vec(nras, UInt()))
}
class RASEntry(implicit p: Parameters)extends GRVBundle{
    val   stack = UInt(XLEN.W)
    val   ctr   = UInt(3.W)
}

class RASResp(implicit p: Parameters)extends GRVBundle{
    val br_type   = Input(Valid(UInt(2.W)))
    val read_addr = Output(UInt(XLEN.W))
    val write_addr= Input(UInt(XLEN.W))
    def is_call= br_type.bits===2.U
    def is_ret= br_type.bits===3.U
}
class RASUpdate(implicit p: Parameters)extends GRVBundle{
    val update_addr      = Input(UInt(XLEN.W))
    val update_type      = Input(Valid(UInt(2.W)))
    val is_commit_update = Input(Bool())
    val is_misspredict   = Input(Bool())
    def is_call= update_type.bits===2.U
    def is_ret = update_type.bits===3.U
}
/* 
RAS首先要有读地址，写地址，更新信号，如果分支预测失败，需要更新RAS，具体的，将commitRAS复制给sepcRAS，指针同样,
RAS在s0阶段更新，s2阶段读出或者写入
面积4000
 */
class GRVRAS(implicit p: Parameters) extends GRVModule with HasFrontendParameters{
    val io = IO(new Bundle {
        val rasResp   = new RASResp
        val rasUpdate = new RASUpdate
    })

    val commit_ras = Reg(Vec(numRAS, new RASEntry))
    val commit_ptr = RegInit(0.U(log2Ceil(numRAS).W))

    val sepc_ptr   = RegInit(0.U(log2Ceil(numRAS).W))
    val sepc_ras   = Reg(Vec(numRAS, new RASEntry))

    val is_sepc_recursion = io.rasResp.write_addr===sepc_ras(commit_ptr-1.U).stack+1.U
    val is_update_recursion =  io.rasUpdate.update_addr===commit_ras(commit_ptr-1.U).stack
    val call_update_ctr = io.rasUpdate.is_call&is_update_recursion
    val ret_update_ctr  = io.rasUpdate.is_ret&commit_ras(commit_ptr-1.U).ctr=/=0.U
    dontTouch(call_update_ctr)
    dontTouch(ret_update_ctr)
    /*  
    预测失败情况：
    1.预测失败的是call
    2.预测失败的是ret
    3.预测失败的是其他指令
    
    */
    when(io.rasUpdate.is_misspredict){
        when(io.rasUpdate.is_call){
            for(i <- 0 until numRAS){
                when(call_update_ctr){
                    when(i.U===commit_ptr-1.U){
                        sepc_ras(commit_ptr-1.U).ctr := commit_ras(commit_ptr-1.U).ctr + 1.U
                    }.otherwise{
                        sepc_ras(i) := commit_ras(i)
                    }
                    
                }.otherwise{
                    when(i.U===commit_ptr){
                        sepc_ras(i).stack := io.rasUpdate.update_addr
                    }
                }
            }
            sepc_ptr := Mux(call_update_ctr,commit_ptr,commit_ptr + 1.U)
        }.elsewhen(io.rasUpdate.is_ret){
            for(i <- 0 until numRAS){
                when(ret_update_ctr){
                    when(i.U===commit_ptr-1.U){
                        sepc_ras(i).ctr := commit_ras(i).ctr -1.U
                    }
                }
            }
            sepc_ptr := Mux(ret_update_ctr,commit_ptr,commit_ptr-1.U)
        }.otherwise{
            for(i <- 0 until numRAS){
                sepc_ras(i) := commit_ras(i)
            }
            sepc_ptr := commit_ptr
        }
    }.elsewhen(io.rasResp.br_type.valid){
        when(io.rasResp.is_call){
            when(is_sepc_recursion){
                sepc_ras(sepc_ptr-1.U).ctr   := sepc_ras(sepc_ptr-1.U).ctr + 1.U
                
            }.otherwise{
                sepc_ras(sepc_ptr).stack := io.rasResp.write_addr
                sepc_ptr := sepc_ptr + 1.U
            }
        }
        when(io.rasResp.is_ret){
            when(sepc_ras(sepc_ptr-1.U).ctr=/=0.U){
                sepc_ras(sepc_ptr).ctr :=  sepc_ras(sepc_ptr).ctr - 1.U
            }.otherwise{
                sepc_ptr := sepc_ptr + 1.U
            }
        }
    }
    io.rasResp.read_addr := Mux(io.rasResp.is_ret&io.rasResp.br_type.valid,sepc_ras(sepc_ptr-1.U).stack,0.U)
    
    dontTouch(is_update_recursion)
    when(io.rasUpdate.update_type.valid&&io.rasUpdate.is_commit_update){
        when(io.rasUpdate.is_call){
            when(is_update_recursion){
                commit_ras(commit_ptr-1.U).ctr   := commit_ras(commit_ptr-1.U).ctr + 1.U
                // commit_TOSW := commit_TOSW-1.U
            }.otherwise{
                commit_ras(commit_ptr).stack := io.rasUpdate.update_addr
                commit_ptr := commit_ptr + 1.U
            }
        }
        when(io.rasUpdate.is_ret){
            when(commit_ras(commit_ptr-1.U).ctr=/=0.U){
                commit_ras(commit_ptr-1.U).ctr := commit_ras(commit_ptr-1.U).ctr-1.U
            }.otherwise{
                commit_ptr := commit_ptr - 1.U
            }
        }
    }
}