package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

/* 

面积：16项：11000
8项：6400
（带两个寄存器阵列）
FTQ作用：
1.存储指令包的首地址
2.存储预测信息，以及预测器更新信息（meta）

所以需要的接口：
1.pc
2.预测target
3.meta

目前打算只在commit处理分支预测失败：
这样不用为redirect请求分配表项
 */
case class FtqParams(
    nEntries: Int = 8
)


//分支预测信息
class FTQBundle(implicit p: Parameters) extends  GRVBundle{
    val cfi_idx   = Valid(UInt(log2Ceil(fetchWidth).W))

    val cfi_taken = Bool()

    val cfi_mispredicted = Bool()

    val cfi_type = UInt(2.W)
    val br_mask   = UInt(fetchWidth.W)

    val is_jal   = Bool()
    val is_jalr  = Bool()
}

class BrUpdateInfo(implicit p: Parameters) extends  GRVBundle with HasFrontendParameters{
    val br_mask          = UInt(bankNum.W)
    val cfi_idx          = Valid(UInt(log2Ceil(fetchWidth).W))
    val cfi_taken        = Bool()
    val cfi_mispredicted = Bool()
    val cfi_is_call      = Bool()
    val cfi_is_ret       = Bool()
    val cfi_type         = UInt(2.W)
    val is_jal           = Bool()
    val is_jalr          = Bool()
    val target           = UInt(XLEN.W)
}
class GetPCFromFtqIO(implicit p: Parameters) extends GRVBundle
{
    val ftq_idx   = Input(UInt(log2Ceil(ftqentries).W))

    val entry     = Output(new FTQBundle)

    val pc        = Output(UInt(XLEN.W))
    val next_pc   = Output(UInt(XLEN.W))
    val next_pc_val=Output(Bool())


}

//我们只在commit阶段处理异常
class FetchTargetQueue(implicit p: Parameters) extends GRVModule with HasFrontendParameters{


    val io = IO(new Bundle{
        //
        val enq = Flipped(Decoupled(new FetchBundle()))
        val enq_idx = Output(UInt(log2Ceil(ftqentries).W))

        //commit 阶段
        val deq = Flipped(Valid(UInt(log2Ceil(ftqentries).W)))

        //分支指令执行阶段
        val get_ftq_pc = new GetPCFromFtqIO()
        //分支预测重定向：commit阶段
        val redirect = Input(Bool())//
        val brupdate = Input(new BrUpdateInfo)
        val bpdupdate = Output(Valid(new BranchPredictionUpdate))
        //更新RAS
        val ras_update = Output(new RASUpdate)
        
    })

    val pcs      = Reg(Vec(ftqentries, UInt(XLEN.W)))
    val brInfo   = Reg(Vec(ftqentries, new FTQBundle))

    //dual port
    val ghist    = SyncReadMem(ftqentries,UInt(globalHistoryLength.W))
    val meta     = SyncReadMem(ftqentries,UInt(bpdMaxMetaLength.W))

    val comm_ptr   = RegInit(0.U(log2Ceil(ftqentries).W))//提交阶段更新指针
    val deq_ptr    = RegInit(0.U(log2Ceil(ftqentries).W))
    val enq_ptr    = RegInit(0.U(log2Ceil(ftqentries).W))
    
    val full     = WireInit(false.B)

    io.enq.ready := !full//或者commit更新
    val do_enq   = WireInit(io.enq.fire)
    full := RegNext((enq_ptr+1.U)===deq_ptr)
    dontTouch(full)
    dontTouch(do_enq)
/////////////////enq logic////////////
    when(do_enq){
        enq_ptr := enq_ptr + 1.U
        pcs(enq_ptr)                  :=  io.enq.bits.pc
        brInfo(enq_ptr).cfi_idx       := io.enq.bits.cfi_idx
        brInfo(enq_ptr).cfi_taken     := io.enq.bits.cfi_taken
        brInfo(enq_ptr).cfi_mispredicted := false.B
        brInfo(enq_ptr).cfi_type      := io.enq.bits.cfi_type
        brInfo(enq_ptr).is_jal        := io.enq.bits.is_jal
        brInfo(enq_ptr).is_jalr       := io.enq.bits.is_jalr
        brInfo(enq_ptr).br_mask       := io.enq.bits.br_mask & io.enq.bits.mask
        meta.write(enq_ptr,io.enq.bits.bpd_meta)
        ghist.write(enq_ptr,io.enq.bits.ghist)
    }



    io.enq_idx := enq_ptr//指令包的唯一标示
/////////////////////////////////////
/* 
提交阶段会把指令包的指针送入ftq，
3   
2   
1   
*/
//////////////////commit logic//////////
    val do_commit_update = io.deq.valid
    
    val empty            = RegNext(deq_ptr+1.U===enq_ptr)
    val commit_mispred   = RegNext(io.redirect&&io.brupdate.cfi_mispredicted)
    val commit_brInfo    = RegNext(brInfo(deq_ptr))
    val commit_pc        = RegNext(pcs(deq_ptr))
    val commit_meta      = meta.read(deq_ptr,true.B)
    val commit_ghist     = ghist.read(deq_ptr,true.B)
    val commit_next_pc   = commit_pc + (io.brupdate.cfi_idx.bits<<2)+4.U 
    when(io.deq.valid){
        deq_ptr := deq_ptr + 1.U
    }
    //预测失败，恢复写指针
    when(io.redirect){
        enq_ptr := deq_ptr + 1.U
    }
    io.bpdupdate.valid := false.B
    io.bpdupdate.bits := DontCare
    io.ras_update.update_type.valid := false.B
    io.ras_update.update_type.bits  := DontCare
    io.ras_update.is_commit_update  := false.B
    io.ras_update.is_misspredict    := false.B
    io.ras_update.update_addr       := DontCare
    //提交的下个周期更新bp
    dontTouch(do_commit_update)
    when(RegNext(do_commit_update)){
        io.bpdupdate.valid          := true.B
        io.bpdupdate.bits.pc        := bankAlign(commit_pc)
        //如果预测成功，pcs deq_ptr的下一个pc必然是target
        io.bpdupdate.bits.target    := Mux(commit_mispred,RegNext(io.brupdate.target),RegNext(pcs(deq_ptr+1.U)))
        io.bpdupdate.bits.meta      := commit_meta
        io.bpdupdate.bits.ghist     := commit_ghist
        io.bpdupdate.bits.br_mask   := Mux(commit_mispred,RegNext(io.brupdate.br_mask),commit_brInfo.br_mask)
        io.bpdupdate.bits.cfi_idx   := Mux(commit_mispred,RegNext(io.brupdate.cfi_idx),commit_brInfo.cfi_idx)
        io.bpdupdate.bits.cfi_taken := RegNext(io.brupdate.cfi_taken)
        io.bpdupdate.bits.cfi_type  := Mux(commit_mispred,RegNext(io.brupdate.cfi_type),commit_brInfo.cfi_type)
        io.bpdupdate.bits.is_mispredict_update:=commit_mispred
        io.bpdupdate.bits.is_commit_update:= commit_mispred
        io.bpdupdate.bits.is_jal    := Mux(commit_mispred,RegNext(io.brupdate.is_jal),commit_brInfo.is_jal)
        io.bpdupdate.bits.is_jalr   := Mux(commit_mispred,RegNext(io.brupdate.is_jalr),commit_brInfo.is_jalr)
        //更新commitRAS
        io.ras_update.update_type.valid:= RegNext(do_commit_update)
        io.ras_update.update_type.bits := io.bpdupdate.bits.cfi_type
        io.ras_update.is_commit_update:= RegNext(do_commit_update)
        io.ras_update.is_misspredict  := commit_mispred
        io.ras_update.update_addr     := Mux(io.bpdupdate.bits.cfi_type===1.U,commit_next_pc,0.U)
    }

/////////////////////backend get pc///////////////
    val get_ftq_idx= io.get_ftq_pc.ftq_idx
///如果issue指令为br，则在issue读，ex阶段出结果
    io.get_ftq_pc.entry  := RegNext(brInfo(get_ftq_idx))
    io.get_ftq_pc.pc     := RegNext(pcs(get_ftq_idx))
    io.get_ftq_pc.next_pc:= RegNext(pcs(get_ftq_idx+1.U))
    io.get_ftq_pc.next_pc_val:=RegNext(get_ftq_idx+1.U=/=enq_ptr||do_enq)
}