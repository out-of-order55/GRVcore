package grvcore
import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._


class BIMMeta(implicit p: Parameters) extends GRVBundle{
    val bim = UInt(2.W)
}

case class BIMParams(
    nSets: Int = 512
    
)
//s2阶段出结果
/* 
之所以要bypass：
bim在提交时更新，更新时是根据自己携带的meta更新，而不是重新去读bim表，
如果有两条分支，第一条已经提交并且更新bim，当第二条提交时，此时bim值不是最新的，在bim表中的才是最新的，所以要bypass最新写入的值
目前准备写支持跨指令包的预测器，预测要对整个指令包的每个指令预测,然后前端选择出第一个预测taken的指令
 */

class BIMBranchPredictor(implicit p: Parameters) extends BasePredictor
{
    override val nSets = bimParams.nSets

    require(isPow2(nSets))

    val nWrBypassEntries = 2

    def bimWrite(v: UInt, taken: Bool): UInt = {
        val old_bim_sat_taken  = v === 3.U
        val old_bim_sat_ntaken = v === 0.U
        Mux(old_bim_sat_taken  &&  taken, 3.U,
        Mux(old_bim_sat_ntaken && !taken, 0.U,
        Mux(taken, v + 1.U, v - 1.U)))
    }
    val s2_meta           = Wire(new BIMMeta)
    override val metaSz   = s2_meta.asUInt.getWidth

    val doing_reset = RegInit(true.B)
    val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (nSets-1).U) { doing_reset := false.B }


    val data  = Vec(bankWidth,SyncReadMem(nSets,UInt(2.W)))

    // val mems = Seq(("bim", nSets, bankWidth * 2))
    //处理跨行请求
    val crossRow        = (!(io.f0_pc(offsetWidth-1,0)===0.U))&s0_valid
    val bankoh          = UIntToOH(bankoffset(io.f0_pc))
    val bankmask        = MaskUpper(bankoh)>>bankoffset(io.f0_pc)
    for(i <- 0 until bankWidth){
        val idx   = Mux(bankmask(i)===1.U,s0_idx,s0_idx+1.U) 
        val valid = s0_valid 
        val ptr   = (i+bankoffset(io.f0_pc))%bankWidth 
        s2_req_rdata(i) := data(ptr).read(idx   , valid)
    }


    val s2_resp         = Wire(Vec(bankWidth, Bool()))

    for (w <- 0 until bankWidth) {

        s2_resp(w)        := s2_valid && s2_req_rdata(w)(1) && !doing_reset
        s2_meta.bims(w)   := s2_req_rdata(w)
    }


    val s1_update_wdata   = Wire(Vec(bankWidth, UInt(2.W)))
    val s1_update_wmask   = Wire(Vec(bankWidth, Bool()))
    val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BIMMeta)
    val s1_update_index   = s1_update_idx

    val s1_update_crossRow        = (!(s1_update.pc(offsetWidth-1,0)===0.U))&s1_update.valid
    val s1_update_bankoh          = UIntToOH(bankoffset(s1_update.pc))
    val s1_update_bankmask        = MaskUpper(bankoh)

    val wrbypass_idxs    = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nSets).W)))
    val wrbypass         = Reg(Vec(nWrBypassEntries, Vec(bankWidth, UInt(2.W))))
    val wrbypass_bankmask= Reg(Vec(nWrBypassEntries, UInt(bankWidth.W)))
    val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

    val wrbypass_hits = VecInit((0 until nWrBypassEntries) map { i =>
        !doing_reset &&
        wrbypass_idxs(i) === s1_update_index(log2Ceil(nSets)-1,0)&&
        (wrbypass_bankmask(i)===s1_update_bankmask)
    })
    val wrbypass_hit = wrbypass_hits.reduce(_||_)
    val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)



    for (w <- 0 until bankWidth) {
        s1_update_wmask(w)         := false.B
        s1_update_wdata(w)         := DontCare
        when (s1_update.bits.br_mask(w)&&s1_update.bits.cfi_idx.valid && s1_update.bits.cfi_idx.bits === w.U) {
            val was_taken = (
                s1_update.bits.cfi_idx.valid &&
                (s1_update.bits.cfi_idx.bits === w.U) &&
                (s1_update.bits.is_br && s1_update.bits.br_mask(w) && s1_update.bits.br_taken) 
            )
            val old_bim_value = Mux(wrbypass_hit, wrbypass(wrbypass_hit_idx)(w), s1_update_meta.bims(w))
            s1_update_wmask(w)     := true.B
            s1_update_wdata(w)     := bimWrite(old_bim_value, was_taken)
        }
    }
//

    when (doing_reset || (s1_update.valid && s1_update.bits.is_commit_update)) {
        val idx   = Mux(s1_update_bankmask(i)===1.U,s1_update_index,s1_update_index+1.U) 
        for (w <- 0 until bankWidth) { 
            data(w).write(
            Mux(doing_reset, reset_idx, idx),
            Mux(doing_reset,  2.U , s1_update_wdata(w)),
            Mux(doing_reset, (~(0.U(bankWidth.W))), s1_update_wmask.asUInt).asBools
            )
        }
    }
    when (s1_update_wmask.reduce(_||_) && s1_update.valid && s1_update.bits.is_commit_update) {
        when (wrbypass_hit) {
            wrbypass(wrbypass_hit_idx)         := s1_update_wdata
            wrbypass_bankmask(wrbypass_hit_idx):= s1_update_bankmask
        } .otherwise {
            wrbypass(wrbypass_enq_idx)      := s1_update_wdata
            wrbypass_idxs(wrbypass_enq_idx) := s1_update_index
            wrbypass_bankmask(wrbypass_enq_idx):= s1_update_bankmask
            wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
        }
    }

    for (w <- 0 until bankWidth) {
        io.resp.f2(w).taken := s2_resp(w)
        io.resp.f3(w).taken := RegNext(io.resp.f2(w).taken)
    }
    io.f3_meta := RegNext(s2_meta.asUInt)
}