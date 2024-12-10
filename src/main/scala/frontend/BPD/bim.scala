package grvcore
import chisel3._
import chisel3.util._


import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._


class BIMMeta(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val bims = Vec(bankNum,UInt(2.W))
}

case class BIMParams(
    nSets: Int = 512
    
)
//s2阶段出结果
/* 
改编自boom
之所以要bypass：
bim在提交时更新，更新时是根据自己携带的meta更新，而不是重新去读bim表，
如果有两条分支，第一条已经提交并且更新bim，当第二条提交时，此时bim值不是最新的，在bim表中的才是最新的，所以要bypass最新写入的值
目前准备写支持跨指令包的预测器，预测要对整个指令包的每个指令预测,然后前端选择出第一个预测taken的指令
主体逻辑面积1300（均不带SRAM）

后端送入的pc为指令包的首pc，
 */

class BIMBranchPredictor(implicit p: Parameters) extends BasePredictor  
{
    // override val nSets = bimParams.nSets
    val BimnSets = bimParams.getOrElse(BIMParams()).nSets
    require(isPow2(BimnSets))

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
    val reset_idx = RegInit(0.U(log2Ceil(BimnSets).W))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (BimnSets-1).U) { doing_reset := false.B }


    val data  = Seq.fill(bankNum)(Module(new SRAMHelper(BimnSets,UInt(2.W))))

    // val mems = Seq(("bim", nSets, bankWidth * 2))
    //处理跨行请求
    val crossRow        = (!(io.f0_pc(offsetWidth-1,0)===0.U))&s0_valid
    val bankoh          = UIntToOH(bankoffset(io.f0_pc))
    val bankmask        = MaskUpper(bankoh)>>bankoffset(io.f0_pc)
    val s2_req_rdata    = Reg(Vec(bankNum,UInt(2.W)))



    val s2_resp         = Wire(Vec(bankNum, Bool()))

    
    for (w <- 0 until bankNum) {

        s2_resp(w)        := s2_valid && s2_req_rdata(w)(1) && !doing_reset
        s2_meta.bims(w)   := s2_req_rdata(w)
    }


    val s1_update_wdata   = Wire(Vec(bankNum, UInt(2.W)))
    val s1_update_wmask   = Wire(Vec(bankNum, Bool()))
    val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BIMMeta)
    val s1_update_index   = s1_update_idx

    val s1_update_crossRow        = (!(s1_update.bits.pc(offsetWidth-1,0)===0.U))&s1_update.valid
    val s1_update_bankoh          = UIntToOH(bankoffset(s1_update.bits.pc))
    val s1_update_bankmask        = MaskUpper(s1_update_bankoh)

    val wrbypass_idxs    = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(BimnSets).W)))
    val wrbypass         = Reg(Vec(nWrBypassEntries, Vec(bankNum, UInt(2.W))))
    val wrbypass_bankmask= Reg(Vec(nWrBypassEntries, UInt(bankNum.W)))
    val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

    val wrbypass_hits = VecInit((0 until nWrBypassEntries) map { i =>
        !doing_reset &&
        wrbypass_idxs(i) === s1_update_index(log2Ceil(BimnSets)-1,0)&&
        (wrbypass_bankmask(i)===s1_update_bankmask)
    })
    val wrbypass_hit = wrbypass_hits.reduce(_||_)
    val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)


    for (w <- 0 until bankNum) { 
        val update_valid =  s1_update.valid && s1_update.bits.is_commit_update
        val wen   = doing_reset || (s1_update.valid && s1_update.bits.is_commit_update)&&
                    s1_update_wmask.reduce(_|_)
        val idx   = Mux(update_valid,Mux(s1_update_bankmask(w)===1.U,s1_update_index,s1_update_index+1.U),
                    Mux(s0_valid,Mux(bankmask(w)===1.U,s0_idx,s0_idx+1.U),reset_idx))
        data(w).io.enable  := wen||s0_valid 
        data(w).io.addr    := idx 
        data(w).io.write   := wen 
        data(w).io.dataIn  := s1_update_wdata(w)
        val ptr   = (w.U+bankoffset(io.f0_pc))%bankNum.U 
        for(j <- 0 until bankNum){
            when(j.U===ptr){
                s2_req_rdata(w) := RegNext(data(j).io.dataOut)
            }
        }
    }

    for (w <- 0 until bankNum) {
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


    when (s1_update_wmask.reduce(_||_) && s1_update.valid && s1_update.bits.is_commit_update) {
        when (wrbypass_hit) {
            wrbypass(wrbypass_hit_idx)         := s1_update_wdata
            wrbypass_bankmask(wrbypass_hit_idx):= s1_update_bankmask
        } .otherwise {
            wrbypass(wrbypass_enq_idx)      := s1_update_wdata
            wrbypass_idxs(wrbypass_enq_idx) := s1_update_index
            wrbypass_bankmask(wrbypass_enq_idx):= s1_update_bankmask
            wrbypass_enq_idx := (wrbypass_enq_idx)+1.U
        }
    }

    for (w <- 0 until bankNum) {
        io.resp.f2(w).taken := s2_resp(w)
        io.resp.f3(w).taken := RegNext(io.resp.f2(w).taken)
    }
    io.f3_meta := RegNext(s2_meta.asUInt)
}