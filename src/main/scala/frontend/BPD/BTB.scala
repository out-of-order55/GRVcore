package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink.TLRationalImp.bundle


case class BTBParams(
    nSets: Int = 128,
    nWays: Int = 2,
    offsetSz: Int = 21
)



class BTBBranchPredictor(implicit p: Parameters) extends BasePredictor()(p){
    val BTBnSets      = btbParams.getOrElse(BTBParams()).nSets
    val tagSz         = XLEN - log2Ceil(BTBnSets) - log2Ceil(fetchWidth) - 1
    val offsetSz      = btbParams.getOrElse(BTBParams()).offsetSz
    class BTBEntry extends Bundle{
        val br_type = UInt(2.W)
        val offset  = UInt(offsetSz.W)
    }
    class BTBMeta extends  Bundle{
        val tag = UInt(tagSz.W)
    }
    class BTBPredMeta extends  Bundle{
        val hit = UInt(2.W)
    }
    val s2_meta           = Wire(new BTBPredMeta)
    override val metaSz   = s2_meta.asUInt.getWidth

    val btb = Seq.fill(bankNum)(Module(new SRAMHelper(BTBnSets,new BTBEntry)))
    val meta = Seq.fill(2)(Module(new SRAMHelper(BTBnSets,new BTBMeta)))//for crossrow access

    val doing_reset = RegInit(true.B)
    val reset_idx = RegInit(0.U(log2Ceil(BTBnSets).W))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (BTBnSets-1).U) { doing_reset := false.B }
    //S0阶段处理读的信号
    val crossRow        = (!(io.f0_pc(offsetWidth-1,0)===0.U))&s0_valid
    val bankoh          = UIntToOH(bankoffset(io.f0_pc))
    val bankmask        = MaskUpper(bankoh)>>bankoffset(io.f0_pc)


    val s1_update_meta            = s1_update.bits.meta.asTypeOf(new BTBPredMeta)
    val s1_update_index           = s1_update_idx
    val s1_update_crossRow        = (!(s1_update.bits.pc(offsetWidth-1,0)===0.U))&s1_update.valid
    val s1_update_bankoh          = UIntToOH(bankoffset(s1_update.bits.pc))
    val s1_update_bankmask        = MaskUpper(s1_update_bankoh)
    val s1_update_bankmask_meta        = MaskUpper(s1_update_bankoh)>>bankoffset(s1_update.bits.pc)

    val new_offset_value = (s1_update.bits.target.asSInt -
        (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 2)).asSInt)
    val s1_update_cfi_idx   = s1_update.bits.cfi_idx.bits
    val s1_update_wbtb_data     = Wire(new BTBEntry)
    val s1_update_wmeta_data    = Wire(new BTBMeta)

    s1_update_wbtb_data.offset := new_offset_value
    val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
        Fill(bankNum, s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update))
    val s1_update_taken =  s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update
    val s1_update_wmeta_mask = UInt(2.W)
    s1_update_wmeta_mask(0) := (!s1_update_meta.hit(0))&s1_update_taken&
                    (UIntToOH(s1_update_cfi_idx)&s1_update_bankmask_meta)=/=0.U
    s1_update_wmeta_mask(1) := (!s1_update_meta.hit(1))&s1_update_taken&
                    (UIntToOH(s1_update_cfi_idx)&s1_update_bankmask_meta)===0.U
    
    val s2_req_rdata = Vec(bankNum,new BTBEntry)
    val s1_btb_meta  = Vec(2,new BTBMeta)
/*
读需要读第一个预测taken的，写需要写实际taken的//一个指令包顶多一个
*/
    for (w <- 0 until 2) { 
        val update_valid =  s1_update.valid && s1_update.bits.is_commit_update

        val idx   = Mux(update_valid,s1_update_index,
                    Mux(s0_valid,s0_idx,reset_idx))
        meta(w).io.enable  := s1_update_wmeta_mask(w)||s0_valid ||doing_reset
        meta(w).io.addr    := idx+w.U 
        meta(w).io.write   := doing_reset||s1_update_wmeta_mask(w)
        meta(w).io.dataIn  := s1_update.bits.pc(XLEN-1,XLEN-tagSz)
        val ptr   = (w.U+bankoffset(io.f0_pc))%bankNum.U 

        // for(j <- 0 until bankNum){
        //     when(j.U===ptr){
        //         s2_req_rdata(w) := RegNext(btb(j).io.dataOut)
        //     }
        // }
    }
    for (w <- 0 until bankNum) { 
        val update_valid =  s1_update.valid && s1_update.bits.is_commit_update

        val idx   = Mux(update_valid,Mux(s1_update_bankmask(w)===1.U,s1_update_index,s1_update_index+1.U),
                    Mux(s0_valid,Mux(bankmask(w)===1.U,s0_idx,s0_idx+1.U),reset_idx))
        btb(w).io.enable  := s1_update_wbtb_mask(w)||s0_valid ||doing_reset
        btb(w).io.addr    := idx 
        btb(w).io.write   := Mux(doing_reset,true.B,s1_update_wbtb_mask(w)) 
        btb(w).io.dataIn  := s1_update_wbtb_data
        val ptr   = (w.U+bankoffset(io.f0_pc))%bankNum.U 
        for(j <- 0 until bankNum){
            when(j.U===ptr){
                s2_req_rdata(w) := RegNext(btb(j).io.dataOut)
            }
        }
    }


}