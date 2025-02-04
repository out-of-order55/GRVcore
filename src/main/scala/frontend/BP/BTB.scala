package grvcore
import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._



case class BTBParams(
    nSets:  Int = 64,
    nWays:  Int = 2,
    offsetSz: Int = 21
)


/*
为每个bank都分配了表项，目前会浪费SRAM，之后可能会做出限制，比如一个fetchpacket，最多存两条taken的分支
主体逻辑3500
去掉跨行为2200
BTB对call和ret类型预测准确（95%以上），但对br并不好，原因：
BTB只记录taken的指令，而br可能是000001（循环），所以会一直不记录这条指令
该程序问题（cpu-test的add测试）
*/
class BTBBranchPredictor(implicit p: Parameters) extends BasePredictor()(p){
    val BTBnSets      = btbParams.nSets
    val tagSz         = XLEN - log2Ceil(BTBnSets) - fetchWidth 
    val offsetSz      = btbParams.offsetSz
    class BTBEntry extends Bundle{
        val br_type = UInt(2.W)
        val offset  = UInt(offsetSz.W)
        def is_br  = br_type===1.U
        def is_call= br_type===2.U
        def is_ret = br_type===3.U
    }
    val btbEntrySz = offsetSz + 2
    class BTBMeta extends  Bundle{
        val tag = UInt(tagSz.W)
    }
    class BTBPredMeta extends  Bundle{
        val hit = Bool()
    }
    val s2_meta           = Wire(new BTBPredMeta)
    override val metaSz   = s2_meta.asUInt.getWidth

    val mems = Seq(
    (f"btb_meta", BTBnSets, bankWidth * metaSz),
    (f"btb_data", BTBnSets, bankWidth * btbEntrySz))
    val btb = Seq.fill(bankNum)(Module(new SRAMHelper(BTBnSets,new BTBEntry)))
    val meta = Module(new SRAMHelper(BTBnSets,new BTBMeta))

    val doing_reset = RegInit(true.B)//调试置为false
    val reset_idx = RegInit(0.U(log2Ceil(BTBnSets).W))
    val reset_wbtb_data = Wire(new BTBEntry)
    // val s1_update_wmeta_data    = Wire(new BTBMeta)
    reset_wbtb_data.offset        := 0.U   
    reset_wbtb_data.br_type       := 0.U 
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (BTBnSets-1).U) { doing_reset := false.B }

    //S0阶段处理读的信号



    val s1_update_meta            = s1_update.bits.meta.asTypeOf(new BTBPredMeta)
    val s1_update_hit             = s1_update_meta.hit
    val s1_update_index           = s1_update_idx



    dontTouch(s1_update_meta)
    val new_offset_value = (s1_update.bits.target.asSInt -
        (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits<<2)).asSInt)
    dontTouch(new_offset_value)
    val s1_update_cfi_idx   = s1_update.bits.cfi_idx.bits
    val s1_update_wbtb_data     = Wire(Vec(bankNum,new BTBEntry))

    for(i <- 0 until bankNum){
        when(i.U===s1_update_cfi_idx){
            s1_update_wbtb_data(i).offset    := new_offset_value.asUInt
            s1_update_wbtb_data(i).br_type   := s1_update.bits.cfi_type 
        }.otherwise{
            s1_update_wbtb_data(i).offset    := 0.U
            s1_update_wbtb_data(i).br_type   := 0.U
        }
    }
    // s1_update_wbtb_data.offset    := new_offset_value.asUInt
    // s1_update_wbtb_data.br_type   := s1_update.bits.cfi_type 
    val s1_commit_update_valid =  s1_update_valid  && s1_update.bits.is_commit_update
    val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
        Fill(bankNum, s1_update.bits.cfi_idx.valid && s1_commit_update_valid))|
        Fill(bankNum,s1_commit_update_valid&&(!s1_update_hit))
    val s1_update_taken =  s1_update.bits.cfi_idx.valid && s1_update.valid  && s1_update.bits.is_commit_update


    
    val s2_req_rdata = Reg(Vec(bankNum,new BTBEntry))

    val s1_btb_meta  = Wire(new BTBMeta)

    //check hit
    val s1_hit = s1_btb_meta.tag===(s1_pc)(XLEN-1,XLEN-tagSz)
    
    val s2_hit = RegNext(s1_hit)

    s2_meta.hit := s2_hit

    io.f3_meta := RegNext(s2_meta.asUInt)
/*
读需要读第一个预测taken的，写需要写实际taken的//一个指令包顶多一个
*/

    val update_valid =  s1_update.valid && s1_update.bits.is_commit_update
    val idx   = Mux(doing_reset,reset_idx, Mux(update_valid,s1_update_index,Mux(s0_valid,s0_idx,0.U)))
    val enable = s1_update_taken||s0_valid ||doing_reset
    // val write  = 
    meta.io.enable      := enable
    meta.io.addr        := idx
    meta.io.write       := doing_reset||(s1_update_taken&(!s1_update_meta.hit))
    meta.io.dataIn.tag  := Mux(doing_reset,0.U,(s1_update.bits.pc)(XLEN-1,XLEN-tagSz))
    s1_btb_meta         :=  meta.io.dataOut 


    for (w <- 0 until bankNum) { 
        val update_valid =  (s1_update.valid && s1_update.bits.is_commit_update)
        val idx   =  Mux(doing_reset,reset_idx, Mux(update_valid,s1_update_index,Mux(s0_valid,s0_idx,0.U))) 
        btb(w).io.enable  := s1_update_wbtb_mask(w)||s0_valid ||doing_reset
        btb(w).io.addr    := idx 
        btb(w).io.write   := Mux(doing_reset,true.B,s1_update_wbtb_mask(w)) 
        btb(w).io.dataIn  := Mux(doing_reset,reset_wbtb_data,s1_update_wbtb_data(w))
        s2_req_rdata(w)   := btb(w).io.dataOut
    }
    // s2_req_rdata := DontCare
    for(w <- 0 until bankNum){
        io.resp.f2(w).predicted_pc.bits := (s2_req_rdata(w).offset.asSInt + (s2_pc + (PopCount(~s2_mask(bankNum-1,0))<<2)).asSInt).asUInt
        io.resp.f2(w).predicted_pc.valid:= s2_req_rdata(w).br_type=/=0.U&s2_hit
        io.resp.f2(w).br_type           := s2_req_rdata(w).br_type
        // io.resp.f2(w).is_br        := s2_req_rdata(w).is_br
        // io.resp.f2(w).is_call      := s2_req_rdata(w).is_call
        // io.resp.f2(w).is_ret       := s2_req_rdata(w).is_ret
        io.resp.f3(w) := RegNext(io.resp.f2(w))
    }


}