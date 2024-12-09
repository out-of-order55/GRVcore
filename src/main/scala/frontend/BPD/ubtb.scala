package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

case class MicroBTBParams(
    nWays: Int = 128,
    offsetSz: Int = 21
)


class MicroBTBBranchPredictor(implicit p: Parameters) extends BasePredictor
{
    val ubtbnWays     = ubtbParams.getOrElse(MicroBTBParams()).nWays
    val tagSz         = XLEN - log2Ceil(fetchWidth) - 1
    val offsetSz      = ubtbParams.getOrElse(MicroBTBParams()).offsetSz
    val nWrBypassEntries = 2


    def bimWrite(v: UInt, taken: Bool): UInt = {
        val old_bim_sat_taken  = v === 3.U
        val old_bim_sat_ntaken = v === 0.U
        Mux(old_bim_sat_taken  &&  taken, 3.U,
        Mux(old_bim_sat_ntaken && !taken, 0.U,
        Mux(taken, v + 1.U, v - 1.U)))
    }

    require(isPow2(ubtbnWays))

    class MicroBTBEntry extends Bundle {
        val offset   = SInt(offsetSz.W)
    }

    class MicroBTBMeta extends Bundle {
        val is_br = Bool()
        val tag   = UInt(tagSz.W)
        val ctr   = UInt(2.W)
    }

    class MicroBTBPredictMeta extends Bundle {
        val hits        = Vec(bankNum, Bool())
        val write_way   = Vec(2,UInt(log2Ceil(ubtbnWays).W))
        val crossRow    = Bool()
    }

    val s1_meta = Wire(new MicroBTBPredictMeta)
    override val metaSz = s1_meta.asUInt.getWidth


    val meta     = RegInit((0.U).asTypeOf(Vec(ubtbnWays, Vec(bankNum, new MicroBTBMeta))))
    val btb      = Reg(Vec(ubtbnWays, Vec(bankNum, new MicroBTBEntry)))

    // val mems = Nil
    val s1_crossRow     = (!(s1_pc(offsetWidth-1,0)===0.U))&s1_valid
    val s1_bankoh       = UIntToOH(bankoffset(s1_pc))
    val s1_bankmask     = MaskUpper(s1_bankoh)>>bankoffset(s1_pc)
    val s1_req_tag      = s1_idx

    val cnt = RegInit(0.U(log2Ceil(ubtbnWays).W))
    cnt := cnt + 1.U
    val s1_resp   = Wire(Vec(bankNum, Valid(UInt(XLEN.W))))
    val s1_taken  = Wire(Vec(bankNum, Bool()))
    val s1_is_br  = Wire(Vec(bankNum, Bool()))
    val s1_is_jal = Wire(Vec(bankNum, Bool()))

    val s1_hit_ohs = VecInit((0 until bankNum) map { i =>
        val tag = Mux(s1_bankmask(i),s1_req_tag,s1_req_tag+1.U)
        VecInit((0 until ubtbnWays) map { w =>
        meta(w)(i).tag === tag(tagSz-1,0)
        })
    })
    val s1_hits     = s1_hit_ohs.map { oh => oh.reduce(_||_) }
    val s1_hit_ways = s1_hit_ohs.map { oh => PriorityEncoder(oh) }


    for (w <- 0 until bankNum) {
        val entry_meta = meta(s1_hit_ways(w))(w)
        s1_resp(w).valid := s1_valid && s1_hits(w)
        s1_resp(w).bits  := (s1_pc.asSInt + (w << 2).S + btb(s1_hit_ways(w))(w).offset).asUInt
        s1_is_br(w)      := s1_resp(w).valid &&  entry_meta.is_br
        s1_is_jal(w)     := s1_resp(w).valid && !entry_meta.is_br
        s1_taken(w)      := !entry_meta.is_br || entry_meta.ctr(1)

        s1_meta.hits(w)  := s1_hits(w)
    }
    val alloc_way = cnt//不能和hit的way一样
    //如果两个way都命中，直接写入hitway，如果一个命中，就写入hitWay的下一个，如果都没命中，写入allocway和+1
    val s1_hit0   = Wire(Vec(bankNum,Bool()))
    val s1_hit1   = Wire(Vec(bankNum,Bool()))
    val s1_hit0_oh= Wire(Vec(bankNum,UInt(ubtbnWays.W)))
    val s1_hit1_oh= Wire(Vec(bankNum,UInt(ubtbnWays.W)))
    for(w <- 0 until bankNum){
        s1_hit0(w) := Mux(s1_bankmask(w),s1_hits(w),false.B)
        s1_hit0_oh(w) := Mux(s1_bankmask(w),s1_hit_ohs(w),0.U)
        s1_hit1(w) := Mux(s1_bankmask(w),false.B,s1_hits(w))
        s1_hit1_oh(w) := Mux(s1_bankmask(w),0.U,s1_hit_ohs(w))

    }
    // val s1_hit1 = 
    s1_meta.write_way(0) := Mux(s1_hit0.reduce(_||_),
        PriorityEncoder(s1_hit0_oh.map(_.asUInt).reduce(_|_)),
        alloc_way)
    s1_meta.write_way(1) := Mux(s1_hit1.reduce(_||_),
        PriorityEncoder(s1_hit1_oh.map(_.asUInt).reduce(_|_)),
        (alloc_way+1.U)%ubtbnWays.U)
    s1_meta.crossRow := s1_crossRow


    for (w <- 0 until bankNum) {
        io.resp.f1(w).predicted_pc := s1_resp(w)
        io.resp.f1(w).is_br        := s1_is_br(w)
        io.resp.f1(w).is_jal       := s1_is_jal(w)
        io.resp.f1(w).taken        := s1_taken(w)

        io.resp.f2(w) := RegNext(io.resp.f1(w))
        io.resp.f3(w) := RegNext(io.resp.f2(w))
    }
    io.f3_meta := RegNext(RegNext(s1_meta.asUInt))

    val s1_update_cfi_idx   = s1_update.bits.cfi_idx.bits
    val s1_update_meta      = s1_update.bits.meta.asTypeOf(new MicroBTBPredictMeta)
    val s1_update_write_way = s1_update_meta.write_way
    val s1_update_crossRow  = s1_update_meta.crossRow

    val s1_update_bankoh    = UIntToOH(bankoffset(s1_update.bits.pc))
    val s1_update_bankmask  = MaskUpper(s1_update_bankoh)>>bankoffset(s1_update.bits.pc)
    

    val max_offset_value = (~(0.U)((offsetSz-1).W)).asSInt
    val min_offset_value = Cat(1.B, (0.U)((offsetSz-1).W)).asSInt
    val new_offset_value = (s1_update.bits.target.asSInt -
        (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 2)).asSInt)

    val s1_update_wbtb_data     = Wire(new MicroBTBEntry)
    s1_update_wbtb_data.offset := new_offset_value
    val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
        Fill(bankNum, s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update))

    val s1_update_wmeta_mask = ((s1_update_wbtb_mask | s1_update.bits.br_mask) &
        Fill(bankNum, s1_update.valid && s1_update.bits.is_commit_update))

    // Write the BTB with the target
    when (s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.cfi_idx.valid && s1_update.bits.is_commit_update) {
        val btb_update_write_way = Mux((s1_update_bankmask&UIntToOH(s1_update_cfi_idx))=/=0.U,
                                s1_update_write_way(0),s1_update_write_way(1))
        val btb_update_idx = (s1_update_cfi_idx+bankoffset(s1_update.bits.pc))(log2Ceil(bankNum)-1,0)
        btb(btb_update_write_way)(btb_update_idx).offset := new_offset_value
    }

    //
    // Write the meta
    for (w <- 0 until bankNum) {
        when (s1_update_wmeta_mask(w)) {
            val was_taken = (s1_update_cfi_idx === w.U && s1_update.bits.cfi_idx.valid &&
                (s1_update.bits.cfi_taken || s1_update.bits.is_jal))
            val ptr   = (w.U+bankoffset(s1_update.bits.pc))%bankNum.U 

            val meta_update_write_way = Mux(s1_update_bankmask(w),
                                s1_update_write_way(0),s1_update_write_way(1))
            for(j <- 0 until bankNum){
                when(j.U===ptr){
                    meta(meta_update_write_way)(j).is_br := s1_update.bits.br_mask(w)
                    meta(meta_update_write_way)(j).tag   := s1_update_idx
                    meta(meta_update_write_way)(j).ctr   := Mux(!s1_update_meta.hits(w),
                                                        Mux(was_taken, 3.U, 0.U),
                                                        bimWrite(meta(meta_update_write_way)(w).ctr, was_taken)
                    )
                }
            }
        }
    }

}
