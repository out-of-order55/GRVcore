package grvcore
import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

case class MicroBTBParams(
    nWays: Int = 4,
    offsetSz: Int = 21
)

/*
只预测jal和br：jalr需要将offset扩展到32位
8way 差不多就能覆盖大部分jal和taken的br（小程序）
ubtb 使用寄存器堆搭建不是一个好的选择，面积太大：50000，除了存储以外的逻辑大概为1500左右
16way：50000
8：27000
4：12000
2：10000
允许跨行
改为不允许跨行：4way：9700
for 295 branches 
4way  has 149 hits:0.51
8way  has 244 hits:0.83
16way has 266 hits:0.90
 */
class MicroBTBBranchPredictor(implicit p: Parameters) extends BasePredictor()(p)
{
    val ubtbnWays     = ubtbParams.getOrElse(MicroBTBParams()).nWays
    val tagSz         = XLEN - fetchWidth
    val offsetSz      = ubtbParams.getOrElse(MicroBTBParams()).offsetSz
    val nWrBypassEntries = 2


    def bimWrite(v: UInt, taken: Bool): UInt = {
        val old_bim_sat_taken  = v === 3.U
        val old_bim_sat_ntaken = v === 0.U
        Mux(old_bim_sat_taken  &&  taken, 3.U,
        Mux(old_bim_sat_ntaken && !taken, 0.U,
        Mux(taken, v + 1.U, v - 1.U)))
    }
    val mems = Nil
    require(isPow2(ubtbnWays))

    class MicroBTBEntry extends Bundle {
        val offset= SInt(offsetSz.W)
        val is_br = Bool()
        val is_jal= Bool()
        val ctr   = UInt(2.W)
    }

    class MicroBTBMeta extends Bundle {
        val tag   = UInt(tagSz.W)
    }

    class MicroBTBPredictMeta extends Bundle {
        val hits        = Bool()
        val write_way   = UInt(log2Ceil(ubtbnWays).W)

    }

    val s1_meta = Wire(new MicroBTBPredictMeta)
    override val metaSz = s1_meta.asUInt.getWidth


    val meta     = RegInit((0.U).asTypeOf(Vec(ubtbnWays,  new MicroBTBMeta)))
    val btb      = Reg(Vec(ubtbnWays, Vec(bankNum, new MicroBTBEntry)))

    // val mems = Nil
    val s1_req_tag      = s1_idx

    val cnt = RegInit(0.U(log2Ceil(ubtbnWays).W))
    cnt := cnt + 1.U
    val s1_resp   = Wire(Vec(bankNum, Valid(UInt(XLEN.W))))
    val s1_taken  = Wire(Vec(bankNum, Bool()))
    val s1_is_br  = Wire(Vec(bankNum, Bool()))
    val s1_is_jal = Wire(Vec(bankNum, Bool()))

    //读mata
    val s1_hit_ohs = VecInit((0 until ubtbnWays) map { w =>
            meta(w).tag === s1_req_tag(tagSz-1,0)
    })

    val s1_hits     = s1_hit_ohs.reduce(_||_)
    val s1_hit_way  = WireInit(PriorityEncoder(s1_hit_ohs.asUInt))
    dontTouch(s1_mask)
    dontTouch(s1_hit_way)
    for (w <- 0 until bankNum) {
        val entry = btb(s1_hit_way)(w)
        s1_resp(w).valid := s1_valid && s1_hits&&s1_mask(w)
        s1_resp(w).bits  := (s1_pc.asSInt + (w << 2).S + entry.offset).asUInt
        s1_is_br(w)      := s1_resp(w).valid &&  entry.is_br
        s1_is_jal(w)     := s1_resp(w).valid && entry.is_jal
        s1_taken(w)      := entry.is_jal || entry.ctr(1)&&entry.is_br
        
    }
    s1_meta.hits     := s1_hits
//LFSR
    val alloc_way = LFSR(4)(log2Ceil(ubtbnWays)-1,0)
    dontTouch(alloc_way)
    dontTouch(s1_meta)
    dontTouch(s1_hit_ohs)
    dontTouch(s1_req_tag)
    s1_meta.write_way := Mux(s1_hits,s1_hit_way,alloc_way)




    for (w <- 0 until bankNum) {
        io.resp.f1(w).predicted_pc := s1_resp(w)
        io.resp.f1(w).br_type      := Mux(s1_is_br(w),1.U,0.U)
        io.resp.f1(w).is_jal       := s1_is_jal(w)
        io.resp.f1(w).taken        := s1_taken(w)

        io.resp.f2(w) := RegNext(io.resp.f1(w))
        io.resp.f3(w) := RegNext(io.resp.f2(w))
    }
    io.f3_meta := RegNext(RegNext(s1_meta.asUInt))

    val s1_update_cfi_idx   = s1_update.bits.cfi_idx.bits
    val s1_update_meta      = s1_update.bits.meta.asTypeOf(new MicroBTBPredictMeta)
    val s1_update_write_way = s1_update_meta.write_way

    dontTouch(s1_update_write_way)
    val new_offset_value = (s1_update.bits.target.asSInt -
        (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 2)).asSInt)
    val was_taken = (s1_update.bits.cfi_idx.valid &&
    (s1_update.bits.cfi_taken || s1_update.bits.is_jal))
    val s1_update_wbtb_data     = Wire(new MicroBTBEntry)
    val btb_update_write_way = s1_update_write_way
    val btb_update_idx = (s1_update_cfi_idx)(log2Ceil(bankNum)-1,0)
    dontTouch(s1_update_wbtb_data)
    s1_update_wbtb_data.offset := new_offset_value
    s1_update_wbtb_data.ctr :=  Mux(!s1_update_meta.hits,
                                            Mux(was_taken, 3.U, 0.U),
                                            bimWrite(btb(btb_update_write_way)(btb_update_idx).ctr, was_taken))
    s1_update_wbtb_data.is_br := s1_update.bits.cfi_is_br
    s1_update_wbtb_data.is_jal:= s1_update.bits.is_jal
    val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
        Fill(bankNum, (s1_update.bits.cfi_is_br||s1_update.bits.is_jal)&&s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update))

    val s1_update_wmeta_mask = ((s1_update_wbtb_mask) &
        Fill(bankNum, s1_update.valid && s1_update.bits.is_commit_update))


    // Write the BTB with the target
    when (s1_update_wbtb_mask.orR) {
        btb(btb_update_write_way)(btb_update_idx) := s1_update_wbtb_data
    }

    //
    // Write the meta

    when (s1_update_wmeta_mask.orR) {
        meta(s1_update_write_way).tag   := s1_update_idx
    }

/////////////////////////////////////////////
    val hit_cnt = RegInit(0.U(10.W))
    val taken_cnt = RegInit(0.U(10.W))
    val total_cnt = RegInit(0.U(10.W))
    dontTouch(hit_cnt)
    dontTouch(taken_cnt)
    dontTouch(total_cnt)
    when(s1_valid&&s1_hits){
        hit_cnt := hit_cnt +1.U
    }
    when(s1_update_valid&&(s1_update_wbtb_data.is_br||s1_update_wbtb_data.is_jal)){
        total_cnt := total_cnt +1.U
    }
    when(s1_update_valid&&(s1_update_wbtb_data.is_br||s1_update_wbtb_data.is_jal)&&(!s1_update.bits.cfi_taken)){
        taken_cnt := taken_cnt+1.U
    }

}
