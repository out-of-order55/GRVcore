package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class BranchPrediction(implicit p: Parameters) extends GRVBundle
{
    val taken           = Bool()
    val is_br           = Bool()
    val is_jal          = Bool()
    val predicted_pc    = Valid(UInt(XLEN.W))
}

class BPReq(implicit p: Parameters) extends GRVBundle
{
    val pc      = UInt(XLEN.W)
    val ghist   = UInt(globalHistoryLength.W)
}

class BPResp(implicit p: Parameters) extends GRVBundle
{
    val f1 = Vec(fetchWidth,new BranchPrediction())
    val f2 = Vec(fetchWidth,new BranchPrediction())
    val f3 = Vec(fetchWidth,new BranchPrediction())
}
class BranchPredictionUpdate(implicit p: Parameters) extends GRVBundle
{
    val pc      = UInt(XLEN.W)
    val meta    = UInt(bpdMaxMetaLength.W)
    val is_mispredict_update = Bool()

    def is_commit_update = !(is_mispredict_update)

    //找出那条是分支指令（目前仅支持1条分支预测）
    val cfi_idx = Valid(UInt(log2Ceil(fetchWidth).W))
    //是否推测更新失败，主要针对ghist
    val br_taken     = Bool()
    val br_mask      = Vec(bankWidth,Bool())
    val is_br        = Bool()

    val target        = UInt(XLEN.W)
}
abstract class BasePredictor(implicit p: Parameters) extends GRVModule
{
    val metaSz = 0

    val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(XLEN.W))

    val f1_ghist = Input(UInt(globalHistoryLength.W))


    val resp_in = Input(new BPResp)
    val resp = Output(new BPResp)

    // 保存从分支预测器旧的状态，比如bim值，BTB的tag信息，GSAHRE读出的ctr值，
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BranchPredictionUpdate))
    })
    io.resp := io.resp_in

    io.f3_meta := 0.U

    val s0_idx       = fetchIdx(io.f0_pc)
    val s1_idx       = RegNext(s0_idx)
    val s2_idx       = RegNext(s1_idx)
    val s3_idx       = RegNext(s2_idx)

    val s0_valid = io.f0_valid
    val s1_valid = RegNext(s0_valid)
    val s2_valid = RegNext(s1_valid)
    val s3_valid = RegNext(s2_valid)

    val s0_pc = io.f0_pc
    val s1_pc = RegNext(s0_pc)

    val s0_update     = io.update
    val s0_update_idx = fetchIdx(io.update.bits.pc)
    val s0_update_valid = io.update.valid

    val s1_update     = RegNext(s0_update)
    val s1_update_idx = RegNext(s0_update_idx)
    val s1_update_valid = RegNext(s0_update_valid)

}