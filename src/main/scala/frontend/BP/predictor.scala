package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class BranchPrediction(implicit p: Parameters) extends GRVBundle
{
    val taken           = Bool()
    val is_jal          = Bool()
    val is_jalr         = Bool()

    val br_type         = UInt(2.W)

    def is_br           = br_type===1.U
    def is_call         = br_type===2.U
    def is_ret          = br_type===3.U
    val predicted_pc    = Valid(UInt(XLEN.W))
}

class BPReq(implicit p: Parameters) extends GRVBundle with HasFrontendParameters
{
    val pc      = UInt(XLEN.W)
    val mask  = Input(UInt(fetchWidth.W))
    val ghist   = UInt(globalHistoryLength.W)
}
class BranchPredictionBundle(implicit p: Parameters) extends GRVBundle()(p)
with HasFrontendParameters
{
    val pc = UInt(XLEN.W)
    val preds = Vec(fetchWidth, new BranchPrediction)
    val meta = (UInt(bpdMaxMetaLength.W))

}
class BPResp(implicit p: Parameters) extends GRVBundle()(p) with HasFrontendParameters
{
    val f1 = Vec(bankNum,new BranchPrediction())
    val f2 = Vec(bankNum,new BranchPrediction())
    val f3 = Vec(bankNum,new BranchPrediction())
}
class BranchPredictionUpdate(implicit p: Parameters) extends GRVBundle with HasFrontendParameters
{
    val pc                   = UInt(XLEN.W)
    val meta                 = UInt(bpdMaxMetaLength.W)
    val is_mispredict_update = Bool()
    val ghist                = UInt(globalHistoryLength.W)
    val is_commit_update     = Bool()

    //找出那条是分支指令（目前仅支持1条分支预测）
    val cfi_idx   = Valid(UInt(log2Ceil(bankNum).W))
    val cfi_taken = Bool()
    val cfi_type  = UInt(2.W)//call ret 信息
    def cfi_is_br   = cfi_type===1.U
    def cfi_is_call = cfi_type===2.U
    def cfi_is_ret  = cfi_type===3.U
    //是否推测更新失败，主要针对ghist

    val br_mask      = UInt(bankNum.W)
    val is_jal       = Bool()
    val is_jalr      = Bool()
    val target        = UInt(XLEN.W)
}

/* 
call和ret在s2预测，其他阶段都是s1
这里用的是信号重载，继承这个抽象类的都可以修改信号
但注意要有对应关系，具体的：
每个阶段得有方向预测和地址预测，方向预测只给出taken信息，地址预测给出分支类型和地址（ubtb除外），否则信号会被覆盖

对于目前预测器，只有ras会推测更新，其他全部在commmit阶段更新，所以暂时用不到is_mispredict_update，
之后上了ghist可能会用到

 */
abstract class BasePredictor(implicit p: Parameters) extends GRVModule with HasFrontendParameters
{
    val metaSz = 0

    val io = IO(new Bundle {
        val f0_valid = Input(Bool())
        val f0_pc    = Input(UInt(XLEN.W))
        val f0_mask  = Input(UInt(fetchWidth.W))
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

    val s0_mask      = io.f0_mask
    dontTouch(s0_mask)
    val s1_mask      = RegNext(s0_mask)
    val s2_mask      = RegNext(s1_mask)
    val s3_mask      = RegNext(s2_mask)

    val s0_valid = io.f0_valid
    val s1_valid = RegNext(s0_valid)
    val s2_valid = RegNext(s1_valid)
    val s3_valid = RegNext(s2_valid)

    val s0_pc = io.f0_pc
    val s1_pc = RegNext(s0_pc)
    val s2_pc = RegNext(s1_pc)

    val s0_update     = io.update
    val s0_update_idx = fetchIdx(io.update.bits.pc)
    val s0_update_valid = io.update.valid

    val s1_update     = RegNext(s0_update)
    val s1_update_idx = RegNext(s0_update_idx)
    val s1_update_valid = RegNext(s0_update_valid)

}

class BranchPredictor(implicit p: Parameters) extends GRVModule()(p)
with HasFrontendParameters
{
    val io = IO(new Bundle {
        // Requests and responses
        val f0_req = Input(Valid(new BPReq))
        val resp = Output(new Bundle {
            val f1 = new BranchPredictionBundle
            val f2 = new BranchPredictionBundle
            val f3 = new BranchPredictionBundle
        })

        val f3_fire = Input(Bool())

        // Update
        val update = Input(Valid(new BranchPredictionUpdate))
    })

    var total_memsize = 0

    val branch_predictors = Module(new Composer)

    branch_predictors.io.f0_valid := io.f0_req.valid
    branch_predictors.io.f0_pc    := io.f0_req.bits.pc
    branch_predictors.io.f0_mask  := io.f0_req.bits.mask
    branch_predictors.io.f1_ghist := RegNext(io.f0_req.bits.ghist)
    branch_predictors.io.resp_in  := (0.U).asTypeOf(new BPResp)

    // io.resp.f3.meta := DontCare
    io.resp.f1.preds    := branch_predictors.io.resp.f1
    io.resp.f2.preds    := branch_predictors.io.resp.f2
    io.resp.f3.preds    := branch_predictors.io.resp.f3
    io.resp.f3.meta     := branch_predictors.io.f3_meta
    branch_predictors.io.f3_fire := io.f3_fire

    io.resp.f1.pc := RegNext(io.f0_req.bits.pc)
    io.resp.f2.pc := RegNext(io.resp.f1.pc)
    io.resp.f3.pc := RegNext(io.resp.f2.pc)

    io.resp.f1.meta := DontCare
    io.resp.f2.meta := DontCare

    branch_predictors.io.update.bits.meta             := io.update.bits.meta

    branch_predictors.io.update.bits.cfi_idx.bits    := io.update.bits.cfi_idx.bits
    branch_predictors.io.update.bits.cfi_taken       := io.update.bits.cfi_taken
    branch_predictors.io.update.bits.cfi_type        := io.update.bits.cfi_type

    branch_predictors.io.update.bits.is_jal          := io.update.bits.is_jal
    branch_predictors.io.update.bits.is_jalr         := io.update.bits.is_jalr

    branch_predictors.io.update.bits.target                  := io.update.bits.target
    branch_predictors.io.update.bits.is_mispredict_update    := io.update.bits.is_mispredict_update
    branch_predictors.io.update.bits.is_commit_update        := io.update.bits.is_commit_update

    branch_predictors.io.update.valid                 := io.update.valid
    branch_predictors.io.update.bits.pc               := io.update.bits.pc
    branch_predictors.io.update.bits.br_mask          := io.update.bits.br_mask

    branch_predictors.io.update.bits.cfi_idx.valid    := io.update.bits.cfi_idx.valid
    branch_predictors.io.update.bits.ghist            := io.update.bits.ghist




    when (io.update.valid) {
        when (io.update.bits.cfi_is_br && io.update.bits.cfi_idx.valid) {
        // assert(io.update.bits.br_mask(io.update.bits.cfi_idx.bits))
        }
    }
}