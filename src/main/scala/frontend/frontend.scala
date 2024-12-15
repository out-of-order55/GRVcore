package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.Annotated.resetVector


trait HasFrontendParameters extends HasICacheParameters{
    override val bankWidth: Int = fetchWidth
    def bankoffset(addr:UInt) = addr(offsetWidth-1,offsetWidth-log2Ceil((XLEN/8)))
    def bankAlign(addr:UInt) =(addr>>bankWidth)<<bankWidth
}
class FetchBundle(implicit p: Parameters) extends GRVBundle with HasFrontendParameters
{
//指令信息
    val pc            = UInt(XLEN.W)
    val mask          = UInt(fetchWidth.W) 
    val insts         = Vec(fetchWidth, Bits(32.W))


//分支信息
    val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
    val cfi_type      = UInt(2.W)
    val cfi_taken     = Bool()
    val is_jal        = Bool()
    val is_jalr       = Bool()
    val br_mask       = UInt(fetchWidth.W)
    val ghist         = UInt(globalHistoryLength.W)
    val bpd_meta      = UInt(bpdMaxMetaLength.W)

    val ftq_idx       = UInt(log2Ceil(ftqentries).W)
    val bpSrc    = UInt(2.W)
}

class FrontendIO(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val fetchpacket       = Flipped(new DecoupledIO(new IQueueResp))
    val get_pc            = Flipped(new GetPCFromFtqIO())


    //后端重定向

    val brupdate          = Output(new BrUpdateInfo)
    val redirect_flush    = Output(Bool())//冲刷
    val redirect_val      = Output(Bool())//重定向
    val redirect_pc       = Output(UInt())

    //commit 信号
    val commit            = Output(Bool())

    val flush_icache = Output(Bool())
}
class FrontBundle(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val cpu = Flipped(new FrontBundle)
}
class FrontendInfo(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val pc   = UInt(XLEN.W)
    val inst = Vec((bankNum),UInt(XLEN.W)) 
    val mask = UInt(fetchWidth.W)
    val ghist= UInt(globalHistoryLength.W)
}


class Frontend(implicit p: Parameters) extends GRVModule with HasFrontendParameters{
    val io = IO(new FrontBundle)


    def nextPC(addr:UInt) = {
        bankAlign(addr) + blockBytes.U
    }
    val start        = RegInit(false.B)
    start := true.B
    val start1 = RegNext(start)

    val s0_vpc       = WireInit(0.U(XLEN.W))
    val s0_valid     = WireInit(false.B)
    val s0_mask      = WireInit(MaskUpper(UIntToOH(bankoffset(s0_vpc),bankNum)))
    
    




    



    val bp     = Module(new BranchPredictor)
    val icache = Module(new ICache)
    val ftq    = Module(new FetchTargetQueue)
    val ibuf   = Module(new IQueue)
    when(start&&(!start1)){
        s0_vpc := reset_vector
        s0_valid := true.B
    }


    bp.io.f0_req.valid      := s0_valid
    bp.io.f0_req.bits.pc    := s0_vpc
    bp.io.f0_req.bits.mask  := s0_mask
    bp.io.f0_req.bits.ghist := 0.U

    icache.io.req.valid     := s0_valid
    icache.io.req.bits.raddr:= bankAlign(s0_vpc)

////////////////////////s1阶段//////////////////
    val s1_mask      = RegNext(s0_mask)
    val s1_vpc       = RegNext(s0_vpc)
    val s1_valid     = RegNext(s0_valid, false.B)
    val s1_clear     = WireInit(false.B)

    icache.io.s1_kill:= s1_clear
/* 
此时ubtb会出结果，需要重定向
 */
//pred info
    val s1_bp_resp =  WireInit(bp.io.resp.f1)
    val s1_taken   =  s1_bp_resp.preds.map{resp=>
        resp.taken&resp.predicted_pc.valid
    }
    val s1_taken_idx = PriorityEncoder(s1_taken)

    val s1_pred_pc   = Mux(s1_taken.reduce(_||_),s1_bp_resp.preds(s1_taken_idx).predicted_pc.bits,
                            nextPC(s1_vpc))
    val s1_br_type   = s1_bp_resp.preds(s1_taken_idx).br_type
    val s1_is_br     = s1_br_type===BR
    val s1_is_jal    = s1_bp_resp.preds(s1_taken_idx).is_jal
    val s1_is_jalr   = s1_bp_resp.preds(s1_taken_idx).is_jalr

    when(s1_valid){
        s0_vpc  := s1_pred_pc
        s0_valid:= true.B
    }
///////////////////////s2/////////////////////
    val s2_mask      = RegNext(s1_mask)
    val s2_vpc       = RegNext(s1_vpc)
    val s2_valid     = RegNext(s1_valid&(!s1_clear), false.B)

/* 
本阶段ICache读出结果，并且BTB，BIM和RAS会出结果
    

 */
    val ras = Module(new GRVRAS)
    val s2_notReady  = WireInit((!icache.io.resp.valid)||
        icache.io.resp.valid&&(!s3_ready))

    icache.io.s2_kill:= s2_clear

    val s2_inst     = icache.io.resp.bits


    val s2_bp_resp =  WireInit(bp.io.resp.f2)
    val s2_taken   =  s2_bp_resp.preds.map{resp=>
        resp.taken&resp.predicted_pc.valid
    }
    val s2_taken_idx = PriorityEncoder(s2_taken)

    // val s2_pred_pc   = 
    val s2_br_type   = s2_bp_resp.preds(s2_taken_idx).br_type
    val s2_is_br     = s2_br_type===BR
    val s2_is_jal    = s2_bp_resp.preds(s2_taken_idx).is_jal
    val s2_is_jalr   = s2_bp_resp.preds(s2_taken_idx).is_jalr
    val s2_clear     = WireInit(false.B)

    val s3_ready     = Bool()
    ras.io.rasResp.br_type.valid := s2_valid&(!s2_notReady)  
    ras.io.rasResp.br_type.bits  := s2_br_type
    ras.io.rasResp.write_addr    := bankAlign(s2_vpc) + (s2_taken_idx<<2) + 4.U  
    val RAS_addr     = ras.io.rasResp.read_addr   
    val s2_pred_pc   =  Mux(s2_taken.reduce(_||_),
                        Mux(s2_br_type===RET,RAS_addr,s2_bp_resp.preds(s2_taken_idx).predicted_pc.bits)
                        ,nextPC(s2_vpc))
    
    when(s2_valid&&s2_notReady){
        s1_clear := true.B
        s0_vpc   := s2_vpc
        s0_valid := true.B
    }.elsewhen(s2_valid&&icache.io.resp.valid&&(s3_ready)){
        when(s1_valid&&s1_vpc=/=s2_pred_pc||(!s1_valid)){
            s1_clear := true.B
            s0_vpc   := s2_pred_pc
            s0_valid := true.B
        }
    }
////////////////////////S3//////////////////////
    val s3_clear = WireInit(false.B)
    val s3 = withReset(reset.asBool || s3_clear) {
        Module(new Queue(new FrontendInfo, 1, pipe=true, flow=false)) }
    s3.io.enq.valid := s2_valid && (!s2_notReady)&&(!s2_clear)
    s3_ready        := s3.io.enq.ready
    s3.io.enq.bits.pc  := s2_vpc
    s3.io.enq.bits.inst:= s2_inst
    s3.io.enq.bits.mask:= s2_mask 
    s3.io.enq.bits.ghist:= 0.U//not use now



}