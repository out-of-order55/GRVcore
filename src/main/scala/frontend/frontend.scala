package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// import freechips.rocketchip.util.Annotated.resetVector
import freechips.rocketchip.util.DontTouch


trait HasFrontendParameters extends HasICacheParameters{
    override val bankWidth: Int = fetchWidth
    val HasBP:Boolean = false
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

    val brupdate          = Valid(new BrUpdateInfo)
    val redirect_flush    = Output(Bool())//冲刷
    val redirect_val      = Output(Bool())//重定向
    val redirect_pc       = Output(UInt(XLEN.W))

    //commit 信号
    val commit            = Output(new CommitMsg)

    val flush_icache = Output(Bool())
}
class FrontBundle(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val cpu          = Flipped(new FrontendIO)
    // val icache_master= AXI4Bundle(CPUAXI4BundleParameters())
}
class FrontendInfo(implicit p: Parameters) extends GRVBundle with HasFrontendParameters{
    val pc   = UInt(XLEN.W)
    val inst = Vec((bankNum),UInt(XLEN.W)) 
    val mask = UInt(fetchWidth.W)
    val ghist= UInt(globalHistoryLength.W)
}

/* 
总体面积：7300（ibuf）+6400（ftq）+3800（icache）+1300（bim）+BTB（2200）+ubtb（9700）+RAS（5800）+frontend（2000）
=38500
均没有计算SRAM，其中面积大的都是寄存器堆搭建的RAM或者FIFO
 */
class FrontEnd(implicit p: Parameters) extends LazyModule{
    lazy val module = new FrontendImp(this)
    val icache = LazyModule(new ICacheWrapper)
    val masterNode = icache.masterNode

    
}
class FrontendImp(val outer: FrontEnd)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasFrontendParameters with GRVOpConstants with DontTouch{
    val io = IO(new FrontBundle)

    val (f_master, _) = outer.masterNode.out(0)
    dontTouch(f_master)
    def nextPC(addr:UInt) = {
        bankAlign(addr) + blockBytes.U
    }
    val start        = RegInit(false.B)
    when((!reset.asBool)){
        start := true.B
    }
    val start1 = RegNext(start)

    val s0_vpc       = WireInit(0.U(XLEN.W))
    val s0_valid     = WireInit(false.B)
    val s0_mask      = WireInit(MaskUpper(UIntToOH(bankoffset(s0_vpc),bankNum)))


    val bp     = Module(new BranchPredictor)
    val icache = outer.icache.module
    icache.suggestName("ICache")
    val ftq    = Module(new FetchTargetQueue)
    val ibuf   = Module(new IQueue)
    when(start&&(!start1)){
        s0_vpc := reset_vector.U
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
    val s1_taken   =  (0 until fetchWidth).map{i=>
        s1_bp_resp.preds(i).taken&s1_bp_resp.preds(i).predicted_pc.valid&s1_mask(i)===1.U&&HasBP.B
    }

    val s1_taken_idx = PriorityEncoder(s1_taken)

    val s1_pred_pc   = Mux(s1_taken.reduce(_||_),s1_bp_resp.preds(s1_taken_idx).predicted_pc.bits,
                            nextPC(s1_vpc))
    val s1_br_type   = s1_bp_resp.preds(s1_taken_idx).br_type
    val s1_is_br     = s1_br_type===BR.U
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
    val s3_ready     = Wire(Bool())
/* 
本阶段ICache读出结果，并且BTB，BIM和RAS会出结果
    

 */
    val ras = Module(new GRVRAS)
    val s2_notReady  = WireInit((!icache.io.resp.valid)||
        icache.io.resp.valid&&(!s3_ready))

    

    val s2_inst     = icache.io.resp.bits


    val s2_bp_resp =  WireInit(bp.io.resp.f2)

    val s2_taken   =  (0 until fetchWidth).map{i=>
        s2_bp_resp.preds(i).taken&s2_bp_resp.preds(i).predicted_pc.valid&s2_mask(i)===1.U&&HasBP.B
    }

    val s2_taken_idx = PriorityEncoder(s2_taken)

    // val s2_pred_pc   = 
    val s2_br_type   = s2_bp_resp.preds(s2_taken_idx).br_type
    val s2_is_br     = s2_br_type===BR.U
    val s2_is_jal    = s2_bp_resp.preds(s2_taken_idx).is_jal
    val s2_is_jalr   = s2_bp_resp.preds(s2_taken_idx).is_jalr
    val s2_clear     = WireInit(false.B)
    
    val RAS_addr     = ras.io.rasResp.read_addr   
    val s2_pred_pc   =  Mux(s2_taken.reduce(_||_),
                        Mux(s2_br_type===RET.U,RAS_addr,s2_bp_resp.preds(s2_taken_idx).predicted_pc.bits)
                        ,nextPC(s2_vpc))
    
    icache.io.s2_kill:= s2_clear
    ras.io.rasResp.br_type.valid := s2_valid&(!s2_notReady)  
    ras.io.rasResp.br_type.bits  := s2_br_type
    ras.io.rasResp.write_addr    := bankAlign(s2_vpc) + (s2_taken_idx<<2) + 4.U  

    
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
    val s4_ready = Wire(Bool())
    s3.io.enq.valid := s2_valid && (!s2_notReady)&&(!s2_clear)
    s3_ready        := s3.io.enq.ready
    s3.io.enq.bits.pc  := s2_vpc
    s3.io.enq.bits.inst:= s2_inst.data
    s3.io.enq.bits.mask:= s2_mask 
    s3.io.enq.bits.ghist:= 0.U//not use now
    s3.io.deq.ready     := s4_ready

    val s3_resp         = s3.io.deq.bits
//s3目前无分支预测，仅仅传送分支预测信息
    val s3_bp_resp =  WireInit(bp.io.resp.f3)
    val s3_taken   =  s3_bp_resp.preds.map{resp=>
        resp.taken&resp.predicted_pc.valid
    }
    val s3_taken_idx = PriorityEncoder(s3_taken)
//////////////////////s4///////////////////////
/* 
写入ftq和ibuf，前段完成
 */
    bp.io.f3_fire := s3.io.deq.fire
    ftq.io.enq.valid := s3.io.deq.valid&(!s3_clear)&ibuf.io.enq.ready
    ibuf.io.enq.valid:= s3.io.deq.valid&(!s3_clear)&ftq.io.enq.ready

    s4_ready := ibuf.io.enq.ready&ftq.io.enq.ready&((!s3_clear))

    val s3_fetch_bundle  = Wire(new FetchBundle)
    s3_fetch_bundle.pc          := s3_resp.pc
    s3_fetch_bundle.mask        := s3_resp.mask
    s3_fetch_bundle.insts       := s3_resp.inst
    s3_fetch_bundle.ghist       := s3_resp.ghist
    s3_fetch_bundle.cfi_idx.valid:=s3_bp_resp.preds(s3_taken_idx).taken&&s3.io.deq.valid &&s3_bp_resp.preds(s3_taken_idx).predicted_pc.valid&&HasBP.B
    s3_fetch_bundle.cfi_idx.bits:= s3_taken_idx
    s3_fetch_bundle.cfi_type    := s3_bp_resp.preds(s3_taken_idx).br_type
    s3_fetch_bundle.cfi_taken   := s3_bp_resp.preds(s3_taken_idx).taken&&s3_bp_resp.preds(s3_taken_idx).predicted_pc.valid&&HasBP.B
    s3_fetch_bundle.is_jal      := s3_bp_resp.preds(s3_taken_idx).is_jal
    s3_fetch_bundle.is_jalr     := s3_bp_resp.preds(s3_taken_idx).is_jalr
    s3_fetch_bundle.br_mask     := UIntToOH(s3_taken_idx,fetchWidth)
    s3_fetch_bundle.bpd_meta    := s3_bp_resp.meta
    s3_fetch_bundle.ftq_idx     := ftq.io.enq_idx
    s3_fetch_bundle.bpSrc       := 0.U

    ftq.io.enq.bits := s3_fetch_bundle
    ibuf.io.enq.bits:= s3_fetch_bundle

    ibuf.io.deq<>io.cpu.fetchpacket
    ras.io.rasUpdate <> ftq.io.ras_update
    ftq.io.bpdupdate <> bp.io.update
    ftq.io.brupdate  <> io.cpu.brupdate
    ftq.io.get_ftq_pc<> io.cpu.get_pc
    ftq.io.deq.valid := io.cpu.commit.valid.reduce(_||_)

    val commit_idx = PriorityEncoder(io.cpu.commit.valid)
    dontTouch(commit_idx)
    ftq.io.deq.bits  := io.cpu.commit.commit_uops(commit_idx).ftq_idx
    val cpu_flush       = io.cpu.redirect_flush
    val cpu_redirect    = io.cpu.redirect_val  
    val cpu_redirect_pc = io.cpu.redirect_pc   
    dontTouch(cpu_flush)
    dontTouch(cpu_redirect_pc)
    icache.io.invalidate:= io.cpu.flush_icache 
    ibuf.io.clear := false.B
    s3_clear      := false.B
    ftq.io.redirect:= false.B
    when(cpu_flush){
        ibuf.io.clear   := true.B
        s1_clear        := true.B
        s2_clear        := true.B
        s3_clear        := true.B
        s0_valid        := cpu_redirect
        s0_vpc          := cpu_redirect_pc
        ftq.io.redirect := cpu_redirect
    }

    override def toString: String =
    (GRVString("====Overall Frontend Params====") + "\n"
    + icache.toString+bp.toString)
    val str = toString()
    // print(str)
}