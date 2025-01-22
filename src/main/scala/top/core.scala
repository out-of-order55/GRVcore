package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Processor Core
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// GRVCore has the following (conceptual) stages:
//   if0 - Instruction Fetch 0 (next-pc select)
//   if1 - Instruction Fetch 1 (I$ access)
//   if2 - Instruction Fetch 2 (instruction return)
//   if3 - Instruction Fetch 3 (enqueue to fetch buffer)
//   dec - Decode
//   ren - Rename
//   dis - Dispatch
//   iss - Issue
//   rrd - Register Read
//   exe - Execute
//   wb  - Writeback
//   com - Commit
/* 
目前的问题是：
1.每个阶段的流水线必须得下一级全部接受才可以流动，之后可以将其分开，也就是分多个queue
2.需要在执行阶段前加入exdecode
3.需要为每个ld_uop分配最近的stq_idx
 */
class GRVCore()(implicit p: Parameters) extends GRVModule
{
    val io = IO(new Bundle {
        val ifu = (new FrontendIO)
        val lsu = Flipped(new LSUBundle)
    })
    val memWidth = ldIssueParam.issueWidth
    val decode_units = Seq.fill(coreWidth)(Module(new DecodeUnit))
    val rename_stage = Module(new RenameStage)
    val alujmp_unit      = Module(new ALUExuUnit(true,true,false,false))
    // val alu_unit         = Module(new ALUExuUnit(true,false,false,false))
    val muldiv_unit      = Module(new ALUExuUnit(false,false,true,true))


    val numIntIssueWakeupPorts = alujmp_unit.numIrfWritePorts + muldiv_unit.numIrfWritePorts + ldIssueParam.issueWidth// 4 for alu 2 for lsu

    val dispatcher       = Module(new ComplexDispatcher)
    //int 的可以发射到两个alu
    val ld_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,ldIssueParam.issueWidth,ldIssueParam.numEntries,IQT_LD.litValue.toInt,ldIssueParam.dispatchWidth))
    ld_iss_unit.suggestName("ld_issue_unit")
    val st_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,stIssueParam.issueWidth,stIssueParam.numEntries,IQT_ST.litValue.toInt,stIssueParam.dispatchWidth))
    st_iss_unit.suggestName("st_issue_unit")
    val int_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,intIssueParam.issueWidth,intIssueParam.numEntries,IQT_INT.litValue.toInt,intIssueParam.dispatchWidth))
    int_iss_unit.suggestName("int_issue_unit")

    val issue_units      = Seq(int_iss_unit,ld_iss_unit,st_iss_unit)
    val exe_units        = Seq(alujmp_unit,muldiv_unit)


    val numIrfReadPorts  =  2*(intIssueParam.issueWidth+ldIssueParam.issueWidth + stIssueParam.issueWidth)
    val numIrfWritePorts = numIntIssueWakeupPorts

    val regfiles         = Module(new RegFile(numIrfReadPorts,numIrfWritePorts,Seq.fill(numIntIssueWakeupPorts) {true}))
    val rob              = Module(new ROB(numIntIssueWakeupPorts))

///////////////////////////////////FRONTEND///////////////////////////
    val dec2rename = withReset(reset.asBool ) {
        Module(new Queue(Vec(coreWidth,new MicroOp), 1, pipe=true, flow=false)) }
    io.ifu.commit                     := rob.io.commit.bits
    io.ifu.fetchpacket.ready          := dec2rename.io.enq.ready
    // io.ifu.redirect_flush             := 
    // io.ifu.redirect_val               := 
    // io.ifu.get_pc                     := 
    // io.ifu.flush_icache               := 
    // io.ifu.brupdate.cfi_mispredicted  := 
    //
///////////////////////////////////DECODER///////////////////////////
    for(i <- 0 until coreWidth){
        io.ifu.fetchpacket
    }
    (decode_units zip io.ifu.fetchpacket.bits.uops).foreach{case(a,b)=>
        a.io.enq.uop := b.bits
        dontTouch(a.io.deq.uop)
    }

    val dec_valid = WireInit(io.ifu.fetchpacket.valid)
    dec2rename.suggestName("dec2rename_pipe")
    dec2rename.io.enq.bits  := decode_units.map(_.io.deq.uop)
    dec2rename.io.enq.valid := dec_valid

///////////////////////////////////RENAME/////////////////////////////
    val rename2dis = withReset(reset.asBool ) {
        Module(new Queue(Vec(coreWidth,new MicroOp), 1, pipe=true, flow=false)) }
    rename2dis.suggestName("dec2rename_pipe")
    rename_stage.io.dec_uops  <> dec2rename.io.deq 
    rename_stage.io.commit    := rob.io.commit.bits
    rename_stage.io.redirect  := false.B
    rename2dis.io.enq<>rename_stage.io.dis_uops  
    rename2dis.io.deq.ready   := dispatcher.io.ren_uops.map(_.ready).reduce(_&&_)
///////////////////////////////////DISPATCHER//////////////////////////

    for(i <- 0 until coreWidth){
        dispatcher.io.ren_uops(i).bits := rename2dis.io.deq.bits(i)
        dispatcher.io.ren_uops(i).valid:= rename2dis.io.deq.valid
    }
    //dispatch 按照type分发，所以dispatch一样
    rob.io.enq.uops <> dispatcher.io.dis_uops(0)

//////////////////////////////////INT/////////////////////////////////
    val int_iss_idx = issueParams.indexWhere(_.iqType == IQT_INT.litValue) 
    val alu_uop = WireInit(dispatcher.io.dis_uops(int_iss_idx))
    val rob_idx= WireInit(rob.io.enq_idxs)
    for(j <- 0 until intIssueParam.issueWidth){
        alu_uop(j).bits.rob_idx := Mux(rob_idx(j).valid,rob_idx(j).bits,0.U)
    }
    int_iss_unit.io.dis_uops<> alu_uop
//////////////////////////////////LD/////////////////////////////////
    val ld_iss_idx = issueParams.indexWhere(_.iqType == IQT_LD.litValue) 
    val ld_uop = WireInit(dispatcher.io.dis_uops(ld_iss_idx))
    val ldq_idx= WireInit(io.lsu.dis(1).enq_idx)


    for(j <- 0 until ldIssueParam.dispatchWidth){
        ld_uop(j).bits.ldq_idx := Mux(ldq_idx(j).valid,ldq_idx(j).bits,0.U)
        ld_uop(j).bits.rob_idx := Mux(rob_idx(j).valid,rob_idx(j).bits,0.U)
    }
    io.lsu.dis(1).enq <> dispatcher.io.dis_uops(ld_iss_idx)
    ld_iss_unit.io.dis_uops<>ld_uop
//////////////////////////////////ST/////////////////////////////////
    val st_iss_idx = issueParams.indexWhere(_.iqType == IQT_ST.litValue) 
    val st_uop = WireInit(dispatcher.io.dis_uops(st_iss_idx))
    val stq_idx= WireInit(io.lsu.dis(0).enq_idx)

    for(j <- 0 until stIssueParam.dispatchWidth){
        st_uop(j).bits.ldq_idx := Mux(stq_idx(j).valid,stq_idx(j).bits,0.U)
        st_uop(j).bits.rob_idx := Mux(rob_idx(j).valid,rob_idx(j).bits,0.U)
        
    }
    st_iss_unit.io.dis_uops<>st_uop
    io.lsu.dis(0).enq <> dispatcher.io.dis_uops(st_iss_idx)

///////////////////////////////////ISSUE PIPE///////////////////////////////
    val int_iss2rrd = withReset(reset.asBool ) {
        Module(new Queue(Vec(intIssueParam.issueWidth,new MicroOp), 1, pipe=true, flow=false)) }

    int_iss2rrd.suggestName("int_iss2rrd_pipe")
    int_iss2rrd.io.enq <> int_iss_unit.io.issue_uops
    val ld_iss2rrd = withReset(reset.asBool ) {
        Module(new Queue(Vec(ldIssueParam.issueWidth,new MicroOp), 1, pipe=true, flow=false)) }

    ld_iss2rrd.suggestName("ld_iss2rrd_pipe")
    ld_iss2rrd.io.enq <> ld_iss_unit.io.issue_uops
    val st_iss2rrd = withReset(reset.asBool ) {
        Module(new Queue(Vec(stIssueParam.issueWidth,new MicroOp), 1, pipe=true, flow=false)) }
    st_iss2rrd.suggestName("st_iss2rrd_pipe")
    st_iss2rrd.io.enq <> st_iss_unit.io.issue_uops
///////////////////////////////////RRD/////////////////////////////////
    
    val int_op = WireInit(VecInit.fill(intIssueParam.issueWidth)(0.U.asTypeOf(new ExuReq())))
    for(i <- 0 until intIssueParam.issueWidth){
        val int_uop = int_iss2rrd.io.deq.bits(i)
        val rs1_addr= int_uop.prs1
        val rs2_addr= int_uop.prs2
        regfiles.io.readports(2*i).addr := rs1_addr
        regfiles.io.readports(2*i+1).addr := rs2_addr
        int_op(i).uop := int_uop
        int_op(i).kill:= false.B
        int_op(i).rs1_data := regfiles.io.readports(2*i).data
        int_op(i).rs2_data := regfiles.io.readports(2*i+1).data
    }
    val ld_op = WireInit(VecInit.fill(ldIssueParam.issueWidth)(0.U.asTypeOf(new LSUReq())))
    val ld_base_idx = intIssueParam.issueWidth
    for(i <- ld_base_idx until ld_base_idx+ldIssueParam.issueWidth){
        val ld_uop = ld_iss2rrd.io.deq.bits(i-ld_base_idx)
        val rs1_addr= ld_uop.prs1
        val rs2_addr= ld_uop.prs2
        regfiles.io.readports(2*i).addr := rs1_addr
        regfiles.io.readports(2*i+1).addr := rs2_addr
        ld_op(i).uop := ld_uop

        ld_op(i).rs1_data := regfiles.io.readports(2*i).data
        ld_op(i).rs2_data := regfiles.io.readports(2*i+1).data
    }
    val st_op = WireInit(VecInit.fill(stIssueParam.issueWidth)(0.U.asTypeOf(new LSUReq())))
    val st_base_idx = ldIssueParam.issueWidth + ld_base_idx
    for(i <- st_base_idx until st_base_idx+stIssueParam.issueWidth){
        val st_uop = st_iss2rrd.io.deq.bits(i-st_base_idx)
        val rs1_addr= st_uop.prs1
        val rs2_addr= st_uop.prs2
        regfiles.io.readports(2*i).addr := rs1_addr
        regfiles.io.readports(2*i+1).addr := rs2_addr
        st_op(i).uop := st_uop

        st_op(i).rs1_data := regfiles.io.readports(2*i).data
        st_op(i).rs2_data := regfiles.io.readports(2*i+1).data
    }
    st_iss2rrd.io.deq.ready := st_rrd2ex.io.enq.ready
    ld_iss2rrd.io.deq.ready := ld_rrd2ex.io.enq.ready
    int_iss2rrd.io.deq.ready := int_rrd2ex.io.enq.ready
///////////////////////////////////EX PIPE////////////////////////////////////
    val int_rrd2ex = withReset(reset.asBool ) {
        Module(new Queue(Vec(intIssueParam.issueWidth,new ExuReq), 1, pipe=true, flow=false)) }

    int_rrd2ex.suggestName("int_rrd2ex_pipe")
    int_rrd2ex.io.enq.valid := int_iss2rrd.io.deq.valid
    int_rrd2ex.io.enq.bits  := int_op
    for(i<- 0 until intIssueParam.issueWidth){
        exe_units(i).io.req.bits := int_rrd2ex.io.deq.bits(i)
        exe_units(i).io.req.valid:= int_rrd2ex.io.deq.valid
        
    }
    int_rrd2ex.io.deq.ready := exe_units.map(_.io.req.ready).reduce(_&&_)


    val ld_rrd2ex = withReset(reset.asBool ) {
        Module(new Queue(Vec(ldIssueParam.issueWidth,new LSUReq), 1, pipe=true, flow=false)) }

    
    ld_rrd2ex.suggestName("ld_rrd2ex_pipe")
    ld_rrd2ex.io.enq.valid := ld_iss2rrd.io.deq.valid
    ld_rrd2ex.io.enq.bits  := ld_op
    for(i<- 0 until ldIssueParam.issueWidth){
        io.lsu.ld_req(i).bits := ld_rrd2ex.io.deq.bits(i)
        io.lsu.ld_req(i).valid:= ld_rrd2ex.io.deq.valid
    }
    ld_rrd2ex.io.deq.ready := true.B

    val st_rrd2ex = withReset(reset.asBool ) {
        Module(new Queue(Vec(stIssueParam.issueWidth,new LSUReq), 1, pipe=true, flow=false)) }
    st_rrd2ex.suggestName("st_rrd2ex_pipe")
    st_rrd2ex.io.enq.valid := st_iss2rrd.io.deq.valid
    st_rrd2ex.io.enq.bits  := st_op
    io.lsu.st_req.bits := st_rrd2ex.io.deq.bits(0)
    io.lsu.st_req.valid:= st_rrd2ex.io.deq.valid

    st_rrd2ex.io.deq.ready := true.B
///////////////////////////////////WB RESP////////////////////////////////////
    val intNumWriteports = alujmp_unit.numIrfWritePorts + muldiv_unit.numIrfWritePorts
    val int_wb_resp  = VecInit(Seq(alujmp_unit.io.iresp,muldiv_unit.io.iresp).flatten)
    for(i <- 0 until intNumWriteports){
        val wbpdst = int_wb_resp(i).bits.uop.pdst 
        val wbwen  = int_wb_resp(i).valid && int_wb_resp(i).bits.uop.ctrl.rf_wen
        val wbdata = int_wb_resp(i).bits.wb_data
        regfiles.io.writeports(i).addr := wbpdst
        regfiles.io.writeports(i).data := wbdata
        regfiles.io.writeports(i).wen  := wbwen
        rob.io.wb_resp(i).bits := int_wb_resp(i).bits
        rob.io.wb_resp(i).valid := int_wb_resp(i).valid
    }
    val memNumWriteports = ldIssueParam.issueWidth
    val ld_wb_resp  = io.lsu.ld_wb_resp
    for(i<-intNumWriteports until intNumWriteports+memNumWriteports){
        val wbpdst = ld_wb_resp(i-intNumWriteports).bits.uop.pdst 
        val wbwen  = ld_wb_resp(i-intNumWriteports).valid && int_wb_resp(i-intNumWriteports).bits.uop.ldst_val
        val wbdata = ld_wb_resp(i-intNumWriteports).bits.wb_data
        regfiles.io.writeports(i).addr := wbpdst
        regfiles.io.writeports(i).data := wbdata
        regfiles.io.writeports(i).wen  := wbwen
        rob.io.wb_resp(i).bits := ld_wb_resp(i-intNumWriteports).bits
        rob.io.wb_resp(i).valid := ld_wb_resp(i-intNumWriteports).valid
    }
}