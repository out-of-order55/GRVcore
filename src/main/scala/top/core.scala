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
1.每个阶段的流水线必须得下一级全部接受才可以流动，之后可以将其分开，也就是分多个queue:finish
2.需要在执行阶段前加入exdecode:finish
3.需要为每个ld_uop分配最近的stq_idx
 */
class GRVCore()(implicit p: Parameters) extends GRVModule with HasFrontendParameters
{
    val io = IO(new Bundle {
        val ifu = (new FrontendIO)
        val lsu = Flipped(new LSUBundle)
    })
    val memWidth = ldIssueParam.issueWidth
    val decode_units = Seq.fill(coreWidth)(Module(new DecodeUnit))
    val rename_stage = Module(new RenameStage)
    val alujmp_unit      = Module(new ALUExuUnit(true,true,false,false))
    val alu_unit      = Module(new ALUExuUnit(true,false,false,false,true))
    // val alu_unit         = Module(new ALUExuUnit(true,false,false,false))
    val muldiv_unit      = Module(new ALUExuUnit(false,false,true,true))


    val numDispatchWidth   = ldIssueParam.dispatchWidth+stIssueParam.dispatchWidth+intIssueParam.dispatchWidth
    val numIntIssueWakeupPorts = alujmp_unit.numIrfWritePorts +alu_unit.numIrfWritePorts+ muldiv_unit.numIrfWritePorts + ldIssueParam.issueWidth// 4 for alu 2 for lsu
    val busytable = Module(new BusyTable(numIntIssueWakeupPorts))
    val dispatcher       = Module(new ComplexDispatcher)
    dispatcher.io := DontCare
    //int 的可以发射到两个alu
    val ld_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,ldIssueParam.issueWidth,ldIssueParam.numEntries,IQT_LD.litValue.toInt,ldIssueParam.dispatchWidth))
    ld_iss_unit.suggestName("ld_issue_unit")
    val st_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,stIssueParam.issueWidth,stIssueParam.numEntries,IQT_ST.litValue.toInt,stIssueParam.dispatchWidth))
    st_iss_unit.suggestName("st_issue_unit")
    val int_iss_unit     = Module(new BaseIssueUnit(numIntIssueWakeupPorts,intIssueParam.issueWidth,intIssueParam.numEntries,IQT_INT.litValue.toInt,intIssueParam.dispatchWidth))
    int_iss_unit.suggestName("int_issue_unit")

    val issue_units      = Seq(int_iss_unit,ld_iss_unit,st_iss_unit)
    def iss_length = issue_units.length
    val exe_units        = Seq(alujmp_unit,alu_unit,muldiv_unit)


    val numIrfReadPorts  =  2*(intIssueParam.issueWidth+ldIssueParam.issueWidth + stIssueParam.issueWidth)
    val numIrfWritePorts = numIntIssueWakeupPorts

    val regfiles         = Module(new RegFile(numIrfReadPorts,numIrfWritePorts,Seq.fill(numIntIssueWakeupPorts) {true}))
    val rob              = Module(new ROB(numIntIssueWakeupPorts+1))

    val commit_flush     = WireInit(rob.io.flush)
    io.ifu := DontCare
///////////////////////////////////FRONTEND///////////////////////////
    val dec2rename = withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(Vec(coreWidth,new MicroOp), 1, pipe=true, flow=false)) }
    io.ifu.commit                     := rob.io.commit.bits
    io.ifu.fetchpacket.ready          := dec2rename.io.enq.ready
    io.ifu.redirect_val                 := false.B
    io.ifu.redirect_flush               := false.B
    io.ifu.flush_icache                 := false.B   
    io.ifu.redirect_pc                  := 0.U
    io.ifu.get_pc                     <> alujmp_unit.io.get_ftq_pc
    
    io.ifu.brupdate <>rob.io.br_update
    when(RegNext(commit_flush.valid)){
        io.ifu.redirect_flush             := true.B
        io.ifu.redirect_val               := true.B
        io.ifu.redirect_pc                := commit_flush.bits.epc
    }

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
    val rename2dis = withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(Vec(coreWidth,new MicroOp), 1, pipe=true, flow=false)) }
    rename2dis.suggestName("rename2dis_pipe")
    rename_stage.io.dec_uops  <> dec2rename.io.deq 
    rename_stage.io.commit    := rob.io.commit.bits
    rename_stage.io.redirect  := commit_flush.valid
    rename2dis.io.enq<>rename_stage.io.dis_uops  
    rename2dis.io.deq.ready   := dispatcher.io.ren_uops.map(_.ready).reduce(_&&_)


///////////////////////////////////Busytable&dis///////////////////////////

    val rename_uop = WireInit(rename2dis.io.deq.bits)
    val rename_req = WireInit(rename2dis.io.deq.valid)
    for(i <- 0 until coreWidth){
        busytable.io.ren_uops(i).valid := rename_req
        busytable.io.ren_uops(i).bits  := rename2dis.io.deq.bits(i)
        rename_uop(i).prs1_busy := busytable.io.busy_resps(i).prs1_busy
        rename_uop(i).prs2_busy := busytable.io.busy_resps(i).prs2_busy
    }
///////////////////////////////////DISPATCHER//////////////////////////
    for(i <- 0 until coreWidth){
        dispatcher.io.ren_uops(i).bits := rename_uop(i)
        dispatcher.io.ren_uops(i).valid:= rename_req
    }
    //dispatch 按照type分发，所以dispatch一样
    // rob.io.enq.uops <> dispatcher.io.dis_uops(0)
    val rob_enq_ready = WireInit(VecInit.fill(intIssueParam.dispatchWidth)(false.B))
    for(i <- 0 until intIssueParam.dispatchWidth){
        val valid = VecInit(dispatcher.io.dis_uops.map{j=> j(i).valid})
        val uop   = VecInit(dispatcher.io.dis_uops.map{j=> j(i).bits})
        val enq_uop = Mux1H(valid,uop)
        rob.io.enq.uops(i).valid := valid.reduce(_||_)
        rob.io.enq.uops(i).bits  := enq_uop
        rob_enq_ready(i) := rob.io.enq.uops(i).ready
    }
//////////////////////////////////INT/////////////////////////////////
    val int_iss_idx = issueParams.indexWhere(_.iqType == IQT_INT.litValue) 
    val alu_uop = WireInit(dispatcher.io.dis_uops(int_iss_idx))
    val rob_idx= WireInit(rob.io.enq_idxs)
    val int_fu_type = Seq(alujmp_unit.io.fu_types,alu_unit.io.fu_types,muldiv_unit.io.fu_types)
    val rob_ready = WireInit(rob.io.enq.uops.map(_.fire).reduce(_&&_))
    for(i <- 0 until intIssueParam.dispatchWidth){
        alu_uop(i).bits.rob_idx := Mux(rob_idx(i).valid,rob_idx(i).bits,0.U)
        
        int_iss_unit.io.dis_uops(i).valid := alu_uop(i).valid&rob_ready
        int_iss_unit.io.dis_uops(i).bits  := alu_uop(i).bits
        dispatcher.io.dis_uops(int_iss_idx)(i).ready := int_iss_unit.io.dis_uops(i).ready&&rob_enq_ready(i)

    }
    
    // int_iss_unit.io.dis_uops<> alu_uop

    
//////////////////////////////////LD/////////////////////////////////
    val ld_iss_idx = issueParams.indexWhere(_.iqType == IQT_LD.litValue) 
    val ld_uop = WireInit(dispatcher.io.dis_uops(ld_iss_idx))
    val ldq_idx= WireInit(io.lsu.dis(1).enq_idx)
    val mem_fu_types = FU_MEM
    val ld_enq_ready = WireInit(VecInit(io.lsu.dis(1).enq.map(_.ready)))
    for(j <- 0 until ldIssueParam.dispatchWidth){
        ld_uop(j).bits.ldq_idx := Mux(ldq_idx(j).valid,ldq_idx(j).bits,0.U)
        ld_uop(j).bits.rob_idx := Mux(rob_idx(j).valid,rob_idx(j).bits,0.U)
        ld_iss_unit.io.dis_uops(j).valid := ld_uop(j).valid&rob_ready
        ld_iss_unit.io.dis_uops(j).bits  := ld_uop(j).bits
        dispatcher.io.dis_uops(ld_iss_idx)(j).ready := ld_iss_unit.io.dis_uops(j).ready&&rob_enq_ready(j)&&ld_enq_ready(j)
        io.lsu.dis(1).enq(j).valid := ld_uop(j).valid
        io.lsu.dis(1).enq(j).bits  := ld_uop(j).bits
        
    }
    // io.lsu.dis(1).enq <> dispatcher.io.dis_uops(ld_iss_idx)
    // ld_iss_unit.io.dis_uops<>ld_uop
//////////////////////////////////ST/////////////////////////////////
    val st_iss_idx = issueParams.indexWhere(_.iqType == IQT_ST.litValue) 
    val st_uop = WireInit(dispatcher.io.dis_uops(st_iss_idx))
    val stq_idx= WireInit(io.lsu.dis(0).enq_idx)
    val st_enq_ready = WireInit(VecInit(io.lsu.dis(0).enq.map(_.ready)))
    for(j <- 0 until stIssueParam.dispatchWidth){
        st_uop(j).bits.ldq_idx := Mux(stq_idx(j).valid,stq_idx(j).bits,0.U)
        st_uop(j).bits.rob_idx := Mux(rob_idx(j).valid,rob_idx(j).bits,0.U)
        // st_iss_unit.io.fu_using(j) := mem_fu_types
        st_iss_unit.io.dis_uops(j).valid := st_uop(j).valid&rob_ready
        st_iss_unit.io.dis_uops(j).bits  := st_uop(j).bits
        dispatcher.io.dis_uops(st_iss_idx)(j).ready := st_iss_unit.io.dis_uops(j).ready&&rob_enq_ready(j)&&st_enq_ready(j)

        io.lsu.dis(0).enq(j).valid := st_uop(j).valid
        io.lsu.dis(0).enq(j).bits  := st_uop(j).bits
    }
    // st_iss_unit.io.dis_uops<>st_uop
    // io.lsu.dis(0).enq <> dispatcher.io.dis_uops(st_iss_idx)

///////////////////////////////////ISSUE PIPE///////////////////////////////
    val wb_resp  = VecInit(Seq(alujmp_unit.io.iresp.map(_.bits),alu_unit.io.iresp.map(_.bits),muldiv_unit.io.iresp.map(_.bits),io.lsu.ld_wb_resp.map(_.bits)).flatten)
    val wb_resp_valid  = VecInit(Seq(alujmp_unit.io.iresp.map(_.valid),alu_unit.io.iresp.map(_.valid),muldiv_unit.io.iresp.map(_.valid),io.lsu.ld_wb_resp.map(_.valid)).flatten)
    dontTouch(wb_resp)
    dontTouch(wb_resp_valid)
    dontTouch(alujmp_unit.io.iresp)
    val int_iss2rrd = Seq.fill(intIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue((new MicroOp), 1, pipe=true, flow=false)) })
    int_iss2rrd.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"int_iss2rrd_pipe_$idx") 
    }


    val alujmp_ex_decoder =Module(new RegisterReadDecode(alujmp_unit.supportedFuncUnits))
    val alu_ex_decoder =Module(new RegisterReadDecode(alu_unit.supportedFuncUnits))
    val muld_ex_decoder= Module(new RegisterReadDecode(muldiv_unit.supportedFuncUnits))
    //  <> int_iss_unit.io.issue_uops
    //这里如果有一个有效的uop，就会全部有效，这里需要细分化
    val int_ex_decoder = Seq(alujmp_ex_decoder,alu_ex_decoder,muld_ex_decoder)
    
    for(i <- 0 until intIssueParam.issueWidth){
        val int_iss_uop = WireInit(int_iss_unit.io.issue_uops(i))
        dontTouch(int_ex_decoder(i).io.iss_valid)
        int_iss_unit.io.fu_using(i) := int_fu_type(i)
        int_ex_decoder(i).io.iss_valid      := int_iss_uop.valid
        int_ex_decoder(i).io.iss_uop        := int_iss_uop.bits
        int_iss2rrd(i).io.enq.valid         := int_iss_unit.io.issue_uops(i).valid
        int_iss2rrd(i).io.enq.bits          := int_ex_decoder(i).io.rrd_uop
        int_iss_unit.io.issue_uops(i).ready := int_iss2rrd(i).io.enq.ready
    }

    def memsupportedFuncUnits = {
        new SupportedFuncUnits(
        alu = true,
        jmp = false,
        mem = true,
        muld = false)
    }

    val ld_ex_decoder = Seq.fill(ldIssueParam.issueWidth)(Module(new RegisterReadDecode(memsupportedFuncUnits)))
    val ld_iss2rrd = Seq.fill(ldIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(new MicroOp, 1, pipe=true, flow=false)) })

    ld_iss2rrd.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"ld_iss2rrd_pipe_$idx") 
    }
    for(i <- 0 until ldIssueParam.issueWidth){
        val ld_iss_uop = WireInit(ld_iss_unit.io.issue_uops(i))
        ld_ex_decoder(i).io.iss_valid       := ld_iss_uop.valid
        ld_ex_decoder(i).io.iss_uop         := ld_iss_uop.bits
        ld_iss2rrd(i).io.enq.valid          := ld_iss_uop.valid&&ld_ex_decoder(i).io.rrd_valid
        ld_iss2rrd(i).io.enq.bits           := ld_ex_decoder(i).io.rrd_uop
        ld_iss_unit.io.issue_uops(i).ready  := ld_iss2rrd(i).io.enq.ready
        ld_iss_unit.io.fu_using(i)          := mem_fu_types
    }
    
    // val ld_ex_decoder = Module(new RegisterReadDecode())
    val st_ex_decoder = Seq.fill(stIssueParam.issueWidth)(Module(new RegisterReadDecode(memsupportedFuncUnits)))
    val st_iss2rrd    = Seq.fill(stIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(new MicroOp, 1, pipe=true, flow=false)) })

    st_iss2rrd.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"st_iss2rrd_pipe_$idx") 
    }
    for(i <- 0 until stIssueParam.issueWidth){
        val st_iss_uop = WireInit(st_iss_unit.io.issue_uops(i))
        st_ex_decoder(i).io.iss_valid       := st_iss_uop.valid
        st_ex_decoder(i).io.iss_uop         := st_iss_uop.bits
        st_iss2rrd(i).io.enq.valid          := st_iss_uop.valid&&st_ex_decoder(i).io.rrd_valid
        st_iss2rrd(i).io.enq.bits           := st_ex_decoder(i).io.rrd_uop
        st_iss_unit.io.issue_uops(i).ready  := st_iss2rrd(i).io.enq.ready
        st_iss_unit.io.fu_using(i)          := mem_fu_types
    }
    ///唤醒，这里目前有问题，delay需要改正
    for(i<- 0 until iss_length){
        for(j <- 0 until numIntIssueWakeupPorts){
            issue_units(i).io.wakeup(j).valid := wb_resp_valid(j)
            issue_units(i).io.wakeup(j).delay := 0.U
            issue_units(i).io.wakeup(j).pdst  := wb_resp(j).uop.pdst 
        }
        issue_units(i).io.flush := commit_flush.valid
    }

    // st_iss2rrd.io.enq <> st_iss_unit.io.issue_uops
///////////////////////////////////RRD/////////////////////////////////
    
    val int_op = WireInit(VecInit.fill(intIssueParam.issueWidth)(0.U.asTypeOf(new ExuReq())))
    for(i <- 0 until intIssueParam.issueWidth){
        val int_uop = int_iss2rrd(i).io.deq.bits
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
        val ld_uop = ld_iss2rrd(i-ld_base_idx).io.deq.bits
        val rs1_addr= ld_uop.prs1
        val rs2_addr= ld_uop.prs2
        regfiles.io.readports(2*i).addr := rs1_addr
        regfiles.io.readports(2*i+1).addr := rs2_addr
        ld_op(i-ld_base_idx).uop := ld_uop

        ld_op(i-ld_base_idx).rs1_data := regfiles.io.readports(2*i).data
        ld_op(i-ld_base_idx).rs2_data := regfiles.io.readports(2*i+1).data
    }



    val st_op = WireInit(VecInit.fill(stIssueParam.issueWidth)(0.U.asTypeOf(new LSUReq())))
    val st_base_idx = ldIssueParam.issueWidth + ld_base_idx
    for(i <- st_base_idx until st_base_idx+stIssueParam.issueWidth){
        val st_uop = st_iss2rrd(i-st_base_idx).io.deq.bits
        val rs1_addr= st_uop.prs1
        val rs2_addr= st_uop.prs2
        regfiles.io.readports(2*i).addr := rs1_addr
        regfiles.io.readports(2*i+1).addr := rs2_addr
        st_op(i-st_base_idx).uop := st_uop

        st_op(i-st_base_idx).rs1_data := regfiles.io.readports(2*i).data
        st_op(i-st_base_idx).rs2_data := regfiles.io.readports(2*i+1).data
    }

///////////////////////////////////EX PIPE////////////////////////////////////
    val int_rrd2ex = Seq.fill(intIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(new ExuReq, 1, pipe=true, flow=false)) })

    // int_rrd2ex.suggestName("int_rrd2ex_pipe")
    int_rrd2ex.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"int_rrd2ex_pipe_$idx") 
    }
    for(i<- 0 until intIssueParam.issueWidth){

        int_rrd2ex(i).io.enq.valid  := int_iss2rrd(i).io.deq.valid
        int_rrd2ex(i).io.enq.bits   := int_op(i)
        dontTouch(exe_units(i).io.req.ready)
        int_rrd2ex(i).io.deq.ready  := exe_units(i).io.req.ready
        exe_units(i).io.req.bits    := int_rrd2ex(i).io.deq.bits
        exe_units(i).io.req.valid   := int_rrd2ex(i).io.deq.valid
        int_iss2rrd(i).io.deq.ready := int_rrd2ex(i).io.enq.ready
    }


    rob.io.br_info <> alujmp_unit.io.brupdate////这里可能出现错误




    val ld_rrd2ex = Seq.fill(ldIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(new LSUReq, 1, pipe=true, flow=false)) })
    ld_rrd2ex.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"ld_rrd2ex_pipe_$idx") 
    }

    
    
    for(i<- 0 until ldIssueParam.issueWidth){
        ld_rrd2ex(i).io.enq.valid   := ld_iss2rrd(i).io.deq.valid
        ld_rrd2ex(i).io.enq.bits    := ld_op(i)
        io.lsu.ld_req(i).bits       := ld_rrd2ex(i).io.deq.bits
        io.lsu.ld_req(i).valid      := ld_rrd2ex(i).io.deq.valid
        ld_rrd2ex(i).io.deq.ready   := true.B
        ld_iss2rrd(i).io.deq.ready := ld_rrd2ex(i).io.enq.ready
    }
    

    val st_rrd2ex = Seq.fill(stIssueParam.issueWidth)(withReset(reset.asBool||commit_flush.valid) {
        Module(new Queue(new LSUReq, 1, pipe=true, flow=false)) })
    st_rrd2ex.zipWithIndex.foreach { case (q, idx) => 
        q.suggestName(s"st_rrd2ex_pipe_$idx") 
    }
    for(i<- 0 until stIssueParam.issueWidth){
        st_rrd2ex(i).io.enq.valid := st_iss2rrd(i).io.deq.valid
        st_rrd2ex(i).io.enq.bits     := st_op(i)
        st_rrd2ex(i).io.deq.ready := true.B
        st_iss2rrd(i).io.deq.ready := st_rrd2ex(i).io.enq.ready
    }

    io.lsu.st_req.bits := st_rrd2ex(0).io.deq.bits
    io.lsu.st_req.valid:= st_rrd2ex(0).io.deq.valid

    

    
    
    
///////////////////////////////////WB RESP////////////////////////////////////
    val intNumWriteports = alujmp_unit.numIrfWritePorts +alu_unit.numIrfWritePorts+ muldiv_unit.numIrfWritePorts
    val int_wb_resp  = VecInit(Seq(alujmp_unit.io.iresp,alu_unit.io.iresp,muldiv_unit.io.iresp).flatten)
    for(i <- 0 until intNumWriteports){
        val wbpdst = int_wb_resp(i).bits.uop.pdst 
        val wbwen  = int_wb_resp(i).valid && int_wb_resp(i).bits.uop.ldst_val
        val wbdata = int_wb_resp(i).bits.wb_data
        int_wb_resp(i).ready := true.B
        regfiles.io.writeports(i).addr := wbpdst
        regfiles.io.writeports(i).data := wbdata
        regfiles.io.writeports(i).wen  := wbwen
        rob.io.wb_resp(i).bits := int_wb_resp(i).bits
        rob.io.wb_resp(i).valid := int_wb_resp(i).valid
        busytable.io.wb_pdsts(i) := int_wb_resp(i).bits.uop.pdst
        busytable.io.wb_valids(i):= int_wb_resp(i).valid
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
        busytable.io.wb_pdsts(i) := ld_wb_resp(i-intNumWriteports).bits.uop.pdst
        busytable.io.wb_valids(i):= ld_wb_resp(i-intNumWriteports).valid
    }
    io.lsu.commit <> rob.io.commit.bits
    io.lsu.flush := commit_flush.valid
    val st_wb_resp = io.lsu.st_wb_resp
    rob.io.wb_resp(intNumWriteports+memNumWriteports).bits  := st_wb_resp.bits
    rob.io.wb_resp(intNumWriteports+memNumWriteports).valid := st_wb_resp.valid
    
///////////////////////////////////EXCEPTION////////////////////////////////////
    val check_unorder = WireInit(io.lsu.check_resp)
    rob.io.lsuexc.bits.cause:= 0.U
    rob.io.lsuexc.bits.uops := check_unorder.uop
    rob.io.lsuexc.valid  := check_unorder.redirect

    if(hasDebug){
        val commit_event = Module(new DifftestWrapper)
        commit_event.io.commit <> rob.io.commit.bits
    }
    override def toString: String =
    (GRVString("====Overall Core Params====") + "\n\n"
    + GRVString(
        "===Other Core Params===",
        "Fetch Width           : " + fetchWidth,
        "Decode Width          : " + coreWidth,
        "Issue Width           : " + issueParams.map(_.issueWidth).sum,
        "ROB Size              : " + ROBEntry,
        "Load/Store Unit Size  : " + numLDQs + "/" + numSTQs,
        "Num Int Phys Registers: " + numPregs)
    + "\n\n"
    + GRVString(
        "Num Wakeup Ports      : " + numIntIssueWakeupPorts,
        "Num Bypass Ports      : " + numIntIssueWakeupPorts) + "\n"
    + GRVString(
        "DCache Ways           : " + DCacheParam.nWays,
        "DCache Sets           : " + DCacheParam.nSets,
        "DCache nMSHRs         : " + DCacheParam.numMSHRs,
        "ICache Ways           : " + ICacheParam.nWays,
        "ICache Sets           : " + ICacheParam.nSets) + "\n")
    // print(toString)
}