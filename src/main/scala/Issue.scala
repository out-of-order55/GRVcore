package grvcore
import chisel3._
import chisel3.util._

class Issue(xlen:Int) extends Module {
    val io = IO(new Bundle {
        val in    = (Flipped(Decoupled(new IduMessage)))
        
        val RegfileReq = Output(new RegfileReq())
        val RegfileResp = Input(new RegfileResp())
        val ExuBypass   =  Input(new BypassMessage())
        val WbuBypass   =  Input(new BypassMessage())
        val out         = Decoupled(new IssueMessage())
    })
    io.RegfileReq <> io.in.bits.issue.RegfileReq

    val issue_ready_go = io.ExuBypass.valid||io.WbuBypass.valid
    
    io.out.valid := issue_ready_go&&io.in.valid
    io.in.ready  := (!io.in.valid)&&(io.out.ready&&issue_ready_go)
    val resp = Wire(new RegfileResp())
    //bypass
    val rs1_ex_hazard = (io.ExuBypass.bypass.wen)&&(io.ExuBypass.bypass.waddr===io.RegfileReq.raddr1)&&(io.RegfileReq.raddr1.orR)
    val rs1_wb_hazard = (io.WbuBypass.bypass.wen)&&(io.WbuBypass.bypass.waddr===io.RegfileReq.raddr1)&&(io.RegfileReq.raddr1.orR)

    val rs2_ex_hazard = (io.ExuBypass.bypass.wen)&&(io.ExuBypass.bypass.waddr===io.RegfileReq.raddr2)&&(io.RegfileReq.raddr2.orR)
    val rs2_wb_hazard = (io.WbuBypass.bypass.wen)&&(io.WbuBypass.bypass.waddr===io.RegfileReq.raddr2)&&(io.RegfileReq.raddr2.orR)


    resp.data1 := Mux(rs1_ex_hazard,io.ExuBypass.bypass.data,Mux(rs1_wb_hazard,io.WbuBypass.bypass.data,io.RegfileResp.data1))
    resp.data2 := Mux(rs2_ex_hazard,io.ExuBypass.bypass.data,Mux(rs2_wb_hazard,io.WbuBypass.bypass.data,io.RegfileResp.data2))


    io.out.bits.imm := io.in.bits.imm
    io.out.bits.pc  := io.in.bits.pc
    io.out.bits.inst:=io.in.bits.inst
    io.out.bits.ctrl <> io.in.bits.ctrl
    io.out.bits.RegfileResp <> resp
}