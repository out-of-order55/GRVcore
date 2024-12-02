package grvcore
import chisel3._
import chisel3.util._
// import IDU.{ALUFN,ScalarOpConstants}
// import CoreIO._

object ALU extends ALUFN{

    def apply(op:UInt,src1:UInt,src2:UInt)={
        //add,sub
        val sub = op===FN_SUB||
                op===FN_SGEU||
                op===FN_SGE||
                op===FN_SLTU||
                op===FN_SLT||
                op===FN_SEQ||
                op===FN_SNE
        val add1=Mux(op===FN_SGEU||op===FN_SLTU,Cat(0.U,src1),Cat(src1(31),src1))
        val add2=Mux(op===FN_SGEU||op===FN_SLTU,Cat(0.U,src2),Cat(src2(31),src2))
        val adder1 = add1
        val adder2 = Mux(sub,~add2,add2)
        val addres = Wire((UInt(33.W)))
            addres:=adder1+adder2+op===FN_SUB

        // logic
        val logic = Mux(op===FN_AND,src1&src2,
                    Mux(op===FN_OR,src1|src2,
                    Mux(op===FN_XOR,src1^src2,0.U)))
        val eq    = addres===0.U 
        //cmp
        val cmp_out =   Mux(op===FN_SGEU||op===FN_SGE,~addres(32),
                        Mux(op===FN_SLTU||op===FN_SLT,addres(32),
                        Mux(op===FN_SEQ,eq,
                        Mux(op===FN_SNE,~eq,0.U))))
        //shift
        val shift_r = (((Cat((op===FN_SRA)&&src1(31),src1)).asSInt)>>src2(4,0))(31,0)
        val shift = Mux(op===FN_SL,src1<<(src2(4,0)),
                    Mux(op===FN_SR||op===FN_SRA,shift_r.asUInt,0.U))
        val out = shift|logic|addres(31,0)
        out(31,0)
    }
}


class EXU(xlen:Int) extends Module with ALUFN with ScalarOpConstants{
    val io = IO(new Bundle {

        val in    = (Flipped(Decoupled(new IssueMessage)))
        val br_taken = Output(Bool())
        val br_pc = Output(UInt(32.W))

        val dcache = Flipped(new CacheIO(xlen, xlen))

        val out = ((Decoupled(new ExuMessage)))
        val exu_bypass = Output(new BypassMessage())
    })
    val exu_ready_go = Wire(Bool())

    val alures = ALU(io.in.bits.ctrl.alu_fn,io.in.bits.RegfileResp.data1,io.in.bits.RegfileResp.data2)

    val memre = io.in.bits.ctrl.mem_cmd===M_LB||io.in.bits.ctrl.mem_cmd===M_LBU||
                    io.in.bits.ctrl.mem_cmd===M_LH||
                    io.in.bits.ctrl.mem_cmd===M_LHU||
                    io.in.bits.ctrl.mem_cmd===M_LW
    val memwe = io.in.bits.ctrl.mem_cmd===M_SW||io.in.bits.ctrl.mem_cmd===M_SH|| io.in.bits.ctrl.mem_cmd===M_SB
    val mask  = alures(1,0).asUInt
    val offset = (alures(1) << 4.U).asUInt | (alures(0) << 3.U).asUInt

    exu_ready_go := Mux(memre||memwe,io.dcache.resp.valid,true.B)
    io.out.valid := io.in.valid&&exu_ready_go
    io.in.ready  := (!io.in.valid)||(io.out.ready&&exu_ready_go)

    io.br_taken := Mux((io.in.bits.ctrl.branch||io.in.bits.ctrl.jal||io.in.bits.ctrl.jalr),alures,0.U)
    io.br_pc    := Mux(io.in.bits.ctrl.branch||io.in.bits.ctrl.jal,io.in.bits.imm,io.in.bits.imm+io.in.bits.RegfileResp.data1)


    
    io.dcache.req.valid      := io.in.valid&&((memre)||(memwe))
    io.dcache.req.bits.addr  := alures>> 2.U << 2.U
    io.dcache.req.bits.data  := io.in.bits.RegfileResp.data2<<offset
    io.dcache.req.bits.mask  := MuxLookup(io.in.bits.ctrl.mem_cmd, "b0000".U)(
    Seq(M_SW -> "b1111".U, M_SH -> ("b11".U << mask), M_SB -> ("b1".U << mask))
    )
    io.dcache.resp.ready     := true.B
    val memrdata = io.dcache.resp.bits.data>>offset

    val rdata = MuxLookup(io.in.bits.ctrl.mem_cmd, io.dcache.resp.bits.data.zext)(
        Seq(
            M_LH -> memrdata(15, 0).asSInt,
            M_LB -> memrdata(7, 0).asSInt,
            M_LHU -> memrdata(15, 0).zext,
            M_LBU -> memrdata(7, 0).zext
        )
    ).asUInt

    io.out.bits.pc   := io.in.bits.pc
    io.out.bits.inst := io.in.bits.inst
    io.out.bits.br_taken := io.br_taken
    io.out.bits.br_target := io.br_pc
    
    io.out.bits.rfreq.wen  := io.in.valid&&io.in.bits.ctrl.wen
    io.out.bits.rfreq.data := Mux((io.in.bits.ctrl.branch||io.in.bits.ctrl.jal||io.in.bits.ctrl.jalr),io.in.bits.pc+4.U,Mux(memre,rdata,alures))
    io.out.bits.rfreq.waddr := io.in.bits.inst(11,7)

    io.exu_bypass.valid := true.B
    io.exu_bypass.bypass.data := io.out.bits.rfreq.data 
    io.exu_bypass.bypass.waddr := io.out.bits.rfreq.waddr 
    io.exu_bypass.bypass.wen   := io.out.bits.rfreq.wen 
}
