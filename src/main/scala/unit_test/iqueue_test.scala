package grvcore
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class enqGen(implicit p:Parameters) extends GRVModule with HasFrontendParameters{
    val io = IO(new Bundle{
        val enq_gen= Decoupled(new FetchBundle())
    })

    val gen_pc        = RegInit(0.U(XLEN.W))
    val gen_mask      = RegInit(15.U(fetchWidth.W))
    val gen_inst      = RegInit(VecInit.fill(fetchWidth)(0.U(XLEN.W)))
    val gen_ftq_idx   = RegInit(0.U(log2Ceil(ftqentries).W))
    val valid         = RegInit(false.B)
    valid := false.B
    io.enq_gen.valid := valid
    
    dontTouch(io.enq_gen.fire)
    when(io.enq_gen.fire){
        io.enq_gen.bits.mask          := gen_mask  
        io.enq_gen.bits.pc            := gen_pc  
        io.enq_gen.bits.ftq_idx       := gen_ftq_idx  
        io.enq_gen.bits.insts         := gen_inst    
        io.enq_gen.bits.bpd_meta      := 0.U         
        io.enq_gen.bits.br_mask       := 0.U   
        io.enq_gen.bits.bpSrc         := 0.U  
        io.enq_gen.bits.cfi_idx.bits  := 0.U      
        io.enq_gen.bits.cfi_idx.valid := 0.U      
        io.enq_gen.bits.cfi_type      := 0.U  
        io.enq_gen.bits.is_jal        := 0.U   
        io.enq_gen.bits.is_jalr       := 0.U
        io.enq_gen.bits.cfi_taken     := 0.U      
        io.enq_gen.bits.ghist         := 0.U      
        gen_pc   := gen_pc + 4.U
        gen_mask := MaskUpper(UIntToOH(bankoffset(gen_pc + 4.U)))
        gen_ftq_idx:= gen_ftq_idx + 1.U
    }.otherwise{
        io.enq_gen := DontCare
    }
}

class deqGen(implicit p:Parameters) extends GRVModule{
    val io = IO(new Bundle{
        val deq_gen = Flipped(Decoupled(new IQueueResp()))
    })
    io.deq_gen.ready := RegInit(true.B)
    val deqData = Reg(new IQueueResp())
    dontTouch(deqData)
    when(io.deq_gen.fire){
        deqData := io.deq_gen.bits
    }

}
/* 
1.连续读写，测试读出的数据正误
2.后端没有请求，测试full是否还可以写入数据
3.前端没有有效指令，测试ibuf是否写入了
4.外部发送clear指令，测试ibuf是否reset
 */
class IQueueTest (implicit p:Parameters)extends GRVModule with HasFrontendParameters with DontTouch{
    val ibuf    = Module(new IQueue)
    val enq_gen = Module(new enqGen)
    val deq_gen = Module(new deqGen)   
    val check        = Module(new Checker)
    check.io.clock := clock
    check.io.reset := reset 
    ibuf.io.clear := false.B
    enq_gen.io.enq_gen<>ibuf.io.enq
    deq_gen.io.deq_gen<>ibuf.io.deq
    val check_pc = RegInit(0.U(XLEN.W))
    val check_ftq_idx = RegInit(0.U(log2Ceil(ftqentries).W))
    val mask   = Wire(UInt(fetchWidth.W))
    mask := MaskUpper(UIntToOH(bankoffset(check_pc),fetchWidth))
    val offset = PopCount(mask)
    val next_offset = PopCount(MaskUpper(UIntToOH(bankoffset(check_pc+4.U),fetchWidth)))
    val cnt    = RegInit(0.U(log2Ceil(fetchWidth+1).W))
    when(deq_gen.io.deq_gen.fire){
        cnt := cnt + coreWidth.U
    }
    when(cnt+coreWidth.U>=offset){
        cnt := 0.U
        check_pc := Mux(next_offset===1.U,check_pc+8.U,check_pc + 4.U)
        check_ftq_idx := Mux(next_offset===1.U,check_ftq_idx + 2.U,check_ftq_idx + 1.U)
    }

    dontTouch(cnt)
    dontTouch(offset)
    dontTouch(mask)
    dontTouch(check_ftq_idx)
    val datafail = Wire(Bool()) 
    datafail := false.B
    when(deq_gen.io.deq_gen.valid){
        datafail := check_ftq_idx=/=deq_gen.io.deq_gen.bits.uops(0).bits.ftq_idx||
                    Mux(cnt+coreWidth.U>offset||next_offset===1.U,(check_ftq_idx+1.U)=/=deq_gen.io.deq_gen.bits.uops(1).bits.ftq_idx,
                    (check_ftq_idx)=/=deq_gen.io.deq_gen.bits.uops(1).bits.ftq_idx)

    }
    // when(cnt+coreWidth.U===)
    val timer = RegInit(0.U(32.W))
    timer := timer +1.U
    when(datafail){
        check.io.finish := true.B
        check.io.ret    := true.B 
    }.elsewhen(timer===300.U){
        check.io.finish := true.B
        check.io.ret    := false.B
    }
    .otherwise{
        check.io.finish := false.B
        check.io.ret    := false.B 
    }
    
}
