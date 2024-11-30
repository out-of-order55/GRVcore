package grvcore
import chisel3._
import chisel3.util._
import org.apache.commons.lang3.builder.Diff

class difftest extends BlackBox{
    val io = IO(new Bundle {
        val pc    = Input(UInt(32.W))
        val valid = Input(Bool())
    })
}


class WBU(xlen:Int,IsDiff:Boolean) extends Module {

    val io = IO(new Bundle {
        val in    = (Flipped(Decoupled(new ExuMessage)))
        val bjp   =  Output(new BjpMessage())
        val wbu_bypass = Output(new BypassMessage())
        val regfile = Output(new RegfileMessage())
    })

    io.in.ready   := !io.in.bits.br_taken // NO BJP     
    io.bjp.taken := io.in.bits.br_taken
    io.bjp.target:= io.in.bits.br_target
    io.regfile <>io.in.bits.rfreq
    io.wbu_bypass.valid:=true.B
    io.wbu_bypass.bypass<>io.in.bits.rfreq

    if(IsDiff){
        val difftest = Module(new difftest())
        difftest.io.pc := io.in.bits.pc
        difftest.io.valid := io.in.valid
    }

}