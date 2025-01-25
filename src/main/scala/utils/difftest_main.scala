package grvcore
import chisel3._
import chisel3.util._
import grvcore.common._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config._
import chisel3.experimental._
class Difftest extends BlackBox {
    val io = IO(new Bundle {
        val clock       = Input(Clock())
        val reset       = Input(Bool())

        val commit_num  = Input(UInt(8.W))
        val commit_valid= Input(UInt(4.W))
        val commit_pc1   = Input(UInt(32.W))
        val commit_pc2   = Input(UInt(32.W))
        val commit_data1 = Input(UInt(32.W))
        val commit_data2 = Input(UInt(32.W))
    })
}
class Debug(implicit p: Parameters) extends GRVModule
{
    val io = IO(new Bundle {
        val commit      = Input(new CommitMsg)
        val commit_data = Input(Vec(coreWidth,UInt(XLEN.W)))
    })
    val commit_num = PopCount(io.commit.valid)
    val commit_uops = io.commit.commit_uops
    val commit_valid= WireInit((io.commit.valid))
    val commit_pc   = WireInit(VecInit(io.commit.commit_uops.map(_.pc)))
    val difftest    = Module(new Difftest)
    difftest.io.clock         := clock
    difftest.io.reset         := reset

    difftest.io.commit_valid := commit_valid.asUInt
    difftest.io.commit_pc1    := commit_pc(0)
    difftest.io.commit_pc2    := commit_pc(1)
    difftest.io.commit_data1 := io.commit_data(0)
    difftest.io.commit_data2 := io.commit_data(1)
    difftest.io.commit_num   := commit_num

}

