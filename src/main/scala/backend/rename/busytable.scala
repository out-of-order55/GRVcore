package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters

class BusyResp extends Bundle
{
    val prs1_busy = Bool()
    val prs2_busy = Bool()
}

class BusyTable(val numWbPorts: Int)(implicit p: Parameters) extends GRVModule
{
    val pregSz = log2Ceil(numPregs)

    val io = IO(new Bundle {
        val ren_uops = Input(Vec(coreWidth, Valid(new MicroOp)))
        val busy_resps = Output(Vec(coreWidth, new BusyResp))

        val wb_pdsts = Input(Vec(numWbPorts, UInt(pregSz.W)))
        val wb_valids = Input(Vec(numWbPorts, Bool()))
    })

    val busy_table = RegInit(0.U(numPregs.W))
    // Unbusy written back registers.
    val busy_table_wb = busy_table & ~(io.wb_pdsts zip io.wb_valids)
        .map {case (pdst, valid) => UIntToOH(pdst) & Fill(numPregs, valid.asUInt)}.reduce(_|_)
    // Rebusy newly allocated registers.
    val busy_table_next = busy_table_wb | (io.ren_uops)
        .map {uop => UIntToOH(uop.bits.pdst) & Fill(numPregs,uop.valid.asUInt)}.reduce(_|_)

    busy_table := busy_table_next

    // Read the busy table.
    for (i <- 0 until coreWidth) {
        val prs1_was_bypassed = (0 until i).map(j =>
        io.ren_uops(i).bits.lrs1 === io.ren_uops(j).bits.ldst && io.ren_uops(j).valid).foldLeft(false.B)(_||_)
        val prs2_was_bypassed = (0 until i).map(j =>
        io.ren_uops(i).bits.lrs2 === io.ren_uops(j).bits.ldst && io.ren_uops(j).valid).foldLeft(false.B)(_||_)

        io.busy_resps(i).prs1_busy := busy_table(io.ren_uops(i).bits.prs1) || prs1_was_bypassed 
        io.busy_resps(i).prs2_busy := busy_table(io.ren_uops(i).bits.prs2) || prs2_was_bypassed 
    }


}