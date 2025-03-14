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
    dontTouch(busy_table_wb)
    val busy_table_ren = (io.ren_uops)
        .map {uop => UIntToOH(uop.bits.pdst) & Fill(numPregs,(uop.valid&&uop.bits.ldst_val).asUInt)}.reduce(_|_)
    // val ren_debug = VecInit(io.ren_uops
    //     .map {uop => UIntToOH(3.U) })
    // val ren_debug1 = VecInit(io.ren_uops
    //     .map {uop => UIntToOH(4.U) })
    // val ren_debug2 = VecInit(io.ren_uops
    //     .map {uop => UIntToOH(4.U,5) })
    // dontTouch(ren_debug)
    // dontTouch(ren_debug1)
    // dontTouch(ren_debug2)
    val busy_table_next = busy_table_wb | busy_table_ren
    dontTouch(busy_table_next)
    dontTouch(busy_table_ren)
    busy_table := busy_table_next&(~(1.U(numPregs.W)))
    
    // Read the busy table.
    for (i <- 0 until coreWidth) {
        val prs1_was_bypassed = (0 until i).map(j =>
        io.ren_uops(i).bits.lrs1 === io.ren_uops(j).bits.ldst && io.ren_uops(j).valid&&io.ren_uops(j).bits.ldst_val).foldLeft(false.B)(_||_)&&
        io.ren_uops(i).bits.lrs1=/=0.U&&io.ren_uops(i).bits.lrs1_rtype =/= RT_X
        val psr1_was_wakeup = (0 until numWbPorts).map{idx=>io.wb_valids(idx)&&
            (io.wb_pdsts(idx)===io.ren_uops(i).bits.prs1)}.reduce(_||_)
        val prs2_was_bypassed = (0 until i).map(j =>
        io.ren_uops(i).bits.lrs2 === io.ren_uops(j).bits.ldst && io.ren_uops(j).valid&&io.ren_uops(j).bits.ldst_val).foldLeft(false.B)(_||_)&&
        io.ren_uops(i).bits.lrs2=/=0.U&&io.ren_uops(i).bits.lrs2_rtype =/= RT_X
        val psr2_was_wakeup = (0 until numWbPorts).map{idx=>io.wb_valids(idx)&&
            (io.wb_pdsts(idx)===io.ren_uops(i).bits.prs2)}.reduce(_||_)
        dontTouch(prs2_was_bypassed)
        dontTouch(prs1_was_bypassed)
        io.busy_resps(i).prs1_busy := (!psr1_was_wakeup)&&(busy_table(io.ren_uops(i).bits.prs1) || prs1_was_bypassed) 
        io.busy_resps(i).prs2_busy := (!psr2_was_wakeup)&&(busy_table(io.ren_uops(i).bits.prs2) || prs2_was_bypassed) 
    }


}