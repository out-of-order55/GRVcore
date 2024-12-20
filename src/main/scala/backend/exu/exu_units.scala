package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters


class ExuResp(implicit p: Parameters) extends GRVBundle{
    val uops        = new MicroOp
    val wb_data     = UInt(XLEN.W)
}