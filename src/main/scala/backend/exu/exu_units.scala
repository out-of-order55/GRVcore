package grvcore

import chisel3._
import chisel3.util._
import grvcore.common._
import org.chipsalliance.cde.config.Parameters

/* 
目前EXU准备两个：
1.ALU+MUL
2.ALU+JMP+DIV

然后就是LSU
 */
class ExuResp(implicit p: Parameters) extends GRVBundle{
    val uops        = new MicroOp
    val wb_data     = UInt(XLEN.W)
}