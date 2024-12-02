package  grvcore
import chisel3._
import chisel3.util._



class IfuMessage extends Bundle {
    val inst = (UInt(32.W))
    val pc = (UInt(32.W))
}

class IduMessage extends Bundle {
    
    val pc = (UInt(32.W))
    val inst = (UInt(32.W))
    val imm   = (UInt(32.W))
    val ctrl  = new DecodeCtrlIO()
    val issue = new RegselMessage()
}
class BypassMessage extends  Bundle{
    val bypass = new RegfileMessage()
    val valid = Bool()
}
class RegfileReq extends Bundle{
    val rxs1      = (Bool())
    val rxs2      = (Bool())
    val raddr1    = (UInt(5.W))
    val raddr2    = (UInt(5.W))
}
class RegfileResp extends Bundle{
    val data1    = (UInt(32.W))
    val data2    = (UInt(32.W))
}
class RegselMessage extends Bundle with ScalarOpConstants{
    val RegfileReq = new RegfileReq()
    val sel_alu2  = (UInt(SZ_A2.W))
    val sel_alu1  = (UInt(SZ_A1.W))
}

class IssueMessage extends Bundle {
    
    val pc = (UInt(32.W))
    val inst = (UInt(32.W))
    val RegfileResp   = new RegfileResp()
    val imm   = (UInt(32.W))
    val ctrl  = (new DecodeCtrlIO)

}

class ExuMessage extends Bundle {
    val inst        = (UInt(32.W))
    val pc          = (UInt(32.W))
    val rfreq       = new RegfileMessage()
    val br_taken    = Bool()
    val br_target   = (UInt(32.W))
}
class BjpMessage extends Bundle{
    val target = (UInt(32.W))
    val taken  = Bool()
}

class RegfileMessage extends Bundle{
    val wen   = Bool()
    val waddr = UInt(5.W)
    val data  = UInt(32.W)
}
class DecodeCtrlIO extends Bundle with ALUFN with ScalarOpConstants{
    val legal = Bool()
    val branch = Bool()
    val jal = Bool()
    val jalr = Bool()

    val alu_fn = UInt(SZ_ALU_FN.W)
    val mem = Bool()
    val mem_cmd = UInt(SZ_MEM.W)
    val mul = Bool()
    val div = Bool()
    val wen = Bool()

}

