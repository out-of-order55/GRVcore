package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SRAMHelper extends BlackBox {
    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Bool())
        val raddr = Input(UInt(32.W))
        val ren   = Input(Bool())
        val rdata = Output(UInt(32.W))
    })

}

//只用于仿真
class AXI4SRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule{
    val beatBytes = 4
    val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
        Seq(AXI4SlaveParameters(
            address       = address,
            executable    = true,
            supportsWrite = TransferSizes.none,
            supportsRead  = TransferSizes(1, beatBytes),
            interleavedId = Some(0))
        ),
        beatBytes  = beatBytes)))

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        val (in, _) = node.in(0)

        val sram = Module(new SRAMHelper)
        sram.io.clock := clock
        sram.io.reset := reset
        val s_idle :: s_wait_ack :: Nil = Enum(2)

        val state       = RegInit(s_idle)
        val state_n     = WireInit(state)
        val dataCnt     = Reg(UInt(log2Ceil(in.ar.bits.params.lenBits).W))
        val burstEnable = WireInit(!(in.ar.bits.burst.orR))//只支持fixed突发
        // burstEnable:=()

        val beatBytes   = WireInit((in.ar.bits.params.dataBits/8).U)
        val transLen    = WireInit((in.ar.bits.len))
        val raddr       = Reg(UInt(32.W))
        dontTouch(burstEnable)
        dontTouch(beatBytes)
        state := state_n
        // assert((in.ar.fire)&(burstEnable)&(transLen=/=0.U),"if burst close,AXLEN must be 0")
        raddr := Mux(state===s_idle,in.ar.bits.addr,raddr + beatBytes)
        switch(state){
            is(s_idle){
                when(in.ar.fire){
                    state_n := s_wait_ack
                }
            }
            is(s_wait_ack){
                when(!burstEnable){
                    state_n := s_idle 
                }.elsewhen(dataCnt===transLen){
                    state_n := s_idle
                }
            }
        }
        when(state===s_idle&state_n===s_wait_ack){
            dataCnt := 0.U
        }.elsewhen(state===s_wait_ack&in.r.fire){
            dataCnt := dataCnt + 1.U
        }

        
        sram.io.raddr := raddr
        sram.io.ren := state===s_wait_ack

        in.ar.ready := (state === s_idle)
        
        assert(!(in.ar.fire && in.ar.bits.size === 3.U), "do not support 8 byte transfter")

        in.r.bits.data := sram.io.rdata
        in.r.bits.id := RegEnable(in.ar.bits.id, in.ar.fire)
        in.r.bits.resp := 0.U
        in.r.bits.last := state===s_wait_ack&(dataCnt===transLen)
        in.r.valid := (state === s_wait_ack)

        in.aw.ready := false.B
        in. w.ready := false.B
        in. b.valid := false.B

        assert(!in.aw.valid, "do not support write operations now")
        assert(!in. w.valid, "do not support write operations now")
    }
}

class ICacheWrapper(implicit p: Parameters) extends LazyModule with HasICacheParameters{
    val masterNode = AXI4MasterNode(Seq(
    AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
        name = "ICache")))))
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch{
        val (master, _) = masterNode.out(0)
        val icache = Module(new ICache)
        val io     = IO(new ICacheBundle)
        io <> icache.io
        icache.imaster <>master
        dontTouch(icache.imaster.ar)
        dontTouch(icache.imaster.r)
        dontTouch(icache.io)
    
}
}
/* TODO
ICache功能验证：
1.只发送一个请求（对齐访问）:初步测试框架搭建完成
	1.先读再读：只会有一次miss
	2.遍历地址：全部miss
PASS
2.测试跨行访问：
	发送两笔访问，下次访问相同地址，最多只有两次miss
    遍历所有地址，并检查读出的数据是否正确
完成
3.tag的初始化問題：完成
4.ICache流水线验证：如果发生miss，重新取指令，外部刷新s1阶段，重定向s0阶段，直到取回数据
这种cache弊端就是必须连续访问
 */

class Checker extends BlackBox {
    val io = IO(new Bundle {
        val clock = Input(Clock())
        val reset = Input(Bool())
        val finish= Input(Bool())
        val ret   = Input(Bool())
    })
}
class CacheTest1 (implicit p:Parameters)extends LazyModule with HasICacheParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x1000)))
    val icache = LazyModule(new ICacheWrapper())
    lsram.node:=icache.masterNode 
    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch {
        // io.finish := icache.module.io.resp.valid

        val check        = Module(new Checker)
        val s0_vpc       = WireInit(0.U(XLEN.W))
        val s0_valid     = WireInit(false.B)
        val timer        = RegInit(0.U(32.W))
        val f1_clear     = WireInit(false.B)
        val s1_vpc       = RegNext(s0_vpc)
        val s2_vpc       = RegNext(s1_vpc)
        val start        = RegInit(false.B)
        val s1_valid     = RegNext(s0_valid, false.B)
        val s2_valid     = RegNext(s1_valid && !f1_clear, false.B)

        start := true.B
        val start1 = RegNext(start)
        val data_fail = Wire(Bool())
        val fail = WireInit(icache.module.io.resp.valid&s2_valid&data_fail)
        dontTouch(fail)
/////////////////Checker///////////////////
        check.io.clock := clock
        check.io.reset := reset
        when(s0_vpc===(blockBytes*50).U){
            check.io.finish := true.B
            check.io.ret    := false.B 
        }.elsewhen(timer===50.U){
            // println("Bus fail")
            check.io.finish := true.B
            check.io.ret    := true.B 
        }.elsewhen(fail){
            // println("Data fail")
            check.io.finish := true.B
            check.io.ret    := true.B 
        }
        .otherwise{
            check.io.finish := false.B
            check.io.ret    := false.B 
        }
        dontTouch(start)
        dontTouch(start1)
        dontTouch(f1_clear)
//////////////////////////////////////////
        when ((start)&(!start1)){
            s0_valid   := true.B
            s0_vpc     := 0.U
        }
        when(s1_valid&(!f1_clear)){
            s0_vpc   := s1_vpc + blockBytes.U 
            s0_valid := true.B
        }

        when(s2_valid&(!icache.module.io.resp.valid)){
            f1_clear := true.B
            s0_vpc   := s2_vpc
            s0_valid := true.B
        }

        //检测总线卡死
        
        timer := timer + 1.U
        when(icache.module.io.resp.valid){
            timer := 0.U
        }
        //检测数据是否正确
        data_fail := ( 0 until bankNum).map{i=>
            icache.module.io.resp.bits.data(i)=/=(s2_vpc/fetchBytes.U+i.U)
        }.reduce(_|_)
        icache.module.io.req.valid      := s0_valid
        icache.module.io.req.bits.raddr := s0_vpc 
        icache.module.io.s1_kill        := f1_clear
        icache.module.io.s2_kill        := false.B
        icache.module.io.invalidate     := false.B
        icache.module.io.resp.bits.data := DontCare
        dontTouch(icache.module.io)
    }
  // s.dontTouchPorts()
}
class CacheTest2 (implicit p:Parameters)extends LazyModule with HasICacheParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x1000)))
    val icache = LazyModule(new ICacheWrapper())
    lsram.node:=icache.masterNode 
    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch {
        val testoff      = 0xc
        val check        = Module(new Checker)
        val s0_vpc       = WireInit(0.U(XLEN.W))
        val s0_valid     = WireInit(false.B)
        val timer        = RegInit(0.U(32.W))
        val f1_clear     = WireInit(false.B)
        val s1_vpc       = RegNext(s0_vpc)
        val s2_vpc       = RegNext(s1_vpc)
        val start        = RegInit(false.B)
        val s1_valid     = RegNext(s0_valid, false.B)
        val s2_valid     = RegNext(s1_valid && !f1_clear, false.B)

        start := true.B
        val start1 = RegNext(start)
        val data_fail = Wire(Bool())
        val fail = WireInit(icache.module.io.resp.valid&s2_valid&data_fail)
        dontTouch(fail)
        val finish =  RegInit(false.B)
        val ret    =  RegInit(false.B)
/////////////////Checker///////////////////
        check.io.clock := clock
        check.io.reset := reset
        check.io.finish := finish
        check.io.ret    := ret   
        when(s0_vpc===(blockBytes*50+testoff).U){
            finish := true.B
            ret    := false.B 
        }.elsewhen(timer===50.U){
            // println("Bus fail")
            printf("Bus Fail\n")
            finish := true.B
            ret    := true.B 
        }.elsewhen(fail){
            // println("Data fail")
            printf("Data Fail\n")
            finish := true.B
            ret    := true.B 
        }
        .otherwise{
            finish := false.B
            ret    := false.B 
        }
        dontTouch(start)
        dontTouch(start1)
        dontTouch(f1_clear)
//////////////////////////////////////////
        when ((start)&(!start1)){
            s0_valid   := true.B
            s0_vpc     := testoff.U
        }
        when(s1_valid&(!f1_clear)){
            s0_vpc   := s1_vpc + blockBytes.U 
            s0_valid := true.B
        }

        when(s2_valid&(!icache.module.io.resp.valid)){
            f1_clear := true.B
            s0_vpc   := s2_vpc
            s0_valid := true.B
        }

        //检测总线卡死
        
        timer := timer + 1.U
        when(icache.module.io.resp.valid){
            timer := 0.U
        }
        //检测数据是否正确
        data_fail := ( 0 until bankNum).map{i=>
            icache.module.io.resp.bits.data(i)=/=(s2_vpc/fetchBytes.U+i.U)
        }.reduce(_|_)
        icache.module.io.req.valid      := s0_valid
        icache.module.io.req.bits.raddr := s0_vpc 
        icache.module.io.s1_kill        := f1_clear
        icache.module.io.s2_kill        := false.B
        icache.module.io.invalidate     := false.B
        icache.module.io.resp.bits.data := DontCare
        dontTouch(icache.module.io)
    }
  // s.dontTouchPorts()
}