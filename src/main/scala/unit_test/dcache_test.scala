package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._




/* 
1.先写后读，读出的和写入的一样，必然miss，此时读需要等写
2.写掩码，然后读
3.并未考虑读缺失一样的情况重复分配mshr
4.DCache在写时异常hit，目前怀疑为tag初始化和valid问题：解决

测试：
1.多读单写：完成测试
2.测试wmask:分别对不同bank的测mask操作，然后再细粒度的对每个bank的mask操作:完成测试
3.测试替换：连续写入多个idx相同但tag不同的:目前不太好实现，之后在联调的时候可以试一试:目前只能小范围测试
 */

class DCacheTest (implicit p:Parameters)extends LazyModule with HasDCacheParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x2000)))
    val ldcache = LazyModule(new DCache())
    lsram.node:=ldcache.masterNode 
    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch {
        // io.finish := icache.module.io.resp.valid
        val dcache       = ldcache.module 
        val check        = Module(new Checker)

        val timer        = RegInit(0.U(32.W))

        val start        = RegInit(false.B)
        val test_cnt = RegInit(0.U(32.W))
        val data_cnt =RegInit(0.U(32.W))
        val s_idle::s_write::s_read::Nil = Enum(3)
        val state = RegInit(s_idle)
        val state_n = WireInit(state)

        val rvalid  = RegInit(VecInit.fill(numReadport)(false.B))
        val raddr   = Wire(Vec(numReadport,UInt(XLEN.W)))

        val wvalid  = RegInit(false.B)
        val waddr   = RegInit(0.U(XLEN.W))
        val wdata   = Reg(Vec(bankNum,UInt(XLEN.W)))
        // val gendata   = Wire(Vec(bankNum,UInt(8.W)))
        val wmask   = Reg(Vec(bankNum,UInt((XLEN/8).W)))
        timer := timer +1.U
/////////////////Checker///////////////////
        check.io.clock := clock
        check.io.reset := reset

        // val data = Wire(Vec(bankNum,UInt(XLEN.W)))
        // val addr = Wire(UInt(XLEN.W))
        // val mask = Wire(Vec(bankNum,UInt((XLEN/8).W)))
        val fail = (0 until numReadport).map{i=>
            ((i.U+data_cnt*2.U+test_cnt)<<((i.U+2.U*data_cnt(0))*8.U))=/=dcache.io.read(i).resp.bits.data&&dcache.io.read(i).resp.bits.hit
        }.reduce(_||_)

        check.io.finish := false.B
        check.io.ret    := false.B
        when(test_cnt===4.U||timer===200.U){
            check.io.finish := true.B
            check.io.ret    := false.B
        }
        // .elsewhen(RegNext(RegNext(fail))){
        //     check.io.finish := true.B
        //     check.io.ret    := true.B
        // }
        dontTouch(fail)

////////////////////////////////////////////////////////////////////
        val read_finish = dcache.io.read.map{read=>
            read.resp.valid&&read.resp.bits.hit
        }.reduce(_&&_)
        state := state_n
        // waddr := Mux(state_n===s_write,waddr+test_cnt*16.U,waddr)
        waddr := Mux(state_n===s_write,waddr+test_cnt*"h1000".U,waddr)
        switch (state){
            is(s_idle){
                state_n := s_write
            }
            is(s_write){
                state_n := s_read
            }
            is(s_read){
                when(data_cnt===1.U&&read_finish){
                    state_n := s_idle
                }
            }
        }

        test_cnt := Mux(state===s_read&&state_n===s_idle,test_cnt+1.U,test_cnt)
        data_cnt := Mux(state===s_idle,0.U,Mux(state===s_read&&read_finish,data_cnt+1.U,data_cnt))
        
        wvalid := Mux(state===s_idle&&state_n===s_write,true.B,Mux(dcache.io.write.req.fire,false.B,wvalid))
        //test2
        // for(i <- 0 until bankNum){
        //     gendata(i) := (test_cnt+i.U)
        //     wdata(i) := Cat(gendata.map(_.asUInt).reverse)
							
        //     wmask(i) := ("b1".U)<<i.U
        // }
        //test3
        for(i <- 0 until bankNum){
            wdata(i) := (test_cnt+i.U)
            wmask(i) := 15.U
        }
        dontTouch((wmask))
        for(i <- 0 until numReadport){
            rvalid(i) := Mux(dcache.io.read(i).req.fire,false.B,
                            Mux(state===s_write&&state_n===s_read||dcache.io.read(i).resp.valid&&state_n=/=s_idle,true.B,rvalid(i)))
                
            raddr(i)  := waddr + (data_cnt*2.U+i.U)*4.U
        }
        dontTouch(rvalid)
        dontTouch(raddr)
        dcache.io.read      := DontCare
        dcache.io.s1_kill   := false.B
        dcache.io.s2_kill   := false.B
        dcache.io.flush     := false.B
        dontTouch(dcache.io.write.resp)
        dcache.io.write.req.valid := wvalid
        dcache.io.write.req.bits.addr := waddr
        dcache.io.write.req.bits.data := wdata
        dcache.io.write.req.bits.mask := wmask


        for(i<- 0 until numReadport ){
            dcache.io.read(i).req.valid := rvalid(i)
            dcache.io.read(i).req.bits.addr:= raddr(i)
        }
        dontTouch((dcache.io.read))

        
//////////////////////////////////////////

    }
}