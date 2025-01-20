package grvcore

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

/* 、
首先测试写流水线：
目前测试了简单的写入测试，并无问题，接下来需要：
1.测试store buf的合并功能:finish
2.测试store buf timeout功能:触发条件比较苛刻，需要DCache的mshr满:finish

对于读流水线：
1.测试数据重组：waiting
2.测试是否可以正确提交:finish
3.测试ldq的写回端口冲突:不好测试
4.bank conflict：完成

对于整体的测试：
1.测试forward
2.测试check unorder
 */
class LSUTest (implicit p:Parameters)extends LazyModule with HasDCacheParameters{
    val lsram = LazyModule(new AXI4SRAM(AddressSet.misaligned(0x0, 0x2000)))
    val llsu = LazyModule(new LSU())
    lsram.node:=llsu.masterNode 
    override lazy val module = new Impl
    class Impl extends LazyModuleImp(this) with DontTouch 
    with freechips.rocketchip.rocket.constants.MemoryOpConstants{


        val lsu = llsu.module
        val check        = Module(new Checker)

        val timer        = RegInit(0.U(32.W))


        timer := timer +1.U
        val s_idle::s_dis::s_req::s_wb::s_commit::Nil= Enum(5)
        val state = RegInit(s_idle)
        val state_n = WireInit(state)
        val init_cnt=RegInit(0.U(2.W)) 
        val dis_cnt = RegInit(0.U(2.W))
/////////////////Checker///////////////////
        check.io.clock := clock
        check.io.reset := reset

        check.io.finish := false.B
        check.io.ret    := false.B
        when(timer===200.U){
            check.io.finish := true.B
            check.io.ret    := false.B
        }
        lsu.io.flush := false.B
        
/////////////////////////  RAED ////////////////////////////////
        // lsu.io.ld_req := DontCare
        // // lsu.io.dis(0) := DontCare
        // for(i <- 0 until numReadport){
        //     lsu.io.ld_req(i).valid := false.B
        //     lsu.io.ld_req(i).bits := 0.U.asTypeOf(new LSUReq)
        // }
        val enq = RegInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new MicroOp))))
        val commitMsg = RegInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new MicroOp))))
        val enq_idxs = lsu.io.dis(1).enq_idx
        val ld_req=RegInit(VecInit.fill(numReadport)(0.U.asTypeOf(Valid(new LSUReq))) )
        val addr_cnt = RegInit(0.U(2.W))
        dontTouch(enq)
        for(i <- 0 until coreWidth){
            // lsu.io.dis(1).enq(i).valid := false.B
            enq(i).valid := false.B
            lsu.io.dis(0).enq(i).valid := enq(i).valid
            lsu.io.dis(0).enq(i).bits := enq(i).bits
            lsu.io.dis(1).enq(i).valid := enq(i).valid
            lsu.io.dis(1).enq(i).bits := enq(i).bits

            commitMsg(i).valid :=false.B
            when(init_cnt===3.U){
                enq(i).bits.rob_idx := i.U*timer
                enq(i).bits.mem_cmd := M_XRD
                enq(i).bits.mem_signed:=false.B
                enq(i).bits.mem_size := 2.U
                enq(i).valid := true.B
            }.elsewhen(state===s_dis){
            
                enq(i).bits.ldq_idx := Mux(enq_idxs(i).valid,enq_idxs(i).bits,enq(i).bits.ldq_idx)
            }
            when(state_n===s_commit&&state===s_wb){
                commitMsg(i).valid := i.U===0.U&&true.B
                commitMsg(i).bits  := RegNext(lsu.io.ld_wb_resp(i).bits.uop)
            }
            lsu.io.commit.valid(i) := commitMsg(i).valid
            lsu.io.commit.commit_uops(i):= commitMsg(i).bits 
        }
        dis_cnt := Mux(state===s_dis,dis_cnt+1.U,dis_cnt)
        init_cnt := Mux(state===s_idle&&(!reset.asBool),init_cnt+1.U,init_cnt)
        for(i <- 0 until numReadport){
            ld_req(i).valid := false.B
            when(state===s_dis&&state_n===s_req){
                ld_req(i).bits.uop := enq(i).bits
                ld_req(i).bits.rs1_data:= (timer+i.U)<<2
                ld_req(i).bits.rs2_data:= "h12345678".U
                ld_req(i).valid := true.B
                addr_cnt := addr_cnt +1.U
            }
        }
        lsu.io.ld_req:=ld_req


////////////////////////  WRITE ////////////////////////////////
        lsu.io.st_req := DontCare
        // // lsu.io.dis(0) := DontCare
        // for(i <- 0 until numReadport){
        //     lsu.io.ld_req(i).valid := false.B
        //     lsu.io.ld_req(i).bits := 0.U.asTypeOf(new LSUReq)
        // }
        // val enq = RegInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new MicroOp))))
        // val commitMsg = RegInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new MicroOp))))
        // val enq_idxs = lsu.io.dis(0).enq_idx
        // val st_req=RegInit((0.U.asTypeOf(Valid(new LSUReq))) )
        // dontTouch(enq)
        // for(i <- 0 until coreWidth){
        //     // lsu.io.dis(1).enq(i).valid := false.B
        //     enq(i).valid := false.B
        //     lsu.io.dis(0).enq(i).valid := enq(i).valid
        //     lsu.io.dis(0).enq(i).bits := enq(i).bits
        //     lsu.io.dis(1).enq(i).valid := enq(i).valid
        //     lsu.io.dis(1).enq(i).bits := enq(i).bits

        //     commitMsg(i).valid :=false.B
        //     when(init_cnt===3.U){
        //         enq(i).bits.rob_idx := i.U*timer
        //         enq(i).bits.mem_cmd := Mux(i.U===0.U,M_XWR,M_SFENCE)
        //         enq(i).bits.mem_signed:=false.B
        //         enq(i).bits.mem_size := 0.U
        //         enq(i).valid := true.B
        //     }.elsewhen(state===s_dis){
            
        //         enq(i).bits.stq_idx := Mux(enq_idxs(i).valid,enq_idxs(i).bits,enq(i).bits.stq_idx)
        //     }
        //     when(state_n===s_commit&&state===s_wb){
        //         commitMsg(i).valid := i.U===0.U&&true.B
        //         commitMsg(i).bits  := lsu.io.st_wb_resp.bits.uop
        //     }
        //     lsu.io.commit.valid(i) := commitMsg(i).valid
        //     lsu.io.commit.commit_uops(i):= commitMsg(i).bits 
        // }
        // dis_cnt := Mux(state===s_dis,dis_cnt+1.U,dis_cnt)
        // init_cnt := Mux(state===s_idle&&(!reset.asBool),init_cnt+1.U,init_cnt)
        // lsu.io.st_req:=st_req
        // st_req.valid := false.B
        // val addr_cnt = RegInit(0.U(2.W))
        // when(state===s_dis&&state_n===s_req){
        //     st_req.bits.uop := enq(0).bits
        //     // st_req.bits.rs1_data:= (addr_cnt)<<2
        //     st_req.bits.rs1_data:= timer<<2
        //     st_req.bits.rs2_data:= "h12345678".U
        //     st_req.valid := true.B
        //     addr_cnt := addr_cnt +1.U
        // }
        state := state_n
	    switch (state){
            is(s_idle){
                when(init_cnt===3.U){
                    state_n := s_dis
                }
            }
            is(s_dis){
                when(dis_cnt===3.U){
                    state_n := s_req
                }
            }
            is(s_req){
                when(lsu.io.ld_wb_resp.map(_.valid).reduce((_||_))){
                    state_n:= s_wb
                }

                
            }
            is(s_wb){
                state_n := s_commit
            }
            is(s_commit){
                state_n := s_idle
            }
        }

    }
}