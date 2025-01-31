package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

case class IQueueParams(
    nEntries: Int = 8
)




class IQueueResp(implicit p: Parameters) extends GRVBundle{
    val uops = Vec(coreWidth,Valid(new MicroOp()))
}
/* 
area:7300
目前需要2R4W，
1.使用bank+offset，来写入每个bank和读出每个bank，需要维护woff和roff
2.使用两个阵列，一个阵列为输入的格式，一个阵列为输出的格式：会引入额外的周期，其中香山和boom使用的是第二种

//需要维护读写指针，维护读写数据是否有效
 */
class IQueue(implicit p: Parameters) extends GRVModule with HasFrontendParameters{
    val io=IO(new Bundle {
        val enq   = Flipped(Decoupled(new FetchBundle()))
        val deq   = new DecoupledIO(new IQueueResp())
        val clear = Input(Bool())
    }
    )
    require (iqentries > fetchWidth)
    require (iqentries % coreWidth == 0)
    val ibuf:Vec[MicroOp] = RegInit(VecInit.fill(iqentries)(0.U.asTypeOf(new MicroOp)))

    val outputEntries = RegInit(VecInit.fill(coreWidth)(0.U.asTypeOf(Valid(new MicroOp))))

    val in_entry = Wire(Vec(fetchWidth,Valid(new MicroOp())))
    val iqSz =  log2Ceil(iqentries+1)-1
    val enq_ptr  = RegInit(0.U(log2Ceil(iqentries+1).W))
    val deq_ptr  = RegInit(0.U(log2Ceil(iqentries+1).W))

    val almost_full  = RegNext(enq_ptr+fetchWidth.U===deq_ptr(iqSz-1,0))
    dontTouch(almost_full)
    for(i <- 0 until fetchWidth){
        in_entry(i) := DontCare
        in_entry(i).valid       := io.enq.bits.mask(i)
        in_entry(i).bits.ftq_idx:= io.enq.bits.ftq_idx
        in_entry(i).bits.inst   := io.enq.bits.insts(i)
        in_entry(i).bits.is_br  := (io.enq.bits.cfi_type===1.U)&&(i.U===io.enq.bits.cfi_idx.bits)
        in_entry(i).bits.is_jal := io.enq.bits.is_jal&&(i.U===io.enq.bits.cfi_idx.bits)
        in_entry(i).bits.is_jalr:= io.enq.bits.is_jalr&&(i.U===io.enq.bits.cfi_idx.bits)
        in_entry(i).bits.pc_off := i.U<<2
        if(hasDebug){
            in_entry(i).bits.pc := (i.U<<2) + bankAlign(io.enq.bits.pc)
        }
        in_entry(i).bits.taken  := io.enq.bits.cfi_taken&&(i.U===io.enq.bits.cfi_idx.bits)
        // in_entry(i).bits.uopc   := DontCare
        
    }   

/////////////////////Write Ibuf///////////////
    val full = WireInit(false.B)
    val numEnq = Mux(io.enq.valid,PopCount(io.enq.bits.mask),0.U)
    dontTouch(numEnq)
    val enqNextPtr = enq_ptr + numEnq
    val do_enq     = io.enq.fire&&(!io.clear)
    dontTouch(do_enq)
    dontTouch(enqNextPtr)
    val numValid = WireInit(0.U((iqSz+1).W))
    dontTouch(numValid)
    numValid := Mux((enq_ptr>=deq_ptr),enq_ptr-deq_ptr,iqentries.U-(deq_ptr-enq_ptr))
        // Mux(enq_ptr>=deq_ptr,enq_ptr-deq_ptr,iqentries.U-(deq_ptr-enq_ptr))
    //满的判断需要不仅在本周期判断：也要在上周期判断，然后寄存下来到本周期
    
    full := (enq_ptr(iqSz)=/=deq_ptr(iqSz)&&(numEnq+enq_ptr)(iqSz-1,0)>deq_ptr(iqSz-1,0))||
    numValid===iqentries.U
    // assert(numValid>iqentries.U,"IQ overflow")
    dontTouch(full)

    io.enq.ready := (!full)&(!io.clear)



    val enq_idxs = VecInit.tabulate(fetchWidth)(i => PopCount(io.enq.bits.mask.asBools.take(i)))
    val enq_offset = VecInit(enq_idxs.map(_+enq_ptr(iqSz-1,0)))
    dontTouch(enq_idxs)
    dontTouch(enq_offset)
    for(i <- 0 until fetchWidth){
        for(j <- 0 until iqentries){
            when(do_enq&&io.enq.bits.mask(i)&&enq_offset(i)===j.U){
                ibuf(j) := in_entry(i).bits
            }

        }
    }
    when(io.clear){
        enq_ptr := 0.U
    }.elsewhen(do_enq){
        enq_ptr := enqNextPtr
    }
//////////////////////write buf to output//////////
    /* 
    如果供应不足：ibuf没有那么多的数据,表示数据已经快空了，需要
    想进入outputEntries：该队列没数据，有数据但是后端已经准备接受数据
     */

    val almost_empty = numValid<coreWidth.U
    val deqNextPtr   = deq_ptr+coreWidth.U
    val numOut       = PopCount(outputEntries.map(_.valid))
    val to_out       = (!io.clear)&&(!almost_empty)&&((numOut===0.U)||numOut===coreWidth.U&&io.deq.fire)//送入output entry
    io.deq.valid     := (!io.clear)&&(numOut===coreWidth.U)
    dontTouch(to_out)
    dontTouch(deqNextPtr)
    val outputOH     = (0 until coreWidth).map{i=>
        (0 until iqentries).map{j=>
            j.U===i.U+deq_ptr(iqSz-1,0)
        }
    }
/* 
1.outputEntries有数据，dec准备接受，如果ibuf还有数据，就true否则false

2.outputEntries有数据，dec不准备接受：需要保存状态
3.outputEntries无数据，ibuf有数据，将数据写入
 */
    for(i <- 0 until coreWidth){
        outputEntries(i).bits := Mux(!io.deq.fire&&numOut===coreWidth.U,outputEntries(i).bits ,
                        Mux1H(outputOH(i),ibuf))
        outputEntries(i).valid:= Mux(io.clear,false.B,
                                Mux(!io.deq.fire&&numOut===coreWidth.U,outputEntries(i).valid,
                                Mux((io.deq.fire||numOut===0.U)&&(!almost_empty),true.B,false.B)))

        io.deq.bits.uops(i) := outputEntries(i)
    }
    when(io.clear){
        deq_ptr := 0.U
    }.elsewhen(to_out){
        deq_ptr := deqNextPtr
    }

}
