package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

case class IQueueParams(
    nEntries: Int = 16
)




class IQueueResp(implicit p: Parameters) extends GRVBundle{
    val uops = Vec(coreWidth,Valid(new MicroOp()))
}
/* 
目前需要2R4W，
1.使用bank+offset，来写入每个bank和读出每个bank，需要维护woff和roff
2.使用两个阵列，一个阵列为输入的格式，一个阵列为输出的格式：会引入额外的周期，其中香山和boom使用的是第二种
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
}
