package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._

///////////////////////////Parameters//////////////////////
import org.chipsalliance.cde.config._
case object ICacheKey extends Field[ICacheParams]


case class ICacheParams(
    nSets: Int = 256,
    nWays: Int = 2,
    rowBits: Int = 128,
    // nTLBSets: Int = 1,
    // nTLBWays: Int = 32,
    // nTLBBasePageSectors: Int = 4,
    // nTLBSuperpages: Int = 4,
    // cacheIdBits: Int = 0,
    // tagECC: Option[String] = None,
    // dataECC: Option[String] = None,
    // itimAddr: Option[BigInt] = None,
    prefetch: Boolean = false,
    blockBytes: Int = 16,
    fetchBytes: Int = 4,
    
    ) {

}

trait HasICacheParameters extends HasGRVParameters{
	val bankNum     = blockBytes/fetchBytes
	val indexWidth  = log2Ceil(nSets)
	val offsetWidth = log2Ceil(blockBytes*8)
	val tagWidth    = XLEN-indexWidth-offsetWidth
	val bankWidth   = log2Ceil(blockBytes*8/bankNum)
	val fetchPacket = fetchBytes*4
	val numReadport = 2
}



////////////////////////////////////////////////////////////
class CacheWriteIO(implicit  p:Parameters) extends GRVBundle with HasICacheParameters{
    val wen   = Input(Bool())
    val wdata = Input(UInt(XLEN.W))
    val waddr = Input(UInt(XLEN.W))
}
class CacheReadIO(implicit  p:Parameters) extends GRVBundle with HasICacheParameters{
    val ren   = Input(Bool())
    val raddr = Input(UInt(XLEN.W))
    // val rdata = Output(Vec(blk_size/(dataWidth/8),UInt(dataWidth.W)))
}
class ICacheResp(implicit  p:Parameters) extends GRVBundle with HasICacheParameters
{
	val data = Vec((bankNum),UInt(XLEN.W))
}

class ICacheReq(implicit  p:Parameters) extends GRVBundle with HasICacheParameters
{
	val raddr = Input(UInt(XLEN.W))
}
class MissMsg(implicit  p:Parameters) extends GRVBundle with HasICacheParameters{
    val miss   = (Bool())
    val addr   = (UInt(XLEN.W))
}
class ICacheBundle(implicit  p:Parameters)  extends GRVBundle with HasICacheParameters
{

	val req = Flipped(Decoupled(new ICacheReq))
	// val s1_paddr = Input(UInt(XLEN.W)) // delayed one cycle w.r.t. req
	val s1_kill = Input(Bool()) // delayed one cycle w.r.t. req
	val s2_kill = Input(Bool()) // delayed two cycles; prevents I$ miss emission
	val resp = Valid(new ICacheResp())
	val invalidate = Input(Bool())


}
class CacheMsg(implicit p:Parameters)extends GRVBundle with HasICacheParameters{
    val tag    = UInt(tagWidth.W)
    val index  = UInt(indexWidth.W)
    val offset = UInt(offsetWidth.W)
    val bank   = UInt(bankWidth.W)
}

class SRAMIO(implicit p:Parameters)extends GRVBundle with HasICacheParameters{
    val enable = Input(Bool())
    val write = Input(Bool())
    val addr = Input(UInt(10.W))
    val dataIn = Input(UInt(XLEN.W))
    val dataOut = Output(UInt(XLEN.W))
}



class TagRAM(implicit p:Parameters) extends GRVModule with HasICacheParameters{
    val io = IO(new SRAMIO())
    val mem = SyncReadMem(nSets, UInt(XLEN.W))
    io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
}
class DataRAM(implicit p:Parameters) extends GRVModule with HasICacheParameters{
    val io = IO(new SRAMIO())
    val mem = SyncReadMem(nSets, UInt(XLEN.W))
    io.dataOut := mem.readWrite(io.addr, io.dataIn, io.enable, io.write)
}
/* TODO
ICache功能验证：
1.只发送一个请求（对齐访问）:初步测试框架搭建完成
	1.先读再读：只会有一次miss
	2.遍历地址：全部miss
2.测试跨行访问：
	发送两笔访问，下次访问相同地址，最多只有两次miss
	2.遍历所有地址
3.tag的初始化問題
4.ICache流水线验证：如果发生miss，重新取指令，外部刷新s1阶段，重定向s0阶段，直到取回数据
这种cache弊端就是必须连续访问
 */
class MissUnit(implicit p:Parameters) extends  GRVModule with HasICacheParameters{
	val missmsg0      = IO(Input(new MissMsg()))
	val missmsg1      = IO(Input(new MissMsg()))
	val refillData    = IO(Output(Vec(numReadport,Vec(bankNum,UInt(XLEN.W)))))
	val finish        = IO(Output(Bool()))
	val imaster   	  = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val refillMask    = IO(Output(UInt(numReadport.W)))
	val miss = missmsg0.miss|missmsg1.miss
	//normal->no miss req->has miss  wait_ack->wait data end->receive data
	/* 
	normal:		没有发生miss的状态	
	req:		生成miss的axi信号	
	wait_ack:	等待接受数据
	end:		看到last信号，缺失处理结束
	 */
	val s_reset :: s_normal::s_req :: s_wait_ack :: s_end :: Nil = Enum(5)
	val state = RegNext(s_reset)
	val state_n = WireInit(state)
	val s_refillData = Reg(Vec(numReadport,Vec(bankNum,UInt(XLEN.W))))
	val s_refillMask	 = RegInit(0.U(numReadport.W))//哪一个port发生miss
////////////////////////AXI//////////////////////////
	val ar_fire = imaster.ar.fire
	val r_fire 	= imaster.r.fire

	imaster.aw := DontCare
	imaster.w  := DontCare
	imaster.b  := DontCare
	
	val ar = Reg(new AXI4BundleAR(CPUAXI4BundleParameters()))
	val r  = Reg(new AXI4BundleR(CPUAXI4BundleParameters()))
	ar <> imaster.ar.bits


	val end 		= Reg(Bool())
	val rcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))
	val rready 		= Reg(Bool())
	val arvalid		= RegInit(false.B)
	imaster.r.ready := rready
	imaster.ar.valid:= arvalid
	//生成axi信号，默认一笔传输的数据大小为4字节，突发长度为blk_size或者2倍的blk
	when(state===s_normal&state_n===s_req){
		ar.id    := 1.U
		ar.addr  := Mux(missmsg0.miss,missmsg0.addr,missmsg1.addr)
		ar.len   := Mux(missmsg1.miss&missmsg0.miss,(2*blockBytes/4-1).U,(blockBytes/4-1).U)
		ar.size  := "b010".U
		ar.burst := "b00".U
		ar.lock  := DontCare
		ar.cache := DontCare
		ar.prot  := DontCare
		ar.qos   := DontCare
		rcnt := 0.U
		arvalid := true.B
		s_refillMask := Cat(missmsg1.miss,missmsg0.miss).asUInt
	}.elsewhen(ar_fire){
		arvalid := false.B
		rready := true.B
	}.elsewhen(imaster.r.bits.last){
		rready := false.B
	}

	when(state===s_wait_ack&imaster.r.fire){
		s_refillData((rcnt/(blockBytes.U))(log2Ceil(numReadport)-1,0))((rcnt%(blockBytes.U))(log2Ceil(bankNum)-1,0)) := imaster.r.bits.data
		rcnt := rcnt + 1.U
	}

	when(state===s_wait_ack&state_n===s_end){
		end := true.B
	}.otherwise{
		end :=false.B
	}
	refillData := s_refillData
	finish := end
	refillMask := s_refillMask
/////////////////////////////////////////////////////

	state := state_n

	switch (state){
		is(s_reset){
			state_n := s_normal
		}
		is(s_normal){
			when(miss){
				state_n := s_req
			}
		}
		is(s_req){
			when(ar_fire){
				state_n := s_wait_ack
			}
		}
		is(s_wait_ack){
			when(r_fire&imaster.r.bits.last){
				state_n := s_end
			}
		}
		is(s_end){
			state_n := s_normal
		}
	}
}

class ICache(implicit p:Parameters) extends GRVModule with HasICacheParameters{
	//将输入的地址变为index，tag形式
	def AddressSplitter(addr:UInt)(implicit p:Parameters)={
		val res = Wire(new CacheMsg())
		res.tag    := addr(XLEN-1,(indexWidth+offsetWidth))
		res.index  := addr((indexWidth+offsetWidth)-1,offsetWidth)
		res.offset := addr((offsetWidth)-1,0)
		res.bank   := addr(offsetWidth-1,offsetWidth-bankWidth)
		res
	}
//输入信号
	val io		 = IO(new ICacheBundle)
	val imaster   = IO(AXI4Bundle(CPUAXI4BundleParameters()))


	
	// val res_data  = IO(Output(Vec((bankNum),UInt(XLEN.W))))
	// val finish    = IO(Output(Bool()))
	

	val rdata        = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,UInt(XLEN.W)))))
	val rtag      = Wire(Vec(nWays,Vec(numReadport,UInt(tagWidth.W))))//numReadport -> cache line 
	val rvalid    = Wire(Vec(nWays,Vec(numReadport,Bool())))

	val s0_readport  = Wire(Vec(numReadport,Valid(new ICacheReq())))
	
	val s0_valid = io.req.fire
	dontTouch(s0_valid)
	val s0_vaddr = io.req.bits.raddr
	val s0_msg   = Wire(Vec(numReadport,Valid(new CacheMsg())))
	
	val s0_bankmask     = Wire(UInt(bankNum.W))//port0要访问的bank掩码

	//port0的访问首bank
	
	//得到每个way的重组data，也就是把两个访问数据合并
	val s1_wayData  = Wire(Vec(nWays,Vec(bankNum,UInt(XLEN.W))))
	val s1_msg     = RegNext(s0_msg)
	val s1_bankoff   = s1_msg(0).bits.bank
	val s1_bankmask= RegNext(s0_bankmask)
	val s1_valid   = RegNext(s0_valid,false.B)
	val s1_hitLine = VecInit((rtag zip rvalid).map{case(a,b)=>
	VecInit((a zip b zip s1_msg).map{ case((r,m),l)=>
		r===l.bits.tag&(RegNext(m)===true.B)
	})
	})//解释了每个port，每个way是否hit

	val t_hitLine = Transpose(s1_hitLine)//为Mux1H构造选择矩阵，第一维度是port，第二维度是nways
	//得到哪个way命中
	val s1_hitWay   = WireInit(Mux(t_hitLine(0).reduce(_|_),OHToUInt(t_hitLine(0).asUInt),OHToUInt(t_hitLine(1).asUInt)))

	//访问是否命中：只有两个访问都hit才会hit
	val s1_hit      = t_hitLine(0).reduce(_|_)&t_hitLine(1).reduce(_|_)
	
	val s1_missMsg0 = Wire(new MissMsg())
	val s1_missMsg1 = Wire(new MissMsg())

	val s1_hitData  = Wire(Vec(bankNum,UInt(XLEN.W)))//得到初步可能的数据


	val s2_missMsg0 = RegNext(s1_missMsg0)
	val s2_missMsg1 = RegNext(s1_missMsg1)
	val s2_valid  = RegNext(s1_valid && !io.s1_kill)
	val s2_hit 	  = RegNext(s1_hit)
	val s2_hitData= RegNext(s1_hitData)

	val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new DataRAM())))
	val tag       = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new TagRAM())))// each way has n(numReadport) ram//if numReadport==2,tag(i)(0)for port0
	val valid     =  RegInit(VecInit(Seq.fill(nWays)(
							VecInit(Seq.fill(numReadport)(
							VecInit(Seq.fill(nSets)(false.B))
							)))))
//random替换算法
	val rp = RegInit(0.U(log2Ceil(nWays).W))
	rp := rp + 1.U 
	dontTouch(s0_readport)
	s0_readport(0).valid        := (s0_valid)
	s0_readport(0).bits.raddr   := (s0_vaddr)
	s0_readport(1).valid        := ((!(s0_vaddr(offsetWidth-1,0)===0.U))&s0_valid)
	s0_readport(1).bits.raddr   := (s0_vaddr)
	for(i <- 0 until numReadport){
		s0_msg(i).bits := AddressSplitter(s0_readport(i).bits.raddr)
		s0_msg(i).valid:= s0_readport(i).valid 
	}
	s0_bankmask:= MaskUpper(UIntToOH(s0_msg(0).bits.bank))
//miss处理单元
	val missunit   = Module(new MissUnit())
//////////////////////REFILL SIGNAL/////////////////////////////
	//处理缺失完成数据取回来
	val refilled   = WireInit(missunit.finish)
	dontTouch(refilled)
	val refillData = WireInit(missunit.refillData) 
	val refillWay  = Wire(UInt(log2Ceil(nWays).W))// 要替换的Way
	val refillMask = WireInit(missunit.refillMask)//哪个readport发生缺失

	io.req.ready := !refilled//防止同读同写
////////////////////////////////////////////////////////////////
	require(nWays==2,"Now Only Support 2 ways assoc")
	require(numReadport==2,"Now Only Support 2 ports")
	val shiftArr = Wire(Vec(nWays,Vec(bankNum,Vec(bankNum,UInt(XLEN.W)))))
	//transpose arr to fit Mux1H
	//拼接两个port的数据
	for (i <- 0 until nWays) {
		val newarr = (0 until bankNum).map { j =>
			val shift = ((0 until bankNum).map { k =>
				if (j + k < bankNum) (rdata(0)(i)(j + k)) else (rdata(1)(i)((j + k) % bankNum))
			})
			VecInit(shift)
		}
		shiftArr(i) := Transpose((VecInit(newarr)))
	}

	for(i<- 0 until nWays){
		for(j<- 0 until bankNum){
			s1_wayData(i)(j) := Mux1H(UIntToOH(s1_bankoff),shiftArr(i)(j))
		}
	}
	//get data table
	//得到查找矩阵，
	val t_wayData = Transpose(s1_wayData)

	dontTouch(s1_wayData)
	// get sel signal(way) for each bank
	//拼接每个way数据
	for(i <- 0 until bankNum){
		val dataSel = Mux(s1_bankmask(i)===1.U,t_hitLine(0),t_hitLine(1))
		s1_hitData(i) := Mux1H(dataSel zip t_wayData(i))
	}
	



	s1_missMsg0.addr := Cat(s1_msg(0).bits.tag,s1_msg(0).bits.index)<<offsetWidth
	s1_missMsg1.addr := Cat(s1_msg(1).bits.tag,s1_msg(1).bits.index)<<offsetWidth
	s1_missMsg0.miss := (!t_hitLine(0).reduce(_|_))&(s1_msg(0).valid)
	s1_missMsg1.miss := Mux(s1_msg(1).valid,!t_hitLine(1).reduce(_|_),false.B)	//如果访问的地址是cache block对齐的，那末port不会访问，也就是miss信息无效
	assert(((!s1_msg(1).valid)&(s1_msg(0).bits.offset===0.U)),"port0 addr fail")


//////////////////////pipe2////////////////////
	//揭示正在处理缺失
	val refillingMsg0  = RegInit(s1_missMsg0)
	val refillingMsg1  = RegInit(s1_missMsg1)


	when(refilled){
		refillingMsg0.miss := false.B
		refillingMsg1.miss := false.B
	}.elsewhen(refillingMsg0.miss|refillingMsg0.miss){
		refillingMsg0 := refillingMsg0
		refillingMsg1 := refillingMsg0
	}.elsewhen(s2_missMsg0.miss|s2_missMsg1.miss){
		refillingMsg0 := s2_missMsg0
		refillingMsg1 := s2_missMsg1
	}
	// if port0 or port1 miss save data partial data miss
//refill tag和index的第0个元素总是来自第一个miss的数据
	val refill_tag = Wire(Vec(numReadport,UInt(tagWidth.W)))
	val refill_idx = Wire(Vec(numReadport,UInt(tagWidth.W)))
	refill_tag(0)	:= Mux(refillingMsg0.miss,refillingMsg0.addr>>(offsetWidth+indexWidth),
					refillingMsg1.addr>>(offsetWidth+indexWidth))			
	refill_tag(1)	:= refillingMsg1.addr>>(offsetWidth+indexWidth)	

	refill_idx(0)	:= Mux(refillingMsg0.miss,refillingMsg0.addr((indexWidth+offsetWidth)-1,offsetWidth),
					refillingMsg1.addr((indexWidth+offsetWidth)-1,offsetWidth))
	refill_idx(1)	:= refillingMsg1.addr((indexWidth+offsetWidth)-1,offsetWidth)
	refillWay := rp

	missunit.missmsg0.miss := s2_missMsg0.miss&s2_valid&(!refillingMsg0.miss)
	missunit.missmsg0.addr := s2_missMsg0.addr
	missunit.missmsg1.addr := s2_missMsg1.addr
	missunit.missmsg1.miss := s2_missMsg1.miss&s2_valid&&(!refillingMsg1.miss)
	missunit.imaster  <> imaster
/////////////////////////////OUTPUT////////////////////////////////
	io.resp.valid      := s2_valid&s2_hit
	io.resp.bits.data  := s2_hitData
///////////////////////////////////////////////////////////////////
//////////////////////////Tag RAM//////////////////////////////////
	for( i <- 0 until tag.length){
		for(j  <- 0 until tag.head.length){
			val tag_idx = Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),s0_msg(j).bits.index)
			tag(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&refilled&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,
									s0_msg(j).valid))
			tag(i)(j).io.addr   := Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),s0_msg(j).bits.index)
			tag(i)(j).io.write  :=  Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,false.B))
			tag(i)(j).io.dataIn := Mux((i.U===refillWay)&(refilled),refill_tag(0),refill_tag(1)) 
			rtag(i)(j)  := tag(i)(j).io.dataOut
			rvalid(i)(j):= valid(i)(j)(tag_idx(log2Ceil(nSets)-1,0))
			when(tag(i)(j).io.write){
				valid(i)(j)(tag_idx(log2Ceil(nSets)-1,0)) :=  true.B
			}
		}
	}
////////////////////////////////DATA RAM////////////////////////////
	for( i <- 0 until data.length){
		for(j  <- 0 until data.head.length){
			data(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,
									Mux(s0_bankmask(j)===1.U,s0_msg(0).valid,
									s0_msg(1).valid)))

			data(i)(j).io.addr   := Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),
									Mux(s0_bankmask(j)===1.U,s0_msg(0).bits.index,
									s0_msg(1).bits.index))

			data(i)(j).io.write  := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,false.B))
			data(i)(j).io.dataIn := Mux(refilled,Mux(i.U===refillWay,refillData(0)(j),refillData(1)(j)),0.U)//refillData(0) must be the first miss data

			rdata(0)(i)(j)       := Mux(s0_bankmask(j)===1.U,data(i)(j).io.dataOut,0.U)
			rdata(1)(i)(j) 		 := Mux(s0_bankmask(j)===1.U,0.U,data(i)(j).io.dataOut)
		}
	}

	dontTouch(rdata)
}

