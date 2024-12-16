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
	val bankNum     = blockBytes/(XLEN/8)
	val indexWidth  = log2Ceil(nSets)
	val offsetWidth = log2Ceil(blockBytes)
	val tagWidth    = XLEN-indexWidth-offsetWidth
	val bankWidth   = log2Ceil(blockBytes/bankNum)
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
    val tag    = UInt((tagWidth).W)
    val index  = UInt((indexWidth).W)
    val offset = UInt((offsetWidth).W)
    val bank   = UInt((bankWidth).W)
}
class ICacheWrapper(implicit p: Parameters) extends LazyModule with HasICacheParameters{
    val masterNode = AXI4MasterNode(Seq(
    AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
        name = "ICache")))))
    lazy val module = new Impl
    class Impl extends LazyModuleImp(this){
        val (master, _) = masterNode.out(0)
        val icache = Module(new ICache)
        val io     = IO(new ICacheBundle)
        io <> icache.io
        icache.imaster <>master
		override def toString: String = GRVString(
		"==L1-ICache==",
		"Fetch bytes   : " + fetchBytes,
		"Block bytes   : " + blockBytes,
		"Row bits      : " + rowBits,
		"Word bits     : " + XLEN,
		"Sets          : " + nSets,
		"Ways          : " + nWays,
		"RAMs          : (" +  blockBytes/bankNum + " x " + nSets + ") using " + bankNum + " banks"+"\n")
    }
}
class MissUnit(implicit p:Parameters) extends  GRVModule with HasICacheParameters{
	val missmsg      = IO(Input(new MissMsg()))
	val refillData    = IO(Output(Vec(bankNum,UInt(XLEN.W))))
	val finish        = IO(Output(Bool()))
	val imaster   	  = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val miss      = missmsg.miss
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
	val s_refillData = Reg(Vec(bankNum,UInt(XLEN.W)))
	// val s_refillMask	 = RegInit(0.U(numReadport.W))//哪一个port发生miss
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
		ar.addr  := missmsg.addr//Mux(missmsg0.miss,,missmsg1.addr)
		ar.len   := (blockBytes/4-1).U//Mux(missmsg1.miss&missmsg0.miss,(2*blockBytes/4-1).U,)
		ar.size  := "b010".U
		ar.burst := "b00".U
		ar.lock  := DontCare
		ar.cache := DontCare
		ar.prot  := DontCare
		ar.qos   := DontCare
		rcnt := 0.U
		arvalid := true.B
		// s_refillMask := Cat(missmsg1.miss,missmsg0.miss).asUInt
	}.elsewhen(ar_fire){
		arvalid := false.B
		rready := true.B
	}.elsewhen(imaster.r.bits.last){
		rready := false.B
	}

	when(state===s_wait_ack&imaster.r.fire){
		s_refillData((rcnt%(bankNum.U))(log2Ceil(bankNum)-1,0)) := imaster.r.bits.data
		rcnt := rcnt + 1.U
	}

	when(state===s_wait_ack&state_n===s_end){
		end := true.B
	}.otherwise{
		end :=false.B
	}
	refillData := s_refillData
	finish := end
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


/* 
ICache逻辑部分面积大概6400，MISSUNIT 为2700，之前将valid设置为regfile，导致面积为14000，差不多1bit的触发器面积为9
去掉跨行逻辑部分面积为2300，MISSUNIT为1500
 */
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


	// println(f"tag $tagWidth $bankWidth $offsetWidth $indexWidth")
	// val res_data  = IO(Output(Vec((bankNum),UInt(XLEN.W))))
	// val finish    = IO(Output(Bool()))
	

	val rdata     = Wire(Vec(nWays,Vec(bankNum,UInt(XLEN.W))))
	val rtag      = Wire(Vec(nWays,UInt(tagWidth.W)))
	val rvalid    = Wire(Vec(nWays,Bool()))

	val s0_readport  = Wire(Valid(new ICacheReq()))
	
	val s0_valid = io.req.fire
	dontTouch(s0_valid)
	val s0_vaddr = io.req.bits.raddr
	val s0_msg   = Wire(Valid(new CacheMsg()))
	
	val s0_bankmask     = Wire(UInt(bankNum.W))//port0要访问的bank掩码

	//port0的访问首bank
	

	val s1_msg     = RegNext(s0_msg)
	val s1_bankoff   = s1_msg.bits.bank
	val s1_bankmask= RegNext(s0_bankmask)
	val s1_valid   = RegNext(s0_valid,false.B)
	val s1_hitWay = VecInit((rtag zip rvalid).map{case(a,b)=>
		a===s1_msg.bits.tag&(b===true.B)//
	})//解释了每个port，每个way是否hit



	//访问是否命中：只有两个访问都hit才会hit
	val s1_hit      = s1_hitWay.reduce(_||_)
	
	val s1_missMsg = Wire(new MissMsg())

	val s1_hitData  = Wire(Vec(bankNum,UInt(XLEN.W)))//得到初步可能的数据


	val s2_missMsg = RegNext(s1_missMsg)

	val s2_valid  = RegNext(s1_valid && !io.s1_kill)
	val s2_hit 	  = RegNext(s1_hit)
	val s2_hitData= RegNext(s1_hitData)

	val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new SRAMHelper(nSets,UInt(XLEN.W)))))
	val tag       = Seq.fill(nWays)(Module(new SRAMHelper(nSets,UInt(tagWidth.W))))
	val valid     = Seq.fill(nWays)(Module(new SRAMHelper(nSets,Bool())))
							
//random替换算法
	val rp = RegInit(0.U(log2Ceil(nWays).W))
	rp := rp + 1.U 
	dontTouch(s0_readport)
	s0_readport.valid        := (s0_valid)
	s0_readport.bits.raddr   := (s0_vaddr)


	s0_msg.bits := AddressSplitter(s0_readport.bits.raddr)
	s0_msg.valid:= s0_readport.valid 

	s0_bankmask:= MaskUpper(UIntToOH(s0_msg.bits.bank))
//miss处理单元
	val missunit   = Module(new MissUnit())
//////////////////////REFILL SIGNAL/////////////////////////////
	//处理缺失完成数据取回来
	val refilled   = WireInit(missunit.finish)
	dontTouch(refilled)
	val refillData = WireInit(missunit.refillData) 
	val refillWay  = Wire(UInt(log2Ceil(nWays).W))// 要替换的Way

	io.req.ready := !refilled//防止同读同写
////////////////////////////////////////////////////////////////
	require(nWays==2,"Now Only Support 2 ways assoc")


	s1_hitData := rdata(PriorityEncoder(s1_hitWay.asUInt))
	s1_missMsg.addr := Cat(s1_msg.bits.tag,s1_msg.bits.index)<<offsetWidth
	s1_missMsg.miss := (!s1_hit)&(s1_msg.valid)
	when(s1_valid){
		assert(((s1_msg.bits.offset(1,0)===0.U)),"port0 Misalign Access")
	}
	// assert(((s1_msg(0).valid)&(s1_msg(0).bits.offset(1,0)===0.U)),"port0 Misalign Access")

//////////////////////pipe2////////////////////
	//揭示正在处理缺失
	val refillingMsg  = Reg(new MissMsg())



	when(refilled){
		refillingMsg.miss := false.B
	}.elsewhen(refillingMsg.miss){
		refillingMsg := refillingMsg
	}.elsewhen((s2_missMsg.miss)&s2_valid){
		refillingMsg := s2_missMsg

	}.otherwise{
		refillingMsg.miss := false.B

	}
	// if port0 or port1 miss save data partial data miss
//refill tag和index的第0个元素总是来自第一个miss的数据
	val refill_tag = Wire(UInt(tagWidth.W))
	val refill_idx = Wire(UInt(tagWidth.W))
	dontTouch(refill_tag)
	dontTouch(refill_idx)
	refill_tag	:= refillingMsg.addr>>(offsetWidth+indexWidth)						
	refill_idx	:= refillingMsg.addr((indexWidth+offsetWidth)-1,offsetWidth)
	refillWay := rp

	missunit.missmsg.miss := s2_missMsg.miss&s2_valid&(!refillingMsg.miss)
	missunit.missmsg.addr := s2_missMsg.addr

	missunit.imaster  <> imaster
/////////////////////////////OUTPUT////////////////////////////////
	io.resp.valid      := s2_valid&s2_hit
	io.resp.bits.data  := s2_hitData
///////////////////////////////////////////////////////////////////
//////////////////////////Tag RAM//////////////////////////////////
	for( i <- 0 until tag.length){
		val tag_idx = Mux(refilled,refill_idx,s0_msg.bits.index)
		tag(i).io.enable := refilled&&i.U===refillWay||s0_msg.valid
		tag(i).io.addr   := Mux(refilled,refill_idx,s0_msg.bits.index)
		tag(i).io.write  := refilled&&i.U===refillWay
		tag(i).io.dataIn := refill_tag
		valid(i).io.enable  := tag(i).io.enable 
		valid(i).io.addr    := tag(i).io.addr   
		valid(i).io.write   := tag(i).io.write 
		valid(i).io.dataIn  := true.B
		rtag(i)  := tag(i).io.dataOut
		rvalid(i):= valid(i).io.dataOut
		
	}
	dontTouch(rtag)
	dontTouch(rvalid)
////////////////////////////////DATA RAM////////////////////////////
	for( i <- 0 until data.length){
		for(j  <- 0 until data.head.length){
			val enable = (i.U===refillWay)&&(refilled)||s0_msg.valid&&(s0_bankmask(j)===1.U)
			val write  = (i.U===refillWay)&&(refilled)
			data(i)(j).io.enable := enable
			data(i)(j).io.addr   := Mux(refilled,refill_idx,s0_msg.bits.index)
			data(i)(j).io.write  := write
			data(i)(j).io.dataIn := refillData(j)
			rdata(i)(j)       := Mux(s0_bankmask(j)===1.U,data(i)(j).io.dataOut,0.U)
			
		}
	}

	dontTouch(rdata)

}

