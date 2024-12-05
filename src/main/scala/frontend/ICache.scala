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
class MissMsg(implicit  p:Parameters) extends GRVBundle with HasICacheParameters{
    val miss   = (Bool())
    val addr   = (UInt(XLEN.W))
}
class CacheMsg(implicit p:Parameters)extends GRVBundle with HasICacheParameters{
    val tag    = UInt(tagWidth.W)
    val index  = UInt(indexWidth.W)
    val offset = UInt(offsetWidth.W)
    val bank   = UInt(bankWidth.W)
    val ren    = (Bool())
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

class MissUnit(implicit p:Parameters) extends  GRVModule with HasICacheParameters{
	val missmsg0      = IO(Input(new MissMsg()))
	val missmsg1      = IO(Input(new MissMsg()))
	val refillData    = IO(Output(Vec(numReadport,Vec(bankNum,UInt(XLEN.W)))))
	val finish        = IO(Output(Bool()))
	val imaster   	  = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val refillMask    = IO(Output(UInt(numReadport.W)))
	val miss = missmsg0.miss|missmsg1.miss
	//normal->no miss req->has miss  wait_ack->wait data end->receive data
	val s_reset :: s_normal::s_req :: s_wait_ack :: s_end :: Nil = Enum(5)
	val state = RegNext(s_reset)
	val state_n = WireInit(state)
	val s_refillData = Reg(Vec(numReadport,Vec(bankNum,UInt(XLEN.W))))
	val s_refillMask	 = RegInit(0.U(numReadport.W))
////////////////////////AXI//////////////////////////
	val ar_fire = imaster.ar.fire
	val r_fire 	= imaster.r.fire

	imaster.aw := DontCare
	imaster.w  := DontCare
	imaster.b  := DontCare
	
	val ar = Reg(new AXI4BundleAR(CPUAXI4BundleParameters()))
	val r  = Reg(new AXI4BundleR(CPUAXI4BundleParameters()))
	ar <> imaster.ar.bits
	r  <> imaster.r.bits

	val s1_missMsg0 = Reg(new MissMsg())
	val s1_missMsg1 = Reg(new MissMsg())
	val end 		= Reg(Bool())
	val rcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))
	val rready 		= Reg(Bool())
	val arvalid		= RegInit(false.B)
	imaster.r.ready := rready
	imaster.ar.valid:= arvalid
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
		s1_missMsg0  := missmsg0
		s1_missMsg1  := missmsg1
		rcnt := 0.U
		arvalid := true.B
		s_refillMask := Cat(missmsg1.miss,missmsg0.miss).asUInt
	}.elsewhen(ar_fire){
		arvalid := false.B
		rready := true.B
	}.elsewhen(r.last){
		rready := false.B
	}

	when(state===s_wait_ack&imaster.r.fire){
		s_refillData((rcnt/(blockBytes.U))(log2Ceil(numReadport)-1,0))(rcnt%(blockBytes.U)(log2Ceil(bankNum)-1,0)) := r.data
		rcnt := rcnt + 1.U
	}

	when(state===s_normal){
		end := false.B
	}.elsewhen(state===s_wait_ack&state_n===s_end){
		end := true.B
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
			when(r_fire&r.last){
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
	def AddressSplitter(readport:CacheReadIO)(implicit p:Parameters)={
		val res = Wire(new CacheMsg())
		res.tag  := (readport.raddr(XLEN-1,(indexWidth+offsetWidth)))
		res.index:= (readport.raddr((indexWidth+offsetWidth)-1,offsetWidth))
		res.offset := readport.raddr((offsetWidth)-1,0)
		res.bank := readport.raddr(offsetWidth-1,offsetWidth-bankWidth)
		res.ren  := readport.ren
		res
	}
//输入信号
	val readport = Seq.fill(numReadport)(IO(new CacheReadIO()))
	val res_data  = IO(Output(Vec((bankNum),UInt(XLEN.W))))
	val finish    = IO(Output(Bool()))
	val imaster   = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new DataRAM())))
	val tag       = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new TagRAM())))// each way has n(numReadport) ram//if numReadport==2,tag(i)(0)for port0
	
//random替换算法
	val rp = RegInit(0.U(log2Ceil(nWays).W))
	rp := rp + 1.U 
//miss处理单元
	val missunit   = Module(new MissUnit())
//////////////////////REFILL SIGNAL/////////////////////////////
	//处理缺失完成数据取回来
	val refill_end = WireInit(missunit.finish)
	val refillData = WireInit(missunit.refillData) 
	val refillWay  = Wire(UInt(log2Ceil(nWays).W))// 要替换的Way
	val refillMask = WireInit(missunit.refillMask)//哪个readport发生缺失
////////////////////////////////////////////////////////////////

	require(nWays==2,"Now Only Support 2 ways assoc")
	require(numReadport==2,"Now Only Support 2 ports")

	val port = VecInit(readport.map{ a =>AddressSplitter(a)})

	//读数据
	val rdata     = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,UInt(XLEN.W)))))
	//读掩码，只有掩码为1才会去读
	val rmask     = Wire(Vec(numReadport,UInt(bankNum.W)))
	rmask(0) := MaskUpper(UIntToOH(port(0).bank))
	rmask(1) := !rmask(0)

	val rtagv     = Wire(Vec(nWays,Vec(numReadport,UInt(tagWidth.W))))//numReadport -> cache line 



//reg1
	val port_r1     = RegNext(port)
	val rmask_r1    = RegNext(rmask)
//pipe2 generate hit and miss res

  //判断是否hit，
  //(nWays)(numPort)
	val hitLine  = VecInit(rtagv.map{i=>
		VecInit((i zip port_r1).map{ case(r,l)=>
			r===l.tag
		})
	})//解释了每个port，每个way是否hit
	val t_hitLine = Transpose(hitLine)//为Mux1H构造选择矩阵，第一维度是port，第二维度是nways
	//得到哪个way命中
	val hitWay   = WireInit(Mux(t_hitLine(0).reduce(_|_),OHToUInt(t_hitLine(0).asUInt),OHToUInt(t_hitLine(1).asUInt)))

	//访问是否命中：只有两个访问都hit才会hit
	val hit       = t_hitLine(0).reduce(_|_)&t_hitLine(1).reduce(_|_)

	//port0的访问首bank
	val bankoff   = port_r1(0).bank
	//port0要访问的bank掩码
	val bankmask  = rmask_r1(0)

	//得到每个way的重组data，也就是把两个访问数据合并
	val wayData  = Wire(Vec(nWays,Vec(bankNum,UInt(XLEN.W))))
	//get data table
	//得到查找矩阵，
	val t_wayData = Transpose(wayData)
	val hitData  = Wire(Vec(bankNum,UInt(XLEN.W)))//得到初步可能的数据
	val dataMask = Wire(Vec(bankNum,Bool()))//保存下哪些bank命中，因为可能port0命中，但port1未命中
	dontTouch(wayData)
	// get sel signal(way) for each bank
	//拼接每个way数据
	for(i <- 0 until bankNum){
		val dataSel = Mux(bankmask(i)===1.U,t_hitLine(0),t_hitLine(1))
		hitData(i) := Mux1H(dataSel zip t_wayData(i))
		when(dataSel.reduce(_|_)){
			dataMask(i) := true.B
		}.otherwise{
			dataMask(i)	:= false.B
		}
	}
	


	val missMsg0 = Wire(new MissMsg())
	val missMsg1 = Wire(new MissMsg())
	missMsg0.addr := Cat(port_r1(0).tag,(port_r1(0).index))<<offsetWidth
	missMsg1.addr := Cat(port_r1(1).tag,(port_r1(1).index))<<offsetWidth
	missMsg0.miss := !t_hitLine(0).reduce(_|_)
	missMsg1.miss := Mux(port_r1(1).ren,!t_hitLine(1).reduce(_|_),false.B)	//如果访问的地址是cache block对齐的，那末port不会访问，也就是miss信息无效
	assert(((!port_r1(1).ren)&(port_r1(0).offset===0.U)),"port0 addr fail")
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
			wayData(i)(j) := Mux1H(UIntToOH(bankoff),shiftArr(i)(j))
		}
	}

//////////////////////pipe2////////////////////
	val hit_r2       = RegNext(hit)
	val hit_data_r2  = RegNext(hitData)
	val missMsg0_r2  = RegInit(missMsg0)
	val missMsg1_r2  = RegInit(missMsg1)
	val hitWay_r2    = Reg(UInt(log2Ceil(nWays).W))
	val hitData_r2	 = Reg(Vec(bankNum,UInt(XLEN.W)))//为refill做准备
	val dataMask_r2  = Reg(Vec(bankNum,Bool()))
	val bankMask_r2	 = Reg(UInt(bankNum.W))
	when(refill_end){
		missMsg0_r2.miss := false.B
		missMsg1_r2.miss := false.B
	}.elsewhen(missMsg0.miss|missMsg1.miss){
		missMsg0_r2 := missMsg0
		missMsg1_r2 := missMsg1
	}
	// if port0 or port1 miss save data partial data miss
	when(missMsg0.miss^missMsg1.miss){
		hitData_r2 := hitData
		dataMask_r2 := dataMask
		bankMask_r2 := bankmask
		hitWay_r2	:= hitWay
	}

  //both 0 and 1 need to be hit or into missunit
///////////////////////////pipe3///////////////////////
  //if hit 
  // val defd =VecInit(bankNum,(0.U(XLEN.W)))
///////////////////////////MISS HANDLE/////////////////////////////
	val finalData = Wire(Vec(bankNum,UInt(XLEN.W)))
	val finalArr = Wire(Vec(bankNum,Vec(bankNum,UInt(XLEN.W))))
	val newarr = (0 until bankNum).map { j =>
		val shift = ((0 until bankNum).map { k =>
			if (j + k < bankNum) (refillData(0)(j + k)) else (refillData(1)((j + k) % bankNum))
		})
		VecInit(shift)
	}
	//最后的refill数据，需要考虑port0，port1
	//						 miss   hit 此时需要拼接之前的hitdata数据
	///						 hit    miss同上
	//						 miss   miss此时直接拼接missunit模块的数据
	finalArr := Transpose((VecInit(newarr)))
	for(i <- 0 until bankNum){
		finalData(i) := Mux(dataMask_r2(i),hitData_r2(i),Mux1H(UIntToOH(bankoff),finalArr(i)))
	}

//refill tag和index的第0个元素总是来自第一个miss的数据
	val refill_tag = Wire(Vec(numReadport,UInt(tagWidth.W)))
	val refill_idx = Wire(Vec(numReadport,UInt(tagWidth.W)))
	refill_tag(0)	:= Mux(missMsg0_r2.miss,missMsg0_r2.addr>>(offsetWidth+indexWidth),
					missMsg1_r2.addr>>(offsetWidth+indexWidth))			
	refill_tag(1)	:= missMsg1_r2.addr>>(offsetWidth+indexWidth)	

	refill_idx(0)	:= Mux(missMsg0_r2.miss,missMsg0_r2.addr((indexWidth+offsetWidth)-1,offsetWidth),
					missMsg1_r2.addr((indexWidth+offsetWidth)-1,offsetWidth))
	refill_idx(1)	:= missMsg1_r2.addr((indexWidth+offsetWidth)-1,offsetWidth)
	refillWay := rp

	missunit.missmsg0 := missMsg0_r2
	missunit.missmsg1 := missMsg1_r2
	missunit.imaster  <> imaster
/////////////////////////////OUTPUT////////////////////////////////
//得到缺失数据的下个周期才会返回ack
	val data_ok = RegNext(refill_end)
	val finalData_s1 = RegNext(finalData)
	finish := Mux(hit_r2,true.B,
				Mux(data_ok,true.B,false.B))
	res_data:=Mux(hit_r2,hit_data_r2,
				Mux(data_ok,finalData_s1,
				VecInit(Seq.fill(bankNum)(0.U(XLEN.W)))))
///////////////////////////////////////////////////////////////////
//////////////////////////Tag RAM//////////////////////////////////
	for( i <- 0 until tag.length){
		for(j  <- 0 until tag.head.length){
			tag(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&refill_end&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refill_end)&(refillMask.xorR===true.B),true.B,
									Mux(port(j).ren,true.B,false.B)))
			tag(i)(j).io.addr   := Mux(refill_end,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),port(j).index)
			tag(i)(j).io.write  :=  Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refill_end)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refill_end)&(refillMask.xorR===true.B),true.B,false.B))
			tag(i)(j).io.dataIn := Mux((i.U===refillWay)&(refill_end),refill_tag(0),refill_tag(1)) 
			rtagv(i)(j) := tag(i)(j).io.dataOut
		}
	}
////////////////////////////////DATA RAM////////////////////////////
	for( i <- 0 until data.length){
		for(j  <- 0 until data.head.length){
			data(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refill_end)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refill_end)&(refillMask.xorR===true.B),true.B,
									Mux(rmask(0)(j)===1.U,port(0).ren,
									port(1).ren)))

			data(i)(j).io.addr   := Mux(refill_end,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),
									Mux(rmask(0)(j)===1.U,port(0).index,
									port(1).index))

			data(i)(j).io.write  := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refill_end)&(refillMask.xorR===false.B),true.B,
									Mux((i.U===refillWay)&(refill_end)&(refillMask.xorR===true.B),true.B,false.B))
			data(i)(j).io.dataIn := Mux(refill_end,Mux(i.U===refillWay,refillData(0)(j),refillData(1)(j)),0.U)//refillData(0) must be the first miss data

			rdata(0)(i)(j)       := Mux(rmask(0)(j)===1.U,data(i)(j).io.dataOut,0.U)
			rdata(1)(i)(j) 		 := Mux(rmask(0)(j)===1.U,0.U,data(i)(j).io.dataOut)
		}
	}

	dontTouch(rdata)
}
//
