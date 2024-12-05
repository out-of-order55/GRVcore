package grvcore
import chisel3._
import chisel3.util._
// import scala.util.matching.Regex
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

	val miss = missmsg0.miss|missmsg1.miss
	//normal->no miss req->has miss  wait_ack->wait data end->receive data
	val s_reset :: s_normal::s_req :: s_wait_ack :: s_end :: Nil = Enum(5)
	val state = RegNext(s_reset)
	val state_n = WireInit(state)
	val s_refillData = Reg(Vec(numReadport,Vec(bankNum,UInt(XLEN.W))))
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

	}.elsewhen(ar_fire){
		arvalid := false.B
		rready := true.B
	}.elsewhen(r.last){
		rready := false.B
	}

	when(state===s_wait_ack&imaster.r.fire){
		s_refillData(rcnt/(blockBytes.U))(rcnt%(blockBytes.U)) := r.data
		rcnt := rcnt + 1.U
	}

	when(state===s_normal){
		end := false.B
	}.elsewhen(state===s_wait_ack&state_n===s_end){
		end := true.B
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

	def AddressSplitter(readport:CacheReadIO)(implicit p:Parameters)={
		val res = Wire(new CacheMsg())
		res.tag  := (readport.raddr(XLEN-1,(indexWidth+offsetWidth)))
		res.index:= (readport.raddr((indexWidth+offsetWidth)-1,offsetWidth))
		res.offset := readport.raddr((offsetWidth)-1,0)
		res.bank := readport.raddr(offsetWidth-1,offsetWidth-bankWidth)
		res.ren  := readport.ren
		res
	}

	val readport = Seq.fill(numReadport)(IO(new CacheReadIO()))
	val res_data  = IO(Output(Vec((bankNum),UInt(XLEN.W))))
	val finish    = IO(Output(Bool()))
	// val imaster   = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new DataRAM())))
	val tag       = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new TagRAM())))// each way has n(numReadport) ram//if numReadport==2,tag(i)(0)for port0
	

//random replacement
	val rp = RegInit(false.B)
	rp := (!rp) 

	require(nWays==2,"Now Only Support 2 ways assoc")
	require(numReadport==2,"Now Only Support 2 ports")


	val port = VecInit(readport.map{ a =>AddressSplitter(a)})


	val rdata     = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,UInt(XLEN.W)))))

	val rmask     = Wire(Vec(numReadport,UInt(bankNum.W)))
	rmask(0) := MaskUpper(UIntToOH(port(0).bank))
	rmask(1) := !rmask(0)

	val rtagv     = Wire(Vec(nWays,Vec(numReadport,UInt(XLEN.W))))//numReadport -> cache line 

// access tag
	(0 until tag.length).map{i=>
		(0 until tag.head.length).map{j=>
			tag(i)(j).io.enable := port(j).ren
			tag(i)(j).io.addr   := port(j).index
			tag(i)(j).io.write  := false.B
			tag(i)(j).io.dataIn := 0.U
			rtagv(i)(j) := tag(i)(j).io.dataOut 
		}
	}
//access data
  //each port need connect both dataram , no bank conflict
    (0 until data.length).map{j=>
		(0 until data.head.length).map{k=>
			when(rmask(0)(k)===1.U){
				data(j)(k).io.enable := port(0).ren
				data(j)(k).io.addr   := port(0).index
				data(j)(k).io.write  := false.B
				data(j)(k).io.dataIn := 0.U
				rdata(0)(j)(k)       := data(j)(k).io.dataOut
				rdata(1)(j)(k) 		 := DontCare		
        	}.otherwise{
				data(j)(k).io.enable := port(1).ren
				data(j)(k).io.addr   := port(1).index
				data(j)(k).io.write  := false.B
				data(j)(k).io.dataIn := 0.U
				rdata(0)(j)(k)       := DontCare
				rdata(1)(j)(k) 		 := data(j)(k).io.dataOut
        	}
        
    	}
    }
	dontTouch(rdata)

//reg1
	val port_r1     = RegNext(port)
	val rmask_r1    = RegNext(rmask)
//pipe2 generate hit and miss res

  //if 2 way ,need judge most 4 cacheline 
  //(nWays)(numPort)
	val hitLine  = rtagv.map{i=>
		(i zip port_r1).map{ case(r,l)=>
			r===l.tag
		}
	}
	val hitWay    = (hitLine.map{
		h => h.reduce(_&_)
	})
	val hit       = hitWay.reduce(_|_)


	val bankoff   = port_r1(0).bank
	val bankmask  = rmask_r1(0)


	val wayData  = Wire(Vec(nWays,Vec(bankNum,UInt(XLEN.W))))
	dontTouch(wayData)
	val hitData  = Mux1H(hitWay zip wayData)

	val miss_addr0 = Wire(UInt(XLEN.W))
	val miss_addr1 = Wire(UInt(XLEN.W))
	miss_addr0 := Cat(port_r1(0).tag,(port_r1(0).index))<<offsetWidth
	miss_addr1 := Cat(port_r1(1).tag,(port_r1(1).index))<<offsetWidth


val shiftArr = Wire(Vec(nWays,Vec(bankNum,Vec(bankNum,UInt(XLEN.W)))))

//transpose arr to fit Mux1H
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

//reg2
	val hit_r2       = RegNext(hit)
	val hit_data_r2  = RegNext(hitData)
	val miss_addr0_r2 = RegNext(miss_addr0)
	val miss_addr1_r2 = RegNext(miss_addr1)

  //both 0 and 1 need to be hit or into missunit
//pipe3
  //if hit 
  // val defd =VecInit(bankNum,(0.U(XLEN.W)))
	res_data:=Mux(hit_r2,hit_data_r2,wayData(0))
	finish := Mux(hit_r2,true.B,false.B)
  // if miss
  // val missunit = Module(new MissUnit(addrWidth,dataWidth,parameter))
}