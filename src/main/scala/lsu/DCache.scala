package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// import freechips.rocketchip.util.Annotated.resetVector
import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config._
case object DCacheKey extends Field[DCacheParams]


case class DCacheParams(
    nSets: Int = 256,
    nWays: Int = 2,
    rowBits: Int = 128,
    numMSHRs:Int = 8,
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
trait HasDCacheParameters extends HasGRVParameters{
    val nSets        = DCacheParam.nSets      
    val nWays        = DCacheParam.nWays     
    val rowBits      = DCacheParam.rowBits   
    val prefetch     = DCacheParam.prefetch  
    val blockBytes   = DCacheParam.blockBytes
    val numMSHRs     = DCacheParam.numMSHRs


	val bankNum     = blockBytes/(XLEN/8)
	val indexWidth  = log2Ceil(nSets)
	val offsetWidth = log2Ceil(blockBytes)
	val tagWidth    = XLEN-indexWidth-offsetWidth
	val bankWidth   = log2Ceil(blockBytes/bankNum)
	val numReadport = 2
}
/* 

目前DCache采用单端口，不能同时读写同一bank，如果sw指令和ld指令发生冲突，replay ld指令

stage0

送入请求

stage1

对比tag

stage2

返回数据与命中信息
如果miss
分配MSHR

MSHR 设计规范：
1.一张MSHR表
LDQ会侦听地址，给出offset
store buffer 会给出offset

 */
class ReadReq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    
    val addr = UInt(XLEN.W)
}
class ReadResp(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val data = UInt(XLEN.W)
    val hit  = Bool()
}
class WriteReq(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val data = Vec(bankNum,UInt(XLEN.W))
    val addr = UInt(XLEN.W)
    val mask = Vec(bankWidth,UInt((XLEN/8).W))
}
class WriteResp(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val finish = Bool()
}

class DCacheReadIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val req = Flipped(Decoupled(new ReadReq))
    val resp= Valid(new ReadResp)
}
class DCacheWriteIO(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val req = Flipped(Decoupled(new WriteReq))
    val resp= Valid(new WriteResp)
}
class DCacheBundle(implicit p: Parameters) extends GRVBundle with HasDCacheParameters{
    val read = Vec(numReadport,new DCacheReadIO)
    val write = new DCacheWriteIO

    val ld_replay = Bool()
    val s1_kill = Bool()
    val s2_kill = Bool()
}

class MSHREntry(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val valid   = Bool()
    val data    = Vec(bankNum,UInt(XLEN.W))
    val addr    = UInt(XLEN.W)
    val mem_type= Bool()//true:load 
}
class RefillMsg(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val refill_addr   = UInt(XLEN.W)
    val refilled      = Bool()
    val refillData    = Vec(bankNum,UInt(XLEN.W))

} 
class DirtyMsg(implicit p: Parameters)extends GRVBundle with HasDCacheParameters{
    val addr        = UInt(XLEN.W)
    val valid       = Bool()
    val data        = Vec(bankNum,UInt(XLEN.W))

} 
class Uncache(implicit p:Parameters) extends  GRVModule with HasDCacheParameters{

    val mshrSz = log2Ceil(numMSHRs)
	val missmsg       = IO(Input(Vec(numReadport,new MissMsg())))
    val dirtymsg      = IO(Input(new DirtyMsg()))
	val refill        = IO(Output(new RefillMsg))
    val full          = IO(Output(Bool()))
	val master   	  = IO(AXI4Bundle(CPUAXI4BundleParameters()))
	val flush		  = IO(Input(Bool()))


    val reqData = Wire(Vec(numReadport+1,new MSHREntry))
    val mshrs = RegInit(VecInit.fill(numMSHRs)(0.U.asTypeOf(new MSHREntry)))
	mshrs.foreach{i=>
		i.valid := (!flush)
	}

    val enq_ptr  = RegInit(0.U(log2Ceil(numMSHRs+1).W))
    val deq_ptr  = RegInit(0.U(log2Ceil(numMSHRs+1).W))

    val enq_idxs = VecInit.tabulate(numReadport+1)(i => PopCount(req.take(i)))
    val enq_offset = VecInit(enq_idxs.map(_+enq_ptr(mshrSz-1,0)))
	val numEnq = PopCount(req)
	val numValid = PopCount(mshrs.map(_.valid))
	val enqNextPtr = enq_ptr + numEnq
	full := (enq_ptr(mshrSz)=/=deq_ptr(mshrSz)&&(numEnq+enq_ptr)(mshrSz-1,0)>deq_ptr(mshrSz-1,0))||
    numValid===numMSHRs.U
	val empty = (enq_ptr(mshrSz)=/=deq_ptr(mshrSz))&&enq_ptr(mshrSz-1,0)===deq_ptr(mshrSz-1,0)
	//normal->no miss req->has miss  wait_ack->wait data end->receive data
	/* 
	normal:		没有发生miss的状态	
	req:		生成miss的axi信号	
	wait_ack:	等待接受数据
	end:		看到last信号，缺失处理结束
	 */
	val s_reset :: s_normal::s_readreq ::s_writereq::s_wait_ack :: s_end :: Nil = Enum(5)
	val state = RegNext(s_reset)
	val state_n = WireInit(state)
	val send_req = (state===s_normal)&&(!empty)
    ///////////////////////////////////MSHR/////////////////////////////
    //write
    val req = WireInit(VecInit.fill(numReadport+1)(false.B))

    for(i <- 0 until numReadport){
        req(i)          := missmsg(i).miss
        reqData(i).addr := missmsg(i).addr
        reqData(i).data := 0.U
        reqData(i).mem_type := true.B
        reqData(i).valid:= missmsg(i).miss       

    }
    req(numReadport) := dirtymsg.valid
    reqData(numReadport).addr := dirtymsg.addr
    reqData(numReadport).data := dirtymsg.data
    reqData(numReadport).mem_type := false.B
    reqData(numReadport).valid:= dirtymsg.valid  

    /*
    请求数目不定如果想写入fifo，可以通过计数true+enq_Ptr
    */

    dontTouch(enq_idxs)
    dontTouch(enq_offset)
    for(i <- 0 until numReadport+1){
        for(j <- 0 until numMSHRs){
            when(req(i)&&enq_offset(i)===j.U){
                mshrs(j) := reqData(i)
            }
        }
    }
	when(flush){
		
		enq_ptr := 0.U
	}
	.elsewhen(req.reduce(_||_)){
		enq_ptr := enq_ptr + enqNextPtr
	}
	when(state===s_wait_ack&&state_n===s_end){
		mshrs(deq_ptr).valid := false.B
		deq_ptr := deq_ptr + 1.U
	}


	val reqMsg = mshrs(deq_ptr)
	val s_refillData = Reg(Vec(bankNum,UInt(XLEN.W)))
	// val s_refillMask	 = RegInit(0.U(numReadport.W))//哪一个port发生miss
////////////////////////AXI//////////////////////////
	val ar_fire = master.ar.fire
	val r_fire 	= master.r.fire
	val aw_fire = master.aw.fire
	val w_fire  = master.w.fire
	val b_fire  = master.b.fire
	// master.aw := DontCare
	// master.w  := DontCare
	// master.b  := DontCare
	
	val ar = Reg(new AXI4BundleAR(CPUAXI4BundleParameters()))
	val r  = Reg(new AXI4BundleR(CPUAXI4BundleParameters()))
	val aw = Reg(new AXI4BundleAW(CPUAXI4BundleParameters()))
	val w  = Reg(new AXI4BundleW(CPUAXI4BundleParameters()))
	val b  = Reg(new AXI4BundleB(CPUAXI4BundleParameters()))
	ar <> master.ar.bits
	aw <> master.aw.bits
	w  <> master.w.bits
	b  <> master.b.bits

	val rcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))
	val wcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))

	val rready 		= Reg(Bool())
	val arvalid		= RegInit(false.B)
	val awvalid 	= RegInit(false.B)
	val wvalid 		= RegInit(false.B)
	val bready 		= RegInit(false.B)
	val wdata	    = RegInit(reqMsg.data)
	master.r.ready := rready
	master.ar.valid:= arvalid
	master.aw.valid:= awvalid
	master.w.valid := wvalid
	master.b.ready := bready
	
	//生成read axi信号，默认一笔传输的数据大小为4字节，突发长度为blk_size或者2倍的blk
	when(state===s_normal&state_n===s_readreq){
		ar.id    := 1.U
		ar.addr  := reqMsg.addr//Mux(missmsg0.miss,,missmsg1.addr)
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
	}.elsewhen(master.r.bits.last){
		rready := false.B
	}
	when(state===s_wait_ack&master.r.fire){
		s_refillData((rcnt%(bankNum.U))(log2Ceil(bankNum)-1,0)) := master.r.bits.data
		rcnt := rcnt + 1.U
	}

	refill.refillData := s_refillData
	refill.refill_addr:= reqMsg.addr
	refill.refilled	  := state===s_end
	//
	//生成write axi信号，默认一笔传输的数据大小为4字节，突发长度为blk_size或者2倍的blk
	when(state===s_normal&state_n===s_writereq){
		aw.id    := 1.U
		aw.addr  := reqMsg.addr//Mux(missmsg0.miss,,missmsg1.addr)
		aw.len   := (blockBytes/4-1).U//Mux(missmsg1.miss&missmsg0.miss,(2*blockBytes/4-1).U,)
		aw.size  := "b010".U
		aw.burst := "b00".U
		aw.lock  := DontCare
		aw.cache := DontCare
		aw.prot  := DontCare
		aw.qos   := DontCare
		w.strb 	 := "b1111".U
		awvalid := true.B
		wvalid 	:= true.B
		// s_refillMask := Cat(missmsg1.miss,missmsg0.miss).asUInt
	}.elsewhen(aw_fire){
		awvalid := false.B
		
	}.elsewhen(w_fire){
		wvalid := false.B
	}
	when(state===s_wait_ack&master.w.fire){
		master.w.bits.data := wdata(wcnt)
		
	}
	awvalid := Mux(state===s_normal&state_n===s_writereq,true.B,Mux(aw_fire,false.B,awvalid))
	wvalid  := Mux(state===s_normal&state_n===s_writereq,true.B,Mux(w_fire,false.B,wvalid))
	wcnt 	:= Mux(state===s_normal&state_n===s_writereq,0.U,Mux(b_fire,wcnt + 1.U,wcnt))
	w.last 	:= Mux(wcnt===(blockBytes/4).U&&(!w_fire),true.B,Mux(w_fire,false.B,w.last))
	bready 	:= Mux(aw_fire&&w_fire,true.B,
				Mux(b_fire,false.B,bready))





/////////////////////////////////////////////////////

	state := state_n

	switch (state){
		is(s_reset){
			state_n := s_normal
		}
		is(s_normal){
			when(send_req){
				state_n := Mux(reqMsg.mem_type,s_readreq,s_writereq)
			}
		}
		is(s_readreq){
			when(ar_fire){
				state_n := s_wait_ack
			}
		}
		is(s_writereq){
			// when(ar_fire){
			// 	state_n := s_wait_ack
			// }
		}
		is(s_wait_ack){
			when(r_fire&&master.r.bits.last||b_fire&&wcnt===(blockBytes/4-1).U){
				state_n := s_end
			}
		}
		is(s_end){
			state_n := s_normal
		}
	}
}
class DCache(implicit p: Parameters) extends LazyModule{
    lazy val module = new DCacheImp(this)
    val masterNode = AXI4MasterNode(Seq(
    AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
        name = "DCache")))))
    
    
}
class DCacheImp(val outer: DCache)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasDCacheParameters with GRVOpConstants with DontTouch{
	def AddressSplitter(addr:UInt)(implicit p:Parameters)={
		val res = Wire(new CacheMsg())
		res.tag    := addr(XLEN-1,(indexWidth+offsetWidth))
		res.index  := addr((indexWidth+offsetWidth)-1,offsetWidth)
		res.offset := addr((offsetWidth)-1,0)
		res.bank   := addr(offsetWidth-1,offsetWidth-bankWidth)
		res
	}
    val io = IO(new DCacheBundle)
    val (master,_) = outer.masterNode.out(0)

    class Meta extends Bundle{
        val dirty = Bool()
    }

	val rdata     = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,UInt(XLEN.W)))))
    val rtag      = Wire(Vec(nWays,Vec(numReadport,UInt(tagWidth.W))))//numReadport -> cache line 
    val rvalid    = Wire(Vec(nWays,Vec(numReadport,Bool())))

    val rmeta     = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,new Meta))))
	
    val meta      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new SRAMHelper(nSets,new Meta()))))
    val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new SRAMHelper(nSets,UInt(XLEN.W)))))
	val tag       = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new SRAMHelper(nSets,UInt(tagWidth.W)))))// each way has n(numReadport) ram//if numReadport==2,tag(i)(0)for port0
	val valid     =  RegInit(VecInit(Seq.fill(nWays)(
							VecInit(Seq.fill(numReadport)(
							VecInit(Seq.fill(nSets)(false.B))
							)))))

    val refilled = WireInit(false.B)
    io.read.foreach(r=>
        r.req.ready := (!io.write.req.valid)
    )
    io.write.req.ready := (!refilled)

	val s0_rvalid = io.read.map(_.req.fire)
	val s0_raddr  = io.read.map(_.req.bits.addr)
	val s0_rmsg    = Wire(Vec(numReadport,Valid(new CacheMsg())))
	


	val s0_wvalid = io.write.req.fire
	val s0_waddr  = io.write.req.bits.addr
	val s0_wmsg    = Wire(Valid(new CacheMsg()))
	
	val s0_wbankmask     = io.write.req.bits.mask


	//得到每个way的重组data，也就是把两个访问数据合并

	val s1_rmsg     = RegNext(s0_rmsg)

	val s1_rvalid   = RegNext(VecInit(s0_rvalid))

	val s1_wvalid       = RegNext(s0_wvalid   )
	val s1_waddr        = RegNext(s0_waddr    )
	val s1_wmsg         = RegNext(s0_wmsg     )
	val s1_wbankmask    = RegNext(s0_wbankmask)

    val s1_whit    = VecInit((rtag zip rvalid).map{case(a,b)=>
        VecInit((a zip b zip s1_rmsg).map{ case((r,m),l)=>
            r===l.bits.tag&(RegNext(m)===true.B)
        }).reduce(_||_)
	})
    val s1_whitWay = OHToUInt(s1_whit)

	val s1_rhit = Transpose(VecInit((rtag zip rvalid).map{case(a,b)=>
        VecInit((a zip b zip s1_rmsg).map{ case((r,m),l)=>
            r===l.bits.tag&(RegNext(m)===true.B)
        })
	}))

	
	val s1_missMsg0 = Wire(new MissMsg())//uesd for write and read
	val s1_missMsg1 = Wire(new MissMsg())//only for read

	val s1_whitData = Wire(Vec(bankNum,UInt(XLEN.W)))


	val s2_missMsg0 = RegNext(s1_missMsg0)
	val s2_missMsg1 = RegNext(s1_missMsg1)

    

//random替换算法
	val rp = RegInit(0.U(log2Ceil(nWays).W))
	rp := rp + 1.U 
    val refillWay = WireInit(rp)

    s0_wmsg.bits := AddressSplitter(io.write.req.bits.addr)
    s0_wmsg.valid:= io.write.req.valid
	for(i <- 0 until numReadport){
		s0_rmsg(i).bits := AddressSplitter(io.read(i).req.bits.addr)
		s0_rmsg(i).valid:= io.read(i).req.valid 
	}

//miss处理单元
	val missunit   = Module(new MissUnit())


	s1_missMsg0.addr := Mux(s1_wvalid,Cat(s1_wmsg.bits.tag,s1_wmsg.bits.index)<<offsetWidth,Cat(s1_rmsg(0).bits.tag,s1_rmsg(0).bits.index)<<offsetWidth)
	s1_missMsg1.addr := Cat(s1_rmsg(1).bits.tag,s1_rmsg(1).bits.index)<<offsetWidth
	s1_missMsg0.miss := Mux(s1_rvalid(0),!s1_rhit(0).reduce(_||_),
                        Mux(s1_wvalid,!s1_whit.reduce(_||_),false.B))
	s1_missMsg1.miss := Mux(s1_rvalid(1),!s1_rhit(1).reduce(_||_),false.B)	


//////////////////////pipe2///////////////////
	val refillingMsg0  = RegInit(s1_missMsg0)
	val refillingMsg1  = RegInit(s1_missMsg1)

//refill tag和index的第0个元素总是来自第一个miss的数据
	val refill_tag = Wire(Vec(numReadport,UInt(tagWidth.W)))
	val refill_idx = Wire(Vec(numReadport,UInt(tagWidth.W)))


	refillWay := rp


///////////////////////////////////////////////////////////////////
//////////////////////////Tag RAM//////////////////////////////////
// 	for( i <- 0 until tag.length){
// 		for(j  <- 0 until tag.head.length){
// 			val tag_idx = Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),s0_msg(j).bits.index)
// 			tag(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&refilled&(refillMask.xorR===false.B),true.B,
// 									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,
// 									s0_msg(j).valid))
// 			tag(i)(j).io.addr   := Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),s0_msg(j).bits.index)
// 			tag(i)(j).io.write  :=  Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
// 									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,false.B))
// 			tag(i)(j).io.dataIn := Mux((i.U===refillWay)&(refilled),refill_tag(0),refill_tag(1)) 
// 			rtag(i)(j)  := tag(i)(j).io.dataOut
// 			rvalid(i)(j):= valid(i)(j)(tag_idx(log2Ceil(nSets)-1,0))
// 			when(tag(i)(j).io.write){
// 				valid(i)(j)(tag_idx(log2Ceil(nSets)-1,0)) :=  true.B
// 			}
// 		}
// 	}
// ////////////////////////////////DATA RAM////////////////////////////
// 	for( i <- 0 until data.length){
// 		for(j  <- 0 until data.head.length){
// 			data(i)(j).io.enable := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
// 									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,
// 									Mux(s0_bankmask(j)===1.U,s0_msg(0).valid,
// 									s0_msg(1).valid)))

// 			data(i)(j).io.addr   := Mux(refilled,Mux(i.U===refillWay,refill_idx(0),refill_idx(1)),
// 									Mux(s0_bankmask(j)===1.U,s0_msg(0).bits.index,
// 									s0_msg(1).bits.index))

// 			data(i)(j).io.write  := Mux((i.U===refillWay||i.U===((refillWay+1.U)%nWays.U))&(refilled)&(refillMask.xorR===false.B),true.B,
// 									Mux((i.U===refillWay)&(refilled)&(refillMask.xorR===true.B),true.B,false.B))
// 			data(i)(j).io.dataIn := Mux(refilled,Mux(i.U===refillWay,refillData(0)(j),refillData(1)(j)),0.U)//refillData(0) must be the first miss data

// 			rdata(0)(i)(j)       := Mux(s0_bankmask(j)===1.U,data(i)(j).io.dataOut,0.U)
// 			rdata(1)(i)(j) 		 := Mux(s0_bankmask(j)===1.U,0.U,data(i)(j).io.dataOut)
// 		}
// 	}





    override def toString: String = GRVString(
    "==L1-DCache==",
    "Block bytes   : " + blockBytes,
    "Row bits      : " + rowBits,
    "Word bits     : " + XLEN,
    "Sets          : " + nSets,
    "Ways          : " + nWays,
    "RAMs          : (" +  blockBytes/bankNum + " x " + nSets + ") using " + bankNum + " banks"+"\n")
}