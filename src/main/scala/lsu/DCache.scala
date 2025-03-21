package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// import freechips.rocketchip.util.Annotated.resetVector
import freechips.rocketchip.util.DontTouch
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink.TLMessages.d
case object DCacheKey extends Field[DCacheParams]


case class DCacheParams(
    nSets: Int = 16,
    nWays: Int = 2,
    rowBits: Int = 128,
    numMSHRs:Int = 4,
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
	val numReadport = ldIssueParam.issueWidth
	
	def AddressSplitter(addr:UInt)(implicit p:Parameters)={
		val res = Wire(new CacheMsg())
		res.tag    := addr(XLEN-1,(indexWidth+offsetWidth))
		res.index  := addr((indexWidth+offsetWidth)-1,offsetWidth)
		res.offset := addr((offsetWidth)-1,0)
		res.bank   := addr(offsetWidth-1,offsetWidth-bankWidth)
		res
	}
	def newRdataHelper(select: UInt, rdata: UInt): UInt = {
		val DataLookupTable = Seq(
		"b000".U -> Cat(Fill(XLEN-8,0.U),rdata(7, 0)),
		"b001".U -> Cat(Fill(XLEN-16,0.U),rdata(15, 0)),
		"b110".U -> rdata(31, 0),
		"b100".U -> Cat(Fill(XLEN-8,rdata(7)),rdata(7, 0)),
		"b101".U -> Cat(Fill(XLEN-16,rdata(15)),rdata(15, 0))	 

		)
		MuxLookup(select,"hbadc0de".U)(DataLookupTable)
	}
	def BankAlign(addr:UInt):UInt={
		(addr>>offsetWidth)<<offsetWidth
	}
	def OffsetAlign(addr:UInt):UInt={
		(addr>>(log2Ceil(XLEN/8)))<<(log2Ceil(XLEN/8))
	}
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

	/* 
	在向下级发出请求时，对way进行替换，并且得到数据
	miss并不会向下级发送写请求，只有replace才会发送写请求
	只有在mshr向下级发送请求时候才会替换，这样虽然会降低性能，但是不用去维护一致性
	 */

class DMissUnit(implicit p:Parameters) extends  GRVModule with HasDCacheParameters{

    val mshrSz 		  = log2Ceil(numMSHRs)

	val io = IO(new Bundle{

		val missmsg       = Input(Vec(numReadport,new DCacheMissMsg()))
		val miss_replay   = Output(Vec(numReadport,Bool()))
		val refill        = Decoupled(new RefillMsg)
		val full          = Output(Bool())
		val master   	  = AXI4Bundle(CPUAXI4BundleParameters())
		val replace_req   = Decoupled(new replaceReq)//replace同时进行invalid
		val replace_resp  = Flipped(Valid(new replaceResp))//regnext(req.fire)
		val flush		  = Input(Bool())
		val bypass 		  = Output(Vec(numReadport,new BypassMsg()))
	})

    class MSHRFlag extends Bundle{
        val valid = Bool()
        val inflight = Bool()
        // val timeout  = Bool()
        // val same_inflight  = Bool()
    }
	class MSHREntry extends Bundle{
		val flag    = new MSHRFlag()
		val data    = Vec(bankNum,UInt(XLEN.W))
		val addr    = UInt(XLEN.W)
		val mask    = Vec(bankNum,UInt((XLEN/8).W))
		val mem_type= Bool()
	}
    val reqData = Wire(Vec(numReadport,new MSHREnq))
    val mshrs = RegInit(VecInit.fill(numMSHRs)(0.U.asTypeOf(new MSHREntry)))

	/*如果两个miss地址一样，replay其中一个miss请求，如果两个miss请求均不满足replay条件，则全部replay
	对于mshr，必须保证mshr内没有重复的地址，否则可能出现后refilled的数据覆盖前面的数据
	合并条件：
	1.请求 A 的 Acquire 请求还没有握手, 且 A 是 load 或 store 请求, B 是 load 请求;
	2.请求 A 的 Acquire 已经发送出去了, 但是还没有收到 Grant(Data), 或者收到 Grant(Data) 了但还没有转发给 Load Queue, 且 A 是 load 或 store 请求, B 是 load 请求.
	*/

	val replaceEntry = RegInit(0.U.asTypeOf(new WBQEntry))
	val replaceMsg 	 = WireInit(io.replace_resp.bits) 
	dontTouch(replaceMsg)
	//只能为2的幂次
    val mshr_enq_ptr  = RegInit(0.U(log2Ceil(numMSHRs+1).W))
    val mshr_deq_ptr  = RegInit(0.U(log2Ceil(numMSHRs+1).W))
	

	val reqMsg = mshrs(mshr_deq_ptr)

	val allocate_req  = WireInit(VecInit.fill(numReadport)(false.B))
	val merge_req     = WireInit(VecInit.fill(numReadport)(false.B))
	val merge_sels    = VecInit(io.missmsg.map{msg=>//only 1
		VecInit(mshrs.map{m=>
			m.addr===msg.addr&&m.flag.valid&&msg.miss&&(m.mem_type)&&(!io.full)
		})
	})
	val merge_bypass   = merge_req zip io.missmsg map{case(merge,msg)=>
			merge&&(msg.addr===reqMsg.addr)&&(io.refill.fire)
		}
	val merge_bypass_idx = PriorityEncoder(merge_bypass)

	val merge_bypass_msg = io.missmsg(merge_bypass_idx)

	//MSHR ENQ
    val enq_idxs = VecInit.tabulate(numReadport)(i => PopCount(allocate_req.take(i)))
    val enq_offset = VecInit(enq_idxs.map(_+mshr_enq_ptr(mshrSz-1,0)))
	val num_enq = PopCount(io.missmsg.map(_.miss))
	val num_valid = PopCount(mshrs.map(_.flag.valid))
	val num_do_enq   = PopCount(allocate_req)
	val enq_next_ptr = mshr_enq_ptr + num_do_enq
	io.full := (mshr_enq_ptr(mshrSz)=/=mshr_deq_ptr(mshrSz)&&(num_enq+mshr_enq_ptr)(mshrSz-1,0)>mshr_deq_ptr(mshrSz-1,0))||
    num_valid===numMSHRs.U
	val empty = (mshr_enq_ptr(mshrSz)===mshr_deq_ptr(mshrSz))&&mshr_enq_ptr(mshrSz-1,0)===mshr_deq_ptr(mshrSz-1,0)
	dontTouch(empty)
	//normal->no miss req->has miss  wait_ack->wait data end->receive data
	/* 
	normal:		没有发生miss的状态	
	replace_req:请求替换块,如果块有效，立刻进入替换阶段,否则进入refill阶段

	refill_req:		生成miss的axi信号	
	refill_wait_ack:	等待接受数据
	refill_end:		看到last信号，缺失处理结束
	
	replace_wait_ack:等待写入数据
	replace_invalid:
	 */
	/*
	对于wbq采用唤醒机制，如果在replace_req的替换块是脏的，分配一个表项，并且返回idx，当refill完成，对wbq进行唤醒，数据写入下级存储

	不需要在替换时就将替换的way无效，可以在替换完再无效，期间访问数据仍然可以写入DCache，不过需要同步进入wbq
	*/
	val s_reset :: s_normal::s_replace_req::s_replace_wait_ack::s_replace_end::s_refill_req ::s_refill_wait_ack :: s_refill_end :: Nil = Enum(8)
	val state = RegNext(s_reset)
	val state_n = WireInit(state)
	val send_req = (state===s_normal)&&(!empty)
	dontTouch(send_req)
	
	
//////////////////////////////////WBQ///////////////////////////////


///////////////////////////////////MSHR/////////////////////////////
    //write
	//mshr合并机制和sb类似，


//如果write和read缺失，该如何解决？
	// val write_read_miss = (1 until numReadport).map{
	// 	io.missmsg(0).addr
	// }
    for(i <- 0 until numReadport){
		val miss_same =(0 until i).foldLeft(false.B) ((p,k) =>
            io.missmsg(i).addr===io.missmsg(k).addr||p)
		val check_same 		 = mshrs.map{mshr=>
				mshr.addr===io.missmsg(i).addr&&mshr.flag.valid
			}.reduce(_||_)
		
		io.miss_replay(i)   := ((io.full)||miss_same||(check_same&&(!merge_sels(i).reduce(_||_))))&&io.missmsg(i).miss


		merge_req(i)		:= (!miss_same)&&merge_sels(i).reduce(_||_)
        allocate_req(i)     := io.missmsg(i).miss&&(!io.miss_replay(i))&&(!merge_req(i))
        reqData(i).addr 	:= io.missmsg(i).addr
        reqData(i).data 	:= io.missmsg(i).data
		reqData(i).mask 	:= io.missmsg(i).mask
        reqData(i).mem_type := io.missmsg(i).memtype
        reqData(i).valid	:= io.missmsg(i).miss       
    }

	dontTouch(reqData)
    /*
    请求数目不定如果想写入fifo，可以通过计数true+mshr_enq_ptr
    */

    dontTouch(enq_idxs)
    dontTouch(enq_offset)
	dontTouch(allocate_req)
    for(i <- 0 until numReadport){
        for(j <- 0 until numMSHRs){
			val merge_data = Wire(Vec(bankNum,Vec(XLEN/8,UInt(8.W))))
			//32 to 8
			for(m <- 0 until bankNum){
				for(n<- 0 until XLEN/8){
					merge_data(m)(n) := Mux(reqData(i).mask(m)(n)===1.U,reqData(i).data(m)((n+1)*8-1,n*8),mshrs(j).data(m)((n+1)*8-1,n*8) ) 
				}
			}   
            when(merge_req(i)&&merge_sels(i)(j)&&(!io.refill.fire)){ 	
				mshrs(j).data 		:= merge_data.map{d=>Cat(d.map(_.asUInt).reverse)}	
				for(b <- 0 until bankNum){
					mshrs(j).mask(b) 	    := reqData(i).mask(b)|mshrs(j).mask(b) 	
				}
			}
			.elsewhen(allocate_req(i)&&enq_offset(i)===j.U){
				mshrs(j).addr 		:= reqData(i).addr 	
				mshrs(j).data 		:= reqData(i).data 	
				mshrs(j).mask 	    := reqData(i).mask 	
				mshrs(j).mem_type   := reqData(i).mem_type 
				mshrs(j).flag.valid	:= true.B
            }
        }
    }


	when(allocate_req.reduce(_||_)){
		mshr_enq_ptr := enq_next_ptr
	}
	when(io.refill.fire){
		mshrs(mshr_deq_ptr):= 0.U.asTypeOf(new MSHREntry())
		mshr_deq_ptr := mshr_deq_ptr + 1.U
	}


	
	mshrs(mshr_deq_ptr).flag.inflight := Mux(io.refill.fire,false.B,Mux(send_req,true.B,mshrs(mshr_deq_ptr).flag.inflight))
	val s_refillData = Reg(Vec(bankNum,UInt(XLEN.W)))
	val replace_addr = AddressSplitter(reqMsg.addr) 
	val replace_valid = RegInit(false.B)
	io.replace_req.valid := replace_valid
	io.replace_req.bits.idx := replace_addr.index
	
	when(io.replace_req.fire){
		replace_valid := false.B
	}.elsewhen(state===s_normal&&state_n===s_replace_req){
		replace_valid 		:= true.B
	}
	// val s_refillMask	 = RegInit(0.U(numReadport.W))//哪一个port发生miss
////////////////////////AXI//////////////////////////
	val ar_fire = io.master.ar.fire
	val r_fire 	= io.master.r.fire
	val aw_fire = io.master.aw.fire
	val w_fire  = io.master.w.fire
	val b_fire  = io.master.b.fire

	
	val ar = Reg(new AXI4BundleAR(CPUAXI4BundleParameters()))
	val r  = Reg(new AXI4BundleR(CPUAXI4BundleParameters()))
	val aw = Reg(new AXI4BundleAW(CPUAXI4BundleParameters()))
	val w  = Reg(new AXI4BundleW(CPUAXI4BundleParameters()))
	val b  = Reg(new AXI4BundleB(CPUAXI4BundleParameters()))
	ar <> io.master.ar.bits
	aw <> io.master.aw.bits
	w  <> io.master.w.bits
	b  <> io.master.b.bits
	// dontTouch(io.master)
	// dontTouch(reqMsg)
	val rcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))
	val wcnt 		= Reg(UInt(log2Ceil(2*blockBytes/4).W))

	val rready 		= Reg(Bool())
	val arvalid		= RegInit(false.B)
	val awvalid 	= RegInit(false.B)
	val wvalid 		= RegInit(false.B)
	val bready 		= RegInit(false.B)
	// val wdata	    = RegInit(replaceEntry.data)
	io.master.r.ready := rready
	io.master.ar.valid:= arvalid
	io.master.aw.valid:= awvalid
	io.master.w.valid := wvalid
	io.master.b.ready := bready
	
	//生成read axi信号，默认一笔传输的数据大小为4字节，突发长度为blk_size或者2倍的blk
	when(state_n===s_refill_req){
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
	}.elsewhen(io.master.r.bits.last){
		rready := false.B
	}
	when(state===s_refill_wait_ack&io.master.r.fire){
		s_refillData((rcnt%(bankNum.U))(log2Ceil(bankNum)-1,0)) := io.master.r.bits.data
		rcnt := rcnt + 1.U
	}

	val merge_bypass_data = Wire(Vec(bankNum,Vec(XLEN/8,UInt(8.W))))
	val merge_bypass_mask = Wire(Vec(bankNum,UInt((XLEN/8).W)))
	//32 to 8
	for(i <- 0 until bankNum){
		for(j<- 0 until XLEN/8){
			merge_bypass_data(i)(j) := Mux(merge_bypass.reduce(_||_)&&merge_bypass_msg.mask(i)(j)===1.U,merge_bypass_msg.data(i)((j+1)*8-1,j*8),reqMsg.data(i)((j+1)*8-1,j*8) ) 
		}
		merge_bypass_mask(i) := Mux(merge_bypass.reduce(_||_),merge_bypass_msg.mask(i)|reqMsg.mask(i),reqMsg.mask(i))
	}
	val refillData = Wire(Vec(bankNum,Vec(XLEN/8,UInt(8.W))))
	//32 to 8
	for(i <- 0 until bankNum){
		for(j<- 0 until XLEN/8){
			refillData(i)(j) := Mux(merge_bypass_mask(i)(j)===1.U,merge_bypass_data(i)(j),s_refillData(i)((j+1)*8-1,j*8) ) 
		}
	}


	//如果merge请求同时mshr表象refilled，这时候就可以去将refilled数据bypass到dcache，并将miss去除
	for(i <- 0 until numReadport){
		io.bypass(i).valid 			:= io.refill.fire&&merge_bypass(i)
		io.bypass(i).bypass_data  	:= io.refill.bits.refillData
	}
	dontTouch(refillData)
	io.refill.valid := state===s_refill_end
	io.refill.bits.refillData := refillData.map{i=>
								Cat(i.map(_.asUInt).reverse)
							}
	io.refill.bits.refill_addr:= reqMsg.addr
	io.refill.bits.refilled	  := state===s_refill_end
	io.refill.bits.refillWay  := replaceEntry.way 
	//
	//生成write axi信号，默认一笔传输的数据大小为4字节，突发长度为blk_size或者2倍的blk
	when(state===s_replace_req&state_n===s_replace_wait_ack){
		aw.id    := 1.U
		aw.addr  := replaceMsg.addr//Mux(missmsg0.miss,,missmsg1.addr)
		aw.len   := (blockBytes/4-1).U//Mux(missmsg1.miss&missmsg0.miss,(2*blockBytes/4-1).U,)
		aw.size  := "b010".U
		aw.burst := "b00".U
		aw.lock  := DontCare
		aw.cache := DontCare
		aw.prot  := DontCare
		aw.qos   := DontCare
		w.strb 	 := "b1111".U

		// s_refillMask := Cat(missmsg1.miss,missmsg0.miss).asUInt
	}
	when(state===s_replace_req){
		replaceEntry.addr := replaceMsg.addr//for debug
		replaceEntry.data := replaceMsg.data
		replaceEntry.way  := replaceMsg.way
		replaceEntry.valid:= io.replace_resp.valid&&io.replace_resp.bits.dirty
	}
	
	when(state===s_replace_req&&state_n===s_replace_wait_ack||(state===s_replace_wait_ack&&io.master.w.fire)){
		io.master.w.bits.data := replaceEntry.data(wcnt)
	}
	awvalid := Mux(state===s_replace_req&state_n===s_replace_wait_ack,true.B,Mux(aw_fire,false.B,awvalid))
	wvalid  := Mux(state===s_replace_req&state_n===s_replace_wait_ack,true.B,Mux(w_fire&&state_n===s_replace_end,false.B,wvalid))
	wcnt 	:= Mux(state===s_replace_req&state_n===s_replace_wait_ack,0.U,Mux(w_fire,wcnt + 1.U,wcnt))
	io.master.w.bits.last 	:= wcnt===(blockBytes/4-1).U&&(w_fire)
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
				state_n := s_replace_req
				// state_n := Mux(reqMsg.mem_type,s_readreq,s_writereq)
			}
		}
		is(s_replace_req){
			when(io.replace_resp.valid){
				state_n := Mux(io.replace_resp.bits.dirty,s_replace_wait_ack,s_refill_req)
			}
		}
		is(s_replace_wait_ack){
			when(w_fire&&wcnt===(blockBytes/4-1).U){
				state_n := s_replace_end
			}
		}
		is(s_replace_end){
			state_n := s_refill_req
		}
		is(s_refill_req){
			when(ar_fire){
				state_n := s_refill_wait_ack
			}
		}
		is(s_refill_wait_ack){
			when(r_fire&&io.master.r.bits.last){
				state_n := s_refill_end
			}
		}
		is(s_refill_end){
			when(io.refill.fire){
				state_n := s_normal
			}
			
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


/*
读DCache：
s0：读tag，读data
s1：对比tag
s2：给出miss信息
写DCache
s0：读tag
s1：对比tag 并且写入data，dirty，如果此时refill数据到了，延迟refill数据
s2：给出miss信息,ack

当write miss，之后如果同一地址miss将会进行合并，只有当mshr非空，才会对sb的数据无效
*/
class DCacheImp(val outer: DCache)(implicit p: Parameters) extends LazyModuleImp(outer) 
with HasDCacheParameters with GRVOpConstants with DontTouch{

    val io = IO(new DCacheBundle)
	dontTouch(io)
    val (master,_) = outer.masterNode.out(0)
	dontTouch(master)
    class Meta extends Bundle{
        val dirty = Bool()
    }

	val rdata     = Wire(Vec(numReadport,Vec(nWays,Vec(bankNum,UInt(XLEN.W)))))
    val rtag      = Wire(Vec(nWays,Vec(numReadport,UInt(tagWidth.W))))//numReadport -> cache line 
    val rvalid    = Wire(Vec(nWays,Vec(numReadport,Bool())))

    val rmeta     = Wire(Vec(nWays,Vec(numReadport,new Meta)))//only used for replace
	
    val meta      = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new SRAMHelper(nSets,new Meta()))))
    val data      = Seq.fill(nWays)(Seq.fill(bankNum)(Module(new SRAMHelper(nSets,UInt(XLEN.W)))))
	val tag       = Seq.fill(nWays)(Seq.fill(numReadport)(Module(new SRAMHelper(nSets,UInt(tagWidth.W)))))// each way has n(numReadport) ram//if numReadport==2,tag(i)(0)for port0
	val valid     = RegInit(VecInit(Seq.fill(nWays)(
							VecInit(Seq.fill(numReadport)(
							VecInit(Seq.fill(nSets)(false.B))
							)))))
	
	val missunit   		= Module(new DMissUnit())
	
	val refill   		= WireInit(missunit.io.refill.bits)
	val mshr_full		= WireInit(missunit.io.full)
	val refillData 		= WireInit(refill.refillData)
	val refill_addr		= WireInit(refill.refill_addr)
	val refillMsg 		= AddressSplitter(refill_addr)
	val refilled 		= WireInit(refill.refilled&&missunit.io.refill.fire)
	val refillWay 		= WireInit(refill.refillWay)
	val replaceMsg 		= WireInit(missunit.io.replace_req.bits)
	val replaced  		 = WireInit(missunit.io.replace_req.fire)
	val s0_bank_conflict = WireInit(VecInit.fill(numReadport)(false.B))
	val s0_rvalid = io.read.map(_.req.fire)
	val s0_raddr  = io.read.map(_.req.bits.addr)
	val s0_rmsg    = Wire(Vec(numReadport,Valid(new CacheMsg())))
	
	
	io.refillMsg.refilled := refilled
	io.refillMsg.refillWay:= refillWay
	io.refillMsg.refillData:= refillData
	io.refillMsg.refill_addr:= refill_addr

	val s0_wvalid = io.write.req.fire
	val s0_waddr  = io.write.req.bits.addr
	val s0_wdata  = io.write.req.bits.data
	val s0_wmsg    = Wire(Valid(new CacheMsg()))
	val s0_wmask     = io.write.req.bits.mask


	//得到每个way的重组data，也就是把两个访问数据合并

	val s1_rmsg     	= RegNext(s0_rmsg)

	val s1_rvalid   	= RegNext(VecInit(s0_rvalid.map{i=>i&(!io.flush)}))

	val s1_wdata 		= RegNext(s0_wdata)
	val s1_wvalid       = RegNext(s0_wvalid   )
	val s1_waddr        = RegNext(s0_waddr    )
	val s1_wmsg         = RegNext(s0_wmsg     )
	val s1_wmask        = RegNext(s0_wmask)
	

	val s1_whit = VecInit((rtag zip rvalid).map{case(a,b)=>
        VecInit((a zip b).map{case(r,m)=>
            r===s1_wmsg.bits.tag&(RegNext(m)===true.B)
        }).reduce(_&&_)
	})
	val s1_rhit = Transpose(VecInit((rtag zip rvalid).map{case(a,b)=>
        VecInit((a zip b zip s1_rmsg).map{ case((r,m),l)=>
            r===l.bits.tag&(RegNext(m)===true.B)
        })
	}))
    val s1_whitWay = OHToUInt(s1_whit)
	val s1_rhitWay = VecInit(s1_rhit.map{way=>
		OHToUInt(way)
	})
	dontTouch(s1_rhitWay)

	
	val s1_missMsg = Wire(Vec(numReadport,new DCacheMissMsg()))//uesd for write and read
	

	val s1_whitData = Wire(Vec(bankNum,UInt(XLEN.W)))
	val s1_rhitData = Wire(Vec(numReadport,(Vec(bankNum,UInt(XLEN.W)))))
	
	val s1_data  		= Wire(Vec(bankNum,Vec(XLEN/8,UInt(8.W))))

	
	//32 to 8
	for(i <- 0 until bankNum){
		for(j<- 0 until XLEN/8){
			s1_data(i)(j) := Mux(s1_wmask(i)(j)===1.U,s1_wdata(i)((j+1)*8-1,j*8),s1_whitData(i)((j+1)*8-1,j*8)) 
		}
	}

	val s1_final_wdata = WireInit(VecInit(s1_data.map{i=>
								Cat(i.map(_.asUInt).reverse)
	}))
	dontTouch(s1_final_wdata)
	val s2_rvalid  		= RegNext(VecInit(s1_rvalid.map{i=>i&(!io.flush)}))
	val s2_wvalid 		= RegNext(s1_wvalid)
	val s2_whit 		= RegNext(s1_whit)
	val s2_wmsg         = RegNext(s1_wmsg     )
	val s2_rmsg     	= RegNext(s1_rmsg)
	val s2_rhit 		= RegNext(VecInit(s1_rhit.map{hit=>
		hit.reduce(_||_)
	}))
	val s2_rhitData 	= RegNext(s1_rhitData)
	val s2_missMsg 		= RegNext(s1_missMsg)
	val miss_bypass     = WireInit(missunit.io.bypass)
	// val missMsg         = WireInit(VecInit.fill(numReadport)(0.U.asTypeOf(new DCacheMissMsg())))
//random替换算法
	val rp = RegInit(0.U(log2Ceil(nWays).W))
	rp := rp + 1.U 
    

    s0_wmsg.bits := AddressSplitter(io.write.req.bits.addr)
    s0_wmsg.valid:= io.write.req.fire
	for(i <- 0 until numReadport){
		s0_rmsg(i).bits := AddressSplitter(io.read(i).req.bits.addr)
		s0_rmsg(i).valid:= io.read(i).req.fire 
	}
	dontTouch(s0_rmsg)
	// s0_bank_conflict := PopCount(s0_rmsg.map{i=>
	// 				UIntToOH(i.bits.bank)
	// }.reduce(_|_))=/=numReadport.U

	// s0_bank_conflict := 
	for(i<- 0 until numReadport){
		s0_bank_conflict(i) := VecInit((0 until i).foldLeft(false.B){(bank,idx)=>
			s0_rmsg(idx).bits.bank===s0_rmsg(i).bits.bank
		}).reduce(_||_)
	}
	dontTouch(s0_bank_conflict)
	s1_whitData := rdata(0)(s1_whitWay)
	for(i <- 0 until numReadport){
		s1_rhitData(i) := rdata(i)(s1_rhitWay(i)) 
	}
//miss处理单元
	val reqReady = (!mshr_full)&(!s1_wvalid)&(!missunit.io.refill.valid)&(!missunit.io.replace_req.valid)&(!io.flush)
    io.read zip s0_bank_conflict foreach{case(r,conflict)=>
        r.req.ready := (!io.write.req.fire)&reqReady&(!conflict)
	}
	//
    io.write.req.ready := reqReady


	missunit.io.missmsg:= s2_missMsg
	missunit.io.flush  := io.flush
	//for replace

////////////////////////////////replace//////////////////////////////////
//not the same time 
	val rlp_valid = RegNext(rvalid)
	dontTouch(rlp_valid) 
	missunit.io.refill.ready	  := (!s1_wvalid)&(!s1_rvalid.reduce(_||_))
	missunit.io.replace_req.ready := (!s1_wvalid)&(!s1_rvalid.reduce(_||_))
	
	missunit.io.replace_resp.valid   	:= RegNext(replaced)
	missunit.io.replace_resp.bits.way	:= rp
	missunit.io.replace_resp.bits.addr 	:= Cat(rtag(rp)(0),RegNext(missunit.io.replace_req.bits.idx))<<offsetWidth
	missunit.io.replace_resp.bits.dirty	:= rmeta(rp)(0).dirty&&rlp_valid(rp)(0)
	missunit.io.replace_resp.bits.data 	:= rdata(0)(rp)
	dontTouch(rmeta)
	dontTouch(rvalid)
	dontTouch(rtag)
	//由于在miss阶段可能填充写入的数据，所以这里在refill时需要一个dirty位，将合并了写请求的mshr dirty置为高
	for(i <- 0 until numReadport){
		when(i.U===0.U){
			for(j <- 0 until bankNum){
				s1_missMsg(i).mask(j) := Mux(s1_rvalid(i),0.U,s1_wmask(j))
			}
			
			s1_missMsg(i).data := s1_wdata
			s1_missMsg(i).memtype := s1_rvalid(i)
			s1_missMsg(i).addr := Mux(s1_wvalid,Cat(s1_wmsg.bits.tag,s1_wmsg.bits.index)<<offsetWidth,Cat(s1_rmsg(0).bits.tag,s1_rmsg(0).bits.index)<<offsetWidth)
			s1_missMsg(i).miss := Mux(s1_rvalid(i),!s1_rhit(i).reduce(_||_),
								Mux(s1_wvalid,!s1_whit.reduce(_||_),false.B))
		}.otherwise{
			s1_missMsg(i).mask := DontCare
			s1_missMsg(i).data := DontCare
			s1_missMsg(i).memtype := true.B
			s1_missMsg(i).addr := Cat(s1_rmsg(i).bits.tag,s1_rmsg(i).bits.index)<<offsetWidth
			s1_missMsg(i).miss := Mux(s1_rvalid(i),!s1_rhit(i).reduce(_||_),false.B)	
		}
	}

	
/////////////////////////////////////////////////////////////////////
	missunit.io.master <>master

//////////////////////pipe2///////////////////
	for(i <- 0 until numReadport){
		val idx = s2_rmsg(i).bits.bank
		io.read(i).resp.valid 		:= s2_rvalid(i)
		io.read(i).resp.bits.data	:= Mux(miss_bypass(i).valid,miss_bypass(i).bypass_data(idx),s2_rhitData(i)(idx))
		io.read(i).resp.bits.hit	:= Mux(miss_bypass(i).valid,true.B,s2_rhit(i)&&s2_rvalid(i))
		io.read(i).resp.bits.replay	:= missunit.io.miss_replay(i)&&s2_rvalid(i)
	}
	io.write.resp.valid 	:= s2_wvalid
	io.write.resp.bits.addr := Cat(s2_wmsg.bits.tag,s2_wmsg.bits.index)<<offsetWidth
	io.write.resp.bits.hit 	:= Mux(miss_bypass(0).valid,true.B,s2_whit.reduce(_||_))
	io.write.resp.bits.replay:= missunit.io.miss_replay(0)&&s2_wvalid


///////////////////////////////////////////////////////////////////
//////////////////////////Tag RAM//////////////////////////////////
	for( i <- 0 until tag.length){
		for(j  <- 0 until tag.head.length){
			val tag_idx = Mux(refilled,refillMsg.index,
							Mux(replaced,replaceMsg.idx,
							Mux(s0_wvalid,s0_wmsg.bits.index,s0_rmsg(j).bits.index)))
			val vld_idx = Mux(refilled,refillMsg.index,
							Mux(RegNext(replaced),RegNext(replaceMsg.idx),
							Mux(s0_wvalid,s0_wmsg.bits.index,s0_rmsg(j).bits.index)))
			val enable  = refilled||(s0_wvalid||s0_rvalid.reduce(_||_))||replaced
			val write	= refilled&&(i.U===refillWay)
			// val w_data 	= refillMsg.tag
			tag(i)(j).io.enable := enable
			tag(i)(j).io.addr   := tag_idx
			tag(i)(j).io.write  := write 
			tag(i)(j).io.dataIn := refillMsg.tag
			rtag(i)(j)  		:= tag(i)(j).io.dataOut
			rvalid(i)(j)		:= valid(i)(j)(tag_idx)

			valid(i)(j)(vld_idx(log2Ceil(nSets)-1,0)) := Mux(write,true.B,
							Mux(RegNext(replaced)&&rp===i.U,false.B,valid(i)(j)(vld_idx(log2Ceil(nSets)-1,0))))
			
		}
	}

////////////////////////////////DATA RAM////////////////////////////
	for( i <- 0 until data.length){//way
		for(j  <- 0 until data.head.length){//banknum

			val s0_renOH   = (0 until numReadport).map{idx=>
				s0_rmsg(idx).bits.bank===j.U&&s0_rmsg(idx).valid
			}
			val data_wvalid = s1_wvalid&&s1_wmask(j)=/=0.U
			val s0_ridx = Mux1H(s0_renOH,s0_rmsg.map(_.bits.index))
			val s0_widx = s0_wmsg.bits.index
			val data_idx = Mux(refilled,refillMsg.index,
							Mux(replaced,replaceMsg.idx,
							Mux(s1_wvalid,s1_wmsg.bits.index,Mux(s0_renOH.reduce(_||_),s0_ridx,s0_widx))))
			dontTouch(s0_ridx)
			val enable  = refilled||((s0_rvalid.reduce(_||_)&&s0_renOH.reduce(_||_)))||s1_wvalid||replaced||s0_wvalid
			val write	= refilled&&(i.U===refillWay)||data_wvalid&&s1_whit(i)
			val w_data 	= Mux(refilled,refillData(j),s1_final_wdata(j))
			data(i)(j).io.enable := enable
			data(i)(j).io.addr   := data_idx
			data(i)(j).io.write  := write
			data(i)(j).io.dataIn := w_data
			for(k <- 0 until numReadport){
				rdata(k)(i)(j)       := data(i)(j).io.dataOut
			}
		}
	}
	dontTouch(rdata)
///////////////////////////////Meta//////////////////////////////
//目前这个写逻辑有问题，只有当写数据，或者写miss才会设定dirty位，读miss读出的数据是不会设置dirty位的，但目前不影响功能
	for( i <- 0 until meta.length){
		for(j  <- 0 until meta.head.length){
			val meta_idx = Mux(refilled,refillMsg.index,
							Mux(replaced,replaceMsg.idx,
							Mux(s1_wvalid,s1_wmsg.bits.index,s0_rmsg(j).bits.index)))
			val enable  = refilled||(s1_wvalid||s0_rvalid.reduce(_||_))||replaced
			val write	= refilled&&(i.U===refillWay)||s1_wvalid&&s1_whit(i)
			val w_meta 	= refilled||s1_wvalid&&s1_whit(i)
			meta(i)(j).io.enable := enable
			meta(i)(j).io.addr   := meta_idx
			meta(i)(j).io.write  := write 
			meta(i)(j).io.dataIn.dirty := w_meta
			rmeta(i)(j)  		:= meta(i)(j).io.dataOut
			
		}
	}




    override def toString: String = GRVString(
    "==L1-DCache==",
    "Block bytes   : " + blockBytes,
    "Row bits      : " + rowBits,
    "Word bits     : " + XLEN,
    "Sets          : " + nSets,
    "Ways          : " + nWays,
    "RAMs          : (" +  blockBytes/bankNum + " x " + nSets + ") using " + bankNum + " banks"+"\n")
}