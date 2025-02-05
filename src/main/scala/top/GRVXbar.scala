package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink.{TLArbiter, TLXbar, TLFilter, TLFuzzer, TLToAXI4, TLRAMModel}
// import freechips.rocketchip.util.Annotated.resetVector
import freechips.rocketchip.util.{DontTouch,BundleField}
// import freechips.rocketchip.util.BundleField
class   GRVXbar(
    arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
    maxFlightPerId:    Int = 7,
    awQueueDepth:      Int = 2)(implicit p: Parameters) extends LazyModule
{
    require (maxFlightPerId >= 1)
    require (awQueueDepth >= 1)

    val node = new AXI4NexusNode(
        masterFn  = { seq =>
        seq(0).copy(
            echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
            requestFields = BundleField.union(seq.flatMap(_.requestFields)),
            responseKeys  = seq.flatMap(_.responseKeys).distinct,
            masters = (GRVXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
            port.masters map { master => master.copy(id = master.id.shift(range.start)) }
            }
        )
        },
        slaveFn = { seq =>
        seq(0).copy(
            responseFields = BundleField.union(seq.flatMap(_.responseFields)),
            requestKeys    = seq.flatMap(_.requestKeys).distinct,
            minLatency = seq.map(_.minLatency).min,
            slaves = seq.flatMap { port =>
            require (port.beatBytes == seq(0).beatBytes,
                s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
            port.slaves
            }
        )
        }
    ){
        override def circuitIdentity = outputs == 1 && inputs == 1
    }

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val (io_in, edgesIn) = node.in.unzip
        val (io_out, edgesOut) = node.out.unzip

        // Grab the port ID mapping
        val inputIdRanges = GRVXbar.mapInputIds(edgesIn.map(_.master))

        // Find a good mask for address decoding
        val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
        val routingMask = AddressDecoder(port_addrs)
        val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
        val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))
    
        // To route W we need to record where the AWs went
        val awIn  = Seq.fill(io_in .size) { Module(new Queue(UInt(io_out.size.W), awQueueDepth, flow = true)) }
        val awOut = Seq.fill(io_out.size) { Module(new Queue(UInt(io_in .size.W), awQueueDepth, flow = true)) }

        val valid_in = Seq(io_in.map(_.aw.valid),io_in.map(_.ar.valid)).flatten
        val block    = WireInit(VecInit(PriorityEncoderOH(valid_in)))
        val block_idx= PriorityEncoder(VecInit(valid_in))
        val block_reg = RegInit(VecInit.fill(block.size)(false.B))
        val block_idx_reg = RegInit(block_idx)
        val brust_vec  = WireInit(VecInit(Seq(io_in.map{i=>i.aw.bits.burst=/="b11".U},io_in.map{i=>i.ar.bits.burst=/="b11".U}).flatten))
        val brust_enable = RegInit(false.B)

        val requestARIO = WireInit(VecInit(io_in.map  { i => VecInit(outputPorts.map   { o => o(i.ar.bits.addr) }) }))
        val requestAWIO = io_in.map  { i => VecInit(outputPorts.map   { o => o(i.aw.bits.addr) }) }
        val req_r = WireInit(VecInit((io_in.size until 2*io_in.size).map(i=>(block_reg(i)))))
        val requestROI  = WireInit(VecInit(io_out.map { o =>req_r}))
        val requestBOI  = io_out.map { o => (0 until io_in.size).map(i=>block_reg(i))  }
        dontTouch((requestARIO))
        dontTouch((requestROI))
        // W follows the path dictated by the AW Q
        for (i <- 0 until io_in.size) { awIn(i).io.enq.bits := requestAWIO(i).asUInt }
        val requestWIO = awIn.map { q => if (io_out.size > 1) q.io.deq.bits.asBools else Seq(true.B) }

        // We need an intermediate size of bundle with the widest possible identifiers
        val wide_bundle = AXI4BundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

        // Transform input bundles
        val in = Wire(Vec(io_in.size, new AXI4Bundle(wide_bundle)))

        dontTouch(block)
        dontTouch(block_idx)
        dontTouch(brust_enable)
        dontTouch(block_reg)
        dontTouch(in)
        when(block_idx_reg<(in.size).U){
            when(in(block_idx_reg).b.fire&&block_reg(block_idx_reg)){
                block_reg(block_idx_reg) := false.B
                block_idx_reg := 0.U
            }
        }.otherwise{
            when(in(block_idx_reg).r.fire&&(!brust_enable)&&block_reg(block_idx_reg)){
                block_idx_reg := 0.U
                block_reg(block_idx_reg) := false.B
            }.elsewhen(in(block_idx_reg).r.bits.last&&(brust_enable)&&block_reg(block_idx_reg)){
                block_reg(block_idx_reg) := false.B
                brust_enable := false.B
                block_idx_reg := 0.U
            }
        }
        //porblem
        when(block_idx<(in.size).U){
            when(in(block_idx).aw.fire&&block(block_idx)){
                block_reg(block_idx) := true.B
                block_idx_reg := block_idx
            }
        }.otherwise{
            when(in(block_idx).ar.fire&&block(block_idx)){
                block_reg(block_idx) := true.B
                brust_enable := brust_vec(block_idx)
                block_idx_reg := block_idx
            }
        }
        //block阻塞同时请求的信号，block_reg阻塞之后请求的信号
        for (i <- 0 until in.size) {
        in(i).aw.bits.user := DontCare
        in(i).aw.bits.echo := DontCare
        in(i).ar.bits.user := DontCare
        in(i).ar.bits.echo := DontCare
        in(i).w.bits.user := DontCare
        in(i).squeezeAll.waiveAll :<>= io_in(i).squeezeAll.waiveAll

        // Handle size = 1 gracefully (Chisel3 empty range is broken)
        def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)
        // Manipulate the AXI IDs to differentiate masters
        val r = inputIdRanges(i)
        
        // in(i).ar.valid   := io_in(i).ar.valid
        in(i).aw.bits.id := io_in(i).aw.bits.id | (r.start).U
        in(i).ar.bits.id := io_in(i).ar.bits.id | (r.start).U
        io_in(i).r.bits.id := trim(in(i).r.bits.id, r.size)
        io_in(i).b.bits.id := trim(in(i).b.bits.id, r.size)

        if (io_out.size > 1) {
            // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
            val endId = edgesIn(i).master.endId
            val arFIFOMap = WireDefault(VecInit.fill(endId) { true.B })
            val awFIFOMap = WireDefault(VecInit.fill(endId) { true.B })
            val arSel = UIntToOH(io_in(i).ar.bits.id, endId)
            val awSel = UIntToOH(io_in(i).aw.bits.id, endId)
            val rSel  = UIntToOH(io_in(i).r .bits.id, endId)
            val bSel  = UIntToOH(io_in(i).b .bits.id, endId)
            val arTag = OHToUInt(requestARIO(i).asUInt, io_out.size)
            val awTag = OHToUInt(requestAWIO(i).asUInt, io_out.size)

            for (master <- edgesIn(i).master.masters) {
            def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
                if (master.maxFlight == Some(0)) {
                true.B
                } else {
                val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
                val flight = legalFlight min maxFlightPerId
                val canOverflow = legalFlight > flight
                val count = RegInit(0.U(log2Ceil(flight+1).W))
                val last = Reg(UInt(log2Ceil(io_out.size).W))
                count := count + req_fire.asUInt - resp_fire.asUInt
                assert (!resp_fire || count =/= 0.U)
                assert (!req_fire  || count =/= flight.U)
                when (req_fire) { last := port }
                // No need to track where it went if we cap it at 1 request
                val portMatch = if (flight == 1) { true.B } else { last === port }
                (count === 0.U || portMatch) && ((!canOverflow).B || count =/= flight.U)
                }
            }

            for (id <- master.id.start until master.id.end) {
                arFIFOMap(id) := idTracker(
                arTag,
                arSel(id) && io_in(i).ar.fire,
                rSel(id) && io_in(i).r.fire && io_in(i).r.bits.last)
                awFIFOMap(id) := idTracker(
                awTag,
                awSel(id) && io_in(i).aw.fire,
                bSel(id) && io_in(i).b.fire)
            }
            }

            val allowAR = arFIFOMap(io_in(i).ar.bits.id)
            val block_ar = WireInit(block(i+io_in.size)&&(!block_reg.reduce(_||_))||block_reg(i+io_in.size))
            dontTouch(block_ar)
            in(i).ar.valid := io_in(i).ar.valid && allowAR&&block_ar
            
            io_in(i).ar.ready := in(i).ar.ready && allowAR&&block_ar

            // Keep in mind that slaves may do this: awready := wvalid, wready := awvalid
            // To not cause a loop, we cannot have: wvalid := awready

            // Block AW if we cannot record the W destination
            val allowAW = awFIFOMap(io_in(i).aw.bits.id)
            val latched = RegInit(false.B) // cut awIn(i).enq.valid from awready
            in(i).aw.valid := io_in(i).aw.valid && (latched || awIn(i).io.enq.ready) && allowAW&&(block(i)&&(!block_reg.reduce(_||_))||block_reg(i))
            io_in(i).aw.ready := in(i).aw.ready && (latched || awIn(i).io.enq.ready) && allowAW&&(block(i)&&(!block_reg.reduce(_||_))||block_reg(i))
            awIn(i).io.enq.valid := io_in(i).aw.valid && !latched
            when (awIn(i).io.enq.fire) { latched := true.B }
            when (in(i).aw.fire) { latched := false.B }

            // Block W if we do not have an AW destination
            in(i).w.valid := io_in(i).w.valid && awIn(i).io.deq.valid // depends on awvalid (but not awready)
            io_in(i).w.ready := in(i).w.ready && awIn(i).io.deq.valid
            awIn(i).io.deq.ready := io_in(i).w.valid && io_in(i).w.bits.last && in(i).w.ready
        } else {
            val block_ar = WireInit(block(i+io_in.size)&&(!block_reg.reduce(_||_))||block_reg(i+io_in.size))
            dontTouch(block_ar)
            in(i).ar.valid := io_in(i).ar.valid &&block_ar
            io_in(i).ar.ready := in(i).ar.ready &&block_ar
            in(i).aw.valid := io_in(i).aw.valid &&(block(i)&&(!block_reg.reduce(_||_))||block_reg(i))
            io_in(i).aw.ready := in(i).aw.ready &&(block(i)&&(!block_reg.reduce(_||_))||block_reg(i))
            awIn(i).io := DontCare // aw in queue is not used when outsize == 1
        }
    }
        // Transform output bundles
        val out = Wire(Vec(io_out.size, new AXI4Bundle(wide_bundle)))
        for (i <- 0 until out.size) {
            out(i).b.bits.user := DontCare
            out(i).r.bits.user := DontCare
            io_out(i).squeezeAll.waiveAll :<>= out(i).squeezeAll.waiveAll

            if (io_in.size > 1) {
                // Block AW if we cannot record the W source
                val latched = RegInit(false.B) // cut awOut(i).enq.valid from awready
                io_out(i).aw.valid := out(i).aw.valid && (latched || awOut(i).io.enq.ready)
                out(i).aw.ready := io_out(i).aw.ready && (latched || awOut(i).io.enq.ready)
                awOut(i).io.enq.valid := out(i).aw.valid && !latched
                when (awOut(i).io.enq.fire) { latched := true.B }
                when (out(i).aw.fire) { latched := false.B }

                // Block W if we do not have an AW source
                io_out(i).w.valid := out(i).w.valid && awOut(i).io.deq.valid // depends on awvalid (but not awready)
                out(i).w.ready := io_out(i).w.ready && awOut(i).io.deq.valid
                awOut(i).io.deq.ready := out(i).w.valid && out(i).w.bits.last && io_out(i).w.ready
            } else {
                awOut(i).io := DontCare // aw out queue is not used when io_in.size == 1
            }
        }
    
        // Fanout the input sources to the output sinks
        def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
        val portsAROI = transpose((in  zip requestARIO) map { case (i, r) => GRVXbar.fanout(i.ar, r) })
        val portsAWOI = transpose((in  zip requestAWIO) map { case (i, r) => GRVXbar.fanout(i.aw, r) })
        val portsWOI  = transpose((in  zip requestWIO)  map { case (i, r) => GRVXbar.fanout(i.w,  r) })
        val respRIO  = transpose(requestROI)
        val respBIO  = transpose(requestBOI)

        // Arbitrate amongst the sources
        for (o <- 0 until out.size) {
            awOut(o).io.enq.bits := // Record who won AW arbitration to select W
                AXI4Arbiter.returnWinner(arbitrationPolicy)(out(o).aw, portsAWOI(o):_*).asUInt
            AXI4Arbiter(arbitrationPolicy)(out(o).ar, portsAROI(o):_*)
            // W arbitration is informed by the Q, not policy
            out(o).w.valid := Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.valid))
            out(o).w.bits :<= Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.bits))
            portsWOI(o).zipWithIndex.map { case (p, i) =>
                if (in.size > 1) {
                p.ready := out(o).w.ready && awOut(o).io.deq.bits(i)
                } else {
                p.ready := out(o).w.ready
                }
            }
        
            out(o).r.ready  := Mux1H(requestROI(o),in.map(_.r.ready))
            out(o).b.ready  := Mux1H(requestBOI(o),in.map(_.b.ready))
        }

        for (i <- 0 until in.size) {
            in(i).r.bits  := Mux1H(respRIO(i),out.map(_.r.bits))
            in(i).r.valid  := Mux1H(respRIO(i),out.map(_.r.valid))&block_reg(i+in.size)
            in(i).b.bits  := Mux1H(respBIO(i),out.map(_.b.bits))
            in(i).b.valid  := Mux1H(respBIO(i),out.map(_.b.valid))&block_reg(i)
        }
    }
}

object GRVXbar
{
    def apply(
        arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
        maxFlightPerId:    Int = 7,
        awQueueDepth:      Int = 2)(implicit p: Parameters) =
    {
        val grvxbar = LazyModule(new GRVXbar(arbitrationPolicy, maxFlightPerId, awQueueDepth))
        grvxbar.node
    }

    def mapInputIds(ports: Seq[AXI4MasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endId))

    // Replicate an input port to each output port
    def fanout[T <: AXI4BundleBase](input: IrrevocableIO[T], select: Seq[Bool]) = {
        val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
        for (i <- 0 until select.size) {
        filtered(i).bits :<= input.bits
        filtered(i).valid := input.valid && select(i)
        }
        input.ready := Mux1H(select, filtered.map(_.ready))
        filtered
    }
}

