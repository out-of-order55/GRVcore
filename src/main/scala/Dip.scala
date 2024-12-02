// package ysyx

// import chisel3._
// import chisel3.experimental.SourceInfo
// import chisel3.util.random.FibonacciLFSR

// import circt.stage.ChiselStage
// import org.chipsalliance.cde.config.Parameters

// import freechips.rocketchip.system.DefaultConfig
// import freechips.rocketchip.diplomacy._

// case class UpwardParam(width: Int)
// case class DownwardParam(width: Int)
// case class EdgeParam(width: Int)
// // 
// // PARAMETER TYPES:                       D              U            E          B
// object AdderNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {
//   def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) = {
//     if (pd.width < pu.width) EdgeParam(pd.width) else EdgeParam(pu.width)
//   }
//   def bundle(e: EdgeParam) = UInt(e.width.W)
//   def render(e: EdgeParam) = RenderedEdge("blue", s"width = ${e.width}")
// }

// /** node for [[AdderDriver]] (source) */
// class AdderDriverNode(widths: Seq[DownwardParam])(implicit valName: ValName)
//   extends SourceNode(AdderNodeImp)(widths)

// /** node for [[AdderMonitor]] (sink) */
// class AdderMonitorNode(width: UpwardParam)(implicit valName: ValName)
//   extends SinkNode(AdderNodeImp)(Seq(width))

// /** node for [[Adder]] (nexus) */
// class AdderNode(dFn: Seq[DownwardParam] => DownwardParam,
//                 uFn: Seq[UpwardParam] => UpwardParam)(implicit valName: ValName)
//   extends NexusNode(AdderNodeImp)(dFn, uFn)

// /** adder DUT (nexus) */
// class Adder(implicit p: Parameters) extends LazyModule {
//   val node = new AdderNode (
//     { case dps: Seq[DownwardParam] =>
//       require(dps.forall(dp => dp.width == dps.head.width), "inward, downward adder widths must be equivalent")
//       dps.head
//     },
//     { case ups: Seq[UpwardParam] =>
//       require(ups.forall(up => up.width == ups.head.width), "outward, upward adder widths must be equivalent")
//       ups.head
//     }
//   )
//   lazy val module = new LazyModuleImp(this) {
//     require(node.in.size >= 2)
//     node.out.head._1 := node.in.unzip._1.reduce(_ + _)
//   }

//   override lazy val desiredName = "Adder"
// }
// /** driver (source)
//   * drives one random number on multiple outputs */
// class AdderDriver(width: Int, numOutputs: Int)(implicit p: Parameters) extends LazyModule {
//   val node = new AdderDriverNode(Seq.fill(numOutputs)(DownwardParam(width)))

//   lazy val module = new LazyModuleImp(this) {
//     // check that node parameters converge after negotiation
//     val negotiatedWidths = node.edges.out.map(_.width)
//     require(negotiatedWidths.forall(_ == negotiatedWidths.head), "outputs must all have agreed on same width")
//     val finalWidth = negotiatedWidths.head

//     // generate random addend (notice the use of the negotiated width)
//     val randomAddend = FibonacciLFSR.maxPeriod(finalWidth)

//     // drive signals
//     node.out.foreach { case (addend, _) => addend := randomAddend }
//   }

//   override lazy val desiredName = "AdderDriver"
// }
// /** monitor (sink) */
// class AdderMonitor(width: Int, numOperands: Int)(implicit p: Parameters) extends LazyModule {
//   val nodeSeq = Seq.fill(numOperands) { new AdderMonitorNode(UpwardParam(width)) }
//   val nodeSum = new AdderMonitorNode(UpwardParam(width))

//   lazy val module = new LazyModuleImp(this) {
//     val io = IO(new Bundle {
//       val error = Output(Bool())
//     })

//     // print operation
//     printf(nodeSeq.map(node => p"${node.in.head._1}").reduce(_ + p" + " + _) + p" = ${nodeSum.in.head._1}")

//     // basic correctness checking
//     io.error := nodeSum.in.head._1 =/= nodeSeq.map(_.in.head._1).reduce(_ + _)
//   }

//   override lazy val desiredName = "AdderMonitor"
// }

// /** top-level connector */
// class AdderTestHarness()(implicit p: Parameters) extends LazyModule {
//   val numOperands = 2
//   val adder = LazyModule(new Adder)
//   // 8 will be the downward-traveling widths from our drivers
//   val drivers = Seq.fill(numOperands) { LazyModule(new AdderDriver(width = 8, numOutputs = 2)) }
//   // 4 will be the upward-traveling width from our monitor
//   val monitor = LazyModule(new AdderMonitor(width = 4, numOperands = numOperands))

//   // create edges via binding operators between nodes in order to define a complete graph
//   drivers.foreach{ driver => adder.node := driver.node }

//   drivers.zip(monitor.nodeSeq).foreach { case (driver, monitorNode) => monitorNode := driver.node }
//   monitor.nodeSum := adder.node

//   lazy val module = new LazyModuleImp(this) {
//     // when(monitor.module.io.error) {
//     //   printf("something went wrong")
//     // }
//   }

//   override lazy val desiredName = "AdderTestHarness"
// }

