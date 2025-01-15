package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.rocket.MulDivParams

///////////////////////////Parameters//////////////////////
import org.chipsalliance.cde.config._
import grvcore.common._
case object CoreKey extends Field[CoreParams]
case class CoreParams(
    XLEN:Int =32,
    globalHistoryLength:Int = 10,
    bpdMaxMetaLength:Int = 64,
    numBr:Int = 1,
    numRAS:Int = 4,
    coreWidth:Int = 2,
    numLregs:Int  = 32,
    numPregs:Int  = 64,
    ROBEntry:Int  = 16,
	numLDQs	:Int = 8,
	numSTQs :Int = 8,
    numSBs  :Int = 8,
    mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams(divEarlyOut=true)),
    issueParams :Seq[IssueParams] =  Seq(                
        IssueParams(issueWidth=2, numEntries=8, iqType=IQT_MEM.litValue, dispatchWidth=2),
        IssueParams(issueWidth=2, numEntries=8, iqType=IQT_INT.litValue, dispatchWidth=2)),
    iqueueParams:IQueueParams = new IQueueParams,
    ftqParams:FtqParams = new FtqParams,
    BIMParams:Option[BIMParams] = Some(new BIMParams) ,
    UBTBParams:Option[MicroBTBParams] = Some(new MicroBTBParams) ,
    btbParams:Option[BTBParams] = Some(new BTBParams()),
    branchPredictor: (BPResp, Parameters) => Tuple2[Seq[BasePredictor], BPResp] =
        (resp_in: BPResp, p: Parameters) => {
        val ubtb = Module(new MicroBTBBranchPredictor()(p))
        val bim  = Module(new BIMBranchPredictor()(p))
        val preds = Seq(ubtb,bim)
        preds.map(_.io := DontCare)
        ubtb.io.resp_in := resp_in
        bim.io.resp_in  := ubtb.io.resp
        (preds, bim.io.resp)
    }
    ) {

}


trait HasGRVParameters {
    implicit val p:Parameters
    ///core
    val CoreParams = p(CoreKey)
    val XLEN        = CoreParams.XLEN
    val coreWidth   = CoreParams.coreWidth
    val numLregs    = CoreParams.numLregs
    val numPregs    = CoreParams.numPregs
    val globalHistoryLength = CoreParams.globalHistoryLength
    val bpdMaxMetaLength = CoreParams.bpdMaxMetaLength
    val numRAS  = CoreParams.numRAS
    val ftqentries = CoreParams.ftqParams.nEntries
    val iqentries = CoreParams.iqueueParams.nEntries
    val ROBEntry  = CoreParams.ROBEntry
    val issueParams = CoreParams.issueParams
	val numLDQs		 = CoreParams.numLDQs
	val numSTQs		 = CoreParams.numSTQs
    val numSBs	 	 = CoreParams.numSBs
    val mulDivParams= CoreParams.mulDiv.getOrElse(MulDivParams())
    val intIssueParam = issueParams.find(_.iqType == IQT_INT.litValue).get
    val memIssueParam = issueParams.find(_.iqType == IQT_MEM.litValue).get
    val bimParams:Option[BIMParams]        = (CoreParams.BIMParams)
    val ubtbParams:Option[MicroBTBParams]  = CoreParams.UBTBParams
    val btbParams = CoreParams.btbParams.getOrElse(BTBParams())
    def getBPDComponents(resp_in: BPResp, p: Parameters) = {
        CoreParams.branchPredictor(resp_in, p)
    }
    //ICache
    val ICacheParam = p(ICacheKey)


    val DCacheParam = p(DCacheKey)


}
class BaseConfig extends Config((site, here, up) => {
    case CoreKey => CoreParams()
    case ICacheKey => ICacheParams()
    case DCacheKey => DCacheParams()
})
//test ubtb
class TestConfig extends Config(
    new BaseConfig().alter((site, here, up)=>{
        case CoreKey => up(CoreKey).copy(
                branchPredictor = 
                ((resp_in: BPResp, p: Parameters) => {
                    val ubtb = Module(new MicroBTBBranchPredictor()(p))
                    val preds = Seq(ubtb)
                    preds.map(_.io := DontCare)
                    ubtb.io.resp_in := resp_in
                    (preds, ubtb.io.resp)
            })
            
        )
        case ICacheKey => ICacheParams()
}))
//test bim+btb+ras
class Test1Config extends Config(
    new BaseConfig().alter((site, here, up)=>{
        case CoreKey => up(CoreKey).copy(
                branchPredictor = 
                ((resp_in: BPResp, p: Parameters) => {
                    // val ubtb = Module(new MicroBTBBranchPredictor()(p))
                    val bim  = Module(new BIMBranchPredictor()(p))
                    val btb  = Module(new BTBBranchPredictor()(p)) 
                    val preds = Seq(bim,btb)
                    preds.map(_.io := DontCare)
                    bim.io.resp_in := resp_in
                    btb.io.resp_in := bim.io.resp
                    // resp := btb.io.resp
                    (preds, btb.io.resp)
            })
            
        )
        case ICacheKey => ICacheParams()
}))

class DefauConfig extends Config(
    new BaseConfig().alter((site, here, up)=>{
        case CoreKey => up(CoreKey).copy(
                branchPredictor = 
                ((resp_in: BPResp, p: Parameters) => {
                    val ubtb = Module(new MicroBTBBranchPredictor()(p))
                    val bim  = Module(new BIMBranchPredictor()(p))
                    val btb  = Module(new BTBBranchPredictor()(p)) 
                    val preds = Seq(ubtb,bim,btb)
                    preds.map(_.io := DontCare)
                    ubtb.io.resp_in:=resp_in
                    bim.io.resp_in := ubtb.io.resp
                    btb.io.resp_in := bim.io.resp
                    // resp := btb.io.resp
                    (preds, btb.io.resp)
            })
            
        )
        case ICacheKey => ICacheParams()
}))