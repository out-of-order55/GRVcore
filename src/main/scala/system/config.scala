package grvcore
import chisel3._
import chisel3.util._
// import scala.util.matching.Regex
import freechips.rocketchip.amba._
import freechips.rocketchip.diplomacy._
///////////////////////////Parameters//////////////////////
import org.chipsalliance.cde.config._

case object CoreKey extends Field[CoreParams]
case class CoreParams(
    XLEN:Int =32,
    globalHistoryLength:Int = 10,
    bpdMaxMetaLength:Int = 64,
    numBr:Int = 1,
    numRAS:Int = 8,
    BIMParams:Option[BIMParams] = Some(new BIMParams) ,
    UBTBParams:Option[MicroBTBParams] = Some(new MicroBTBParams) ,
    btbParams:Option[BTBParams] = Some(new BTBParams),
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


trait HasGRVParameters{
    implicit val p:Parameters
    ///core
    val CoreParams = p(CoreKey)
    val XLEN        = CoreParams.XLEN
    val globalHistoryLength = CoreParams.globalHistoryLength
    val bpdMaxMetaLength = CoreParams.bpdMaxMetaLength
    val numRAS  = CoreParams.numRAS
    val bimParams:Option[BIMParams]        = (CoreParams.BIMParams)
    val ubtbParams:Option[MicroBTBParams]  = CoreParams.UBTBParams
    val btbParams:Option[BTBParams] = CoreParams.btbParams
    def getBPDComponents(resp_in: BPResp, p: Parameters) = {
        CoreParams.branchPredictor(resp_in, p)
    }
    //ICache
    val ICacheParams = p(ICacheKey)
    val nSets        = ICacheParams.nSets      
    val nWays        = ICacheParams.nWays     
    val rowBits      = ICacheParams.rowBits   
    val prefetch     = ICacheParams.prefetch  
    val blockBytes   = ICacheParams.blockBytes
    val fetchBytes   = ICacheParams.fetchBytes
    val fetchWidth   = blockBytes/(XLEN/8)
    def fetchIdx(addr: UInt)  = addr >> log2Ceil(blockBytes)


    
}
class BaseConfig extends Config((site, here, up) => {
    case CoreKey => CoreParams()
    case ICacheKey => ICacheParams()
})
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