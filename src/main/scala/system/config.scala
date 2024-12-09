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
    BIMParams:Option[BIMParams] = Some(new BIMParams) ,
    UBTBParams:Option[MicroBTBParams] = Some(new MicroBTBParams) 
    // branchPredictor: Function2[BranchPredictionBankResponse, Parameters, Tuple2[Seq[BranchPredictorBank], BranchPredictionBankResponse]] = ((resp_in: BranchPredictionBankResponse, p: Parameters) => (Nil, resp_in))
    ) {
}


trait HasGRVParameters{
    implicit val p:Parameters
    ///core
    val CoreParams = p(CoreKey)
    val XLEN        = CoreParams.XLEN
    val globalHistoryLength = CoreParams.globalHistoryLength
    val bpdMaxMetaLength = CoreParams.bpdMaxMetaLength
    val bimParams:Option[BIMParams]        = (CoreParams.BIMParams)
    val ubtbParams:Option[MicroBTBParams]  = CoreParams.UBTBParams
    // def getBPDComponents(resp_in: BranchPredictionBankResponse, p: Parameters) = {
    //     CoreParams.branchPredictor(resp_in, p)
    // }
    //ICache
    val ICacheParams = p(ICacheKey)
    val nSets        = ICacheParams.nSets      
    val nWays        = ICacheParams.nWays     
    val rowBits      = ICacheParams.rowBits   
    val prefetch     = ICacheParams.prefetch  
    val blockBytes   = ICacheParams.blockBytes
    val fetchBytes   = ICacheParams.fetchBytes
    val fetchWidth   = blockBytes/(XLEN/8)
    def fetchIdx(addr: UInt)  = addr >> log2Ceil(fetchBytes)
    
}
class BaseConfig extends Config((site, here, up) => {
    case CoreKey => CoreParams()
    case ICacheKey => ICacheParams()
})