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
    XLEN:Int =32
    ) {
}

// trait HasCoreParameters {
//     implicit val p:Parameters
//     val CoreParams = p(CoreKey)
//     val XLEN        = CoreParams.XLEN      
// }
trait HasGRVParameters{
    implicit val p:Parameters
    ///core
    val CoreParams = p(CoreKey)
    val XLEN        = CoreParams.XLEN
    //ICache
    val ICacheParams = p(ICacheKey)
    val nSets        = ICacheParams.nSets      
    val nWays        = ICacheParams.nWays     
    val rowBits      = ICacheParams.rowBits   
    val prefetch     = ICacheParams.prefetch  
    val blockBytes   = ICacheParams.blockBytes
    val fetchBytes   = ICacheParams.fetchBytes
}
class BaseConfig extends Config((site, here, up) => {
    case CoreKey => CoreParams()
    case ICacheKey => ICacheParams()
})