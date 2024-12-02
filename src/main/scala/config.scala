package Cache
import chisel3._
import chisel3.util._

case class CoreConfig(xlen:Int,IsDiff:Boolean,IsPref:Boolean)
case class CacheConfig(nWays: Int, nSets: Int, blockBytes: Int)
case class Config(core: CoreConfig, cache: CacheConfig)


object grvcoreConfig {
    def apply(): Config = {
        val xlen = 32
        Config(
            core = CoreConfig(
                xlen = xlen,
                IsDiff = false,
                IsPref = false
            ),
            cache = CacheConfig(
                nWays = 2,
                nSets = 256,
                blockBytes = 4 * (xlen / 8)
            ),
        )
    }
}