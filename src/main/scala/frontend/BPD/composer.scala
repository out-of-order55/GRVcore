package grvcore
import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
// class Composer(implicit p: Parameters) extends BasePredictor
// {

//     val (components, resp) = getBPDComponents(io.resp_in, p)
//     io.resp := resp


//     var metas = 0.U(1.W)
//     var meta_sz = 0
//     for (c <- components) {
//         c.io.f0_valid  := io.f0_valid
//         c.io.f0_pc     := io.f0_pc
//         c.io.f1_ghist  := io.f1_ghist
//         c.io.f3_fire   := io.f3_fire
//         if (c.metaSz > 0) {
//         metas = (metas << c.metaSz) | c.io.f3_meta(c.metaSz-1,0)
//         }
//         meta_sz = meta_sz + c.metaSz
//     }
//     require(meta_sz < bpdMaxMetaLength)
//     io.f3_meta := metas


//     var update_meta = io.update.bits.meta
//     for (c <- components.reverse) {
//         c.io.update := io.update
//         c.io.update.bits.meta := update_meta
//         update_meta = update_meta >> c.metaSz
//     }

//     // val mems = components.map(_.mems).flatten

// }
