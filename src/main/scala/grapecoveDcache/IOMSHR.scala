package grapecoveDcache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import _root_.circt.stage.ChiselStage
import grapecoveDCache._

class IOMSHR(id: Int)(
    implicit edge: TLEdgeOut
) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new MainPipeReq))

  })

  val mode_idle :: mode_req_enqueue :: mode_resp_wait :: mode_clear :: Nil = Enum(3)
}
