package coincreekDCache

import chisel3._
import chisel3.util._

trait DCacheParams {
  // {{{
  val nSets             = 128
  val nWays             = 4
  val rowBits           = 64
  val blockBytes        = 64
  val nMSHRs            = 8
  val replacementPolicy = "plru"
  // }}}

  // {{{
  val vaddrWidth = 39
  val paddrWidth = 39
  val srcWidth   = 8
  val dataWidth  = 64
  // }}}

}

object DCacheParams extends DCacheParams
