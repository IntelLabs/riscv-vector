package coincreekDCache

import chisel3._
import chisel3.util._
import scala.math.max

trait DCacheParams {
  // {{{
  val nSets             = 128
  val nWays             = 4
  val nBanks            = 8
  val rowBits           = 64
  val blockBytes        = 64
  val nMSHRs            = 8
  val replacementPolicy = "plru"
  // }}}

  // lrsc params
  // {{{
  val lrscCycles  = 80
  val lrscBackoff = 3
  // }}}

  // {{{
  val XLEN       = 64
  val VLEN       = 512
  val vaddrWidth = 39
  val paddrWidth = 39
  val srcWidth   = 8

  val dataWidth = max(XLEN, VLEN)
  val dataBytes = dataWidth / 8
  // }}}

  val rowBytes     = rowBits / 8
  val rowOffBits   = log2Up(rowBytes)
  val blockOffBits = log2Up(blockBytes)
  val setIdxBits   = log2Up(nSets)
  val bankIdxBits  = log2Up(nBanks)
  val rowIdxBits   = blockOffBits - (bankIdxBits + rowOffBits)
  val untagBits    = blockOffBits + setIdxBits

  val rowWords = rowBits / XLEN

  assert(rowIdxBits >= 0)

}
