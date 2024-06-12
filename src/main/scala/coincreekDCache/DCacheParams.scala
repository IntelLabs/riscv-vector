package coincreekDCache

import chisel3._
import chisel3.util._

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

  // {{{
  val XLEN       = 64
  val vaddrWidth = 39
  val paddrWidth = 39
  val srcWidth   = 8
  val dataWidth  = 64
  // }}}

  val rowBytes     = rowBits / 8
  val rowOffBits   = log2Up(rowBytes)
  val blockOffBits = log2Up(blockBytes)
  val setIdxBits   = log2Up(nSets)
  val bankIdxBits  = log2Up(nBanks)
  val wordIdxBits  = blockOffBits - (bankIdxBits + rowOffBits)
  val untagBits    = blockOffBits + setIdxBits

  val rowWords = rowBits / XLEN

  assert(wordIdxBits >= 0)

}
