package grapecoveDCache

import chisel3._
import chisel3.util._
import scala.math.max
import freechips.rocketchip.tilelink._

trait DCacheParams {
  // {{{ cache config params
  val nSets               = 128
  val nWays               = 4
  val nBanks              = 8
  val rowBytes            = 8
  val blockBytes          = 64
  val nMSHRs              = 8
  val nMSHRMetas          = 8 // number of metas each MSHR entry can hold
  val nMMIOs              = 1
  val nWBQEntries         = 2 // writeback queue entries
  val nRefillQDataEntries = 8 // refill queue data entries
  val nRefillQPermEntries = 8 // refill queue permission entries
  val replacementPolicy   = "setlru"
  val beatBytes           = 64

  // calculate
  val rowBits      = rowBytes * 8
  val blockBits    = blockBytes * 8
  val beatBits     = beatBytes * 8
  val refillCycles = blockBytes / beatBytes
  // assert(nWBQEntries > nMSHRs)
  // }}}

  // {{{
  val XLEN      = 64
  val VLEN      = 512
  val dataWidth = max(XLEN, VLEN)
  val dataBytes = dataWidth / 8

  val vaddrWidth = 64
  val paddrWidth = 39

  val regAddrWidth  = 5
  val vLsqSizeWidth = 5
  val destWidth     = max(regAddrWidth, vLsqSizeWidth)
  // }}}

  val rowOffBits   = log2Up(rowBytes)
  val blockOffBits = log2Up(blockBytes)
  val setIdxBits   = log2Up(nSets)
  val bankIdxBits  = log2Up(nBanks)
  val rowIdxBits   = blockOffBits - (bankIdxBits + rowOffBits)
  val untagBits    = blockOffBits + setIdxBits
  val tagBits      = paddrWidth - untagBits
  val cohBits      = ClientStates.width // tilelink

  val lineAddrWidth = paddrWidth - blockOffBits

  val rowWords = rowBits / XLEN

  assert(rowIdxBits >= 0)

  // lrsc params
  // {{{
  val lrscCycles  = 80
  val lrscBackoff = 3
  // }}}

  // {{{ TODO: move
  // 1 for write & 0 for read
  val mshrType = 1

  // One Hot codeing
  // bit 0: allocate req
  // bit 1: replay req
  // bit 2: probe req
  val mshrReqType = 3
  // }}}
}
