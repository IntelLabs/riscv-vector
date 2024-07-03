package coincreekDCache

import chisel3._
import chisel3.util._
import scala.math.max
import freechips.rocketchip.tilelink._

trait DCacheParams {
  // {{{
  val nSets             = 128
  val nWays             = 4
  val nBanks            = 8
  val rowBits           = 64
  val blockBytes        = 64
  val nMSHRs            = 4
  val nMMIOs            = 0
  val replacementPolicy = "setplru"
  val beatBytes         = 64
  // }}}

  // lrsc params
  // {{{
  val lrscCycles  = 80
  val lrscBackoff = 3
  // }}}

  // {{{
  val XLEN          = 64
  val VLEN          = 512
  val vaddrWidth    = 39
  val paddrWidth    = 39
  val regAddrWidth  = 5
  val vLsqSizeWidth = 5
  val srcWidth      = 8

  val destWidth = max(regAddrWidth, vLsqSizeWidth)
  val dataWidth = max(XLEN, VLEN)
  val dataBytes = dataWidth / 8

  val cohBits = ClientStates.width // tilelink

  val beatBits     = beatBytes * 8
  val refillCycles = blockBytes / beatBytes
  // }}}

  val rowBytes     = rowBits / 8
  val rowOffBits   = log2Up(rowBytes)
  val blockOffBits = log2Up(blockBytes)
  val setIdxBits   = log2Up(nSets)
  val bankIdxBits  = log2Up(nBanks)
  val rowIdxBits   = blockOffBits - (bankIdxBits + rowOffBits)
  val untagBits    = blockOffBits + setIdxBits
  val tagBits      = paddrWidth - untagBits

  val lineAddrWidth = paddrWidth - blockOffBits

  val rowWords = rowBits / XLEN

  assert(rowIdxBits >= 0)

  // MSHR
  // {{{
  val mshrEntryMetaNum = 8

  val mshrEntryNum = 8
  // Mask Data definition
  // scalar write: 2 (meta) + 2 (typ for max 8 bytes) + 6 (index for 64 Bytes) = 10
  // scalar load:  2 (meta) + 5 (regAddr for 32 regs) + 2 (typ) + 1 (signed) + 6 (index for 64 Bytes) = 16
  // vector write: 2 (meta) + 3 (typ for max 64 bytes) + 6 (index for 64 bytes) = 11
  // vector load:  2 (meta) + 5 (regAddr for 32 regs) + 3 (typ) + 1 (signed) +  6 (index for 64 Bytes) = 17
  // typ: 0->1B, 1->2B, 2->4B, 3->8B, ... , 6->64B
  //  val mshrMetaWidth = 16
  val mshrMaskWidth = 64
  val mshrDataWidth = 512

  val sizeMax         = 6
  val dataOffsetWidth = log2Up(mshrDataWidth / 8)

  val mshrType = 1
  // 1 for write & 0 for read

  val mshrReqType = 3
  // One Hot codeing
  // bit 0: allocate req
  // bit 1: replay req
  // bit 2: probe req

}

object MasterSource {
  val width = 2

  def Core   = 0.U(width.W)
  def Vector = 2.U(width.W)
}
