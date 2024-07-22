package grapecoveDCache

import chisel3._
import chisel3.util._

object toInt {
  def apply(in: UInt, range: Int = 256): Int = {
    var out: Int = 0
    for (i <- 0 to range)
      when(i.asUInt === in) {
        out = i
      }
    out
  }
}

object AddrDecoder {
  def getLineAddr(addr: UInt): UInt =
    addr(addr.getWidth - blockOffBits - 1, blockOffBits)

  def getBlockOffset(addr: UInt): UInt =
    addr(blockOffBits - 1, 0)

  def getBankIdx(addr: UInt): UInt =
    addr(bankIdxBits + rowOffBits - 1, rowOffBits)

  def getSetIdx(addr: UInt): UInt =
    addr(setIdxBits + blockOffBits - 1, blockOffBits)

  def getRowIdx(addr: UInt): UInt =
    addr(rowIdxBits + bankIdxBits + rowOffBits - 1, bankIdxBits + rowOffBits)

  def getTag(addr: UInt): UInt =
    addr(addr.getWidth - 1, untagBits)
}
