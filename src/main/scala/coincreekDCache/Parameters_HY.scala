package coincreekDCache

import chisel3._
import chisel3.util._

trait Parameters_HY {
  val tagWidth = 32

  val mshrEntryDataNum   = (mshrMaskBusWidth + mshrDataBusWidth) / mshrEntryDataWidth
  val mshrEntryDataWidth = 64

  val mshrEntryNum     = 8
  val mshrMaskBusWidth = 64
  val mshrDataBusWidth = 512

  val mshrTypeTag = 2
  // bit 0: 1 for write & 0 for read
  // bit 1: 1 for vector & 0 for scalar
}
