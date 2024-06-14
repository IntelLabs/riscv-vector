package coincreekDCache

import chisel3._
import chisel3.util._

trait Parameters_HY {
  val regAddrWidth = 5
  val regDataWidth = 64
  val tagWidth     = 32

  val mshrEntryDataNum   = (mshrMaskBusWidth + mshrDataBusWidth) / mshrEntryDataWidth
  val mshrEntryDataWidth = 64

  val mshrEntryNum     = 8
  val mshrMaskBusWidth = 64
  val mshrDataBusWidth = 512

  val mshrTypeTag = 2
  // bit 0: 1 for write & 0 for read
  // bit 1: 1 for vector & 0 for scalar

  val mshrReqType = 3
  // One Hot codeing
  // bit 0: allocate req
  // bit 1: replay req
  // bit 2: probe req
}
