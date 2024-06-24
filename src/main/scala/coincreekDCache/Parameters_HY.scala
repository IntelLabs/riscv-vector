package coincreekDCache

import chisel3._
import chisel3.util._

trait Parameters_HY {
  val regAddrWidth = 5
  val regDataWidth = 64
  val tagWidth     = 32

  val mshrEntryDataWidth = 64
  val mshrEntryMetaNum   = 16 // should be bigger than mshrEntryDataNum
  val mshrEntryDataNum   = 8  // mshrMetaBusWidth / mshrEntryDataWidth

  val mshrEntryNum = 8
  // Mask Data definition
  // scalar write: 1 (1 for write) + 2 (typ for max 8 bytes) + 6 (index for 64 Bytes) = 9
  // scalar load:  1 (0 for read ) + 5 (regAddr for 32 regs) + 2 (typ) + 1 (signed) + 6 (index for 64 Bytes) = 15
  // vector write: 1 (1 for write) + 3 (typ for max 64 bytes) + 6 (index for 64 bytes) = 10
  // vector load:  1 (0 for read ) + 5 (regAddr for 32 regs) + 3 (typ) + 1 (signed) +  6 (index for 64 Bytes) = 16
  // typ: 0->1B, 1->2B, 2->4B, 3->8B, ... , 6->64B
  val mshrMetaBusWidth = 16
  val mshrDataBusWidth = 512

  val typMax         = 6
  val typWidth       = log2Up(log2Up(mshrDataBusWidth / 8) + 1)
  val dataIndexWidth = log2Up(mshrDataBusWidth / 8)

  val mshrType = 1
  // 1 for write & 0 for read

  val mshrReqType = 3
  // One Hot codeing
  // bit 0: allocate req
  // bit 1: replay req
  // bit 2: probe req
}
