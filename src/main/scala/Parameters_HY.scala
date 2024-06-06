package coincreekDCache

import chisel3._
import chisel3.util._

trait Parameters_HY {
  val tagWidth = 32

  val mshrEntryDataNum   = 8
  val mshrEntryMaskWidth = 64
  val mshrEntryDataWidth = 512

  val mshrEntryNum = 8
}

object Parameters_HY extends Parameters_HY
