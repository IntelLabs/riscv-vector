package darecreek.util

import chisel3._
import chisel3.util._

object ReadRegFile {
  def oneHot(rf: Seq[UInt], addr: UInt) = {
    val size = rf.size
    require(addr.getWidth == log2Up(size), "Error: registers (RF) size/width unmatched!")
    val addrOH = Seq.tabulate(size)(i => addr === i.U)
    Mux1H(addrOH, rf)
  }
}

object WriteRegFile {
  // Priority write. Note: wren(0) has the lowest priority.
  def priority(wren: Seq[Bool], wraddr: Seq[UInt], wrdata: Seq[UInt], self: UInt, selfIdx: Int) = {
    val wrHit = wraddr zip wren map {case (addr, wren) => addr === selfIdx.U && wren}
    MuxCase(self, (wrHit zip wrdata).reverse)
  }
  def oneHot(wren: Seq[Bool], wraddr: Seq[UInt], wrdata: Seq[UInt], self: UInt, selfIdx: Int) = {
    val wrHit = wraddr zip wren map {case (addr, wren) => addr === selfIdx.U && wren}
    Mux(wrHit.reduce(_ || _), Mux1H(wrHit, wrdata), self)
  }
  // One-hot wraddr (wraddrOH.size = num_write_ports)
  def oneHot[T <: Data](wren: Seq[Bool], wraddrOH: Seq[Bool], wrdata: Seq[T], self: T) = {
    val wrHit = wraddrOH zip wren map {case (addrOH, wren) => addrOH && wren}
    Mux(wrHit.reduce(_ || _), Mux1H(wrHit, wrdata), self)
  }
}

object CircularShift {
  def left(data: UInt, offset: Int) = {
    val len = data.getWidth
    require(len > offset, "Len should > offset for CircularShift")
    Cat(data(len - offset -1, 0), data(len-1, len - offset))
  }
  def right(data: UInt, offset: Int) = {
    val len = data.getWidth
    require(len > offset, "Len should > offset for CircularShift")
    Cat(data(offset-1, 0), data(len-1, offset))
  }
}
