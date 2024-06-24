package coincreekDCache.util

import chisel3._
import chisel3.util._

class StoreGen(typ: UInt, addr: UInt, dat: UInt, maxSize: Int) {
  val size = Wire(UInt(log2Up(log2Up(maxSize) + 1).W))
  size := typ

  def misaligned: Bool =
    (addr & ((1.U << size).asUInt - 1.U)(log2Up(maxSize) - 1, 0)).orR

  def mask = {
    var res = 1.U
    for (i <- 0 until log2Up(maxSize)) {
      val upper = Mux(addr(i), res, 0.U) | Mux(size >= (i + 1).U, ((BigInt(1) << (1 << i)) - 1).U, 0.U)
      val lower = Mux(addr(i), 0.U, res)
      res = Cat(upper, lower)
    }
    res
  }

  def genData(i: Int): UInt =
    if (i >= log2Up(maxSize)) dat
    else Mux(size === i.U, Fill(1 << (log2Up(maxSize) - i), dat((8 << i) - 1, 0)), genData(i + 1))

  def data     = genData(0)
  def wordData = genData(2)
}

class LoadGen(typ: UInt, signed: Bool, addr: UInt, dat: UInt, zero: Bool, maxSize: Int) {
  private val size = new StoreGen(typ, addr, dat, maxSize).size

  def genData(logMinSize: Int): UInt = {
    var res = dat
    for (i <- log2Up(maxSize) - 1 to logMinSize by -1) {
      val pos     = 8 << i
      val shifted = Mux(addr(i), res(2 * pos - 1, pos), res(pos - 1, 0))
      val doZero  = (i == 0).B && zero
      val zeroed  = Mux(doZero, 0.U, shifted)
      res = Cat(
        Mux(size === i.U || doZero, Fill(8 * maxSize - pos, signed && zeroed(pos - 1)), res(8 * maxSize - 1, pos)),
        zeroed
      )
    }
    res
  }

  def wordData = genData(2)
  def data     = genData(0)
}
