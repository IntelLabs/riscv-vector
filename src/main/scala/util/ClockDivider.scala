// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.withClock
import chisel3.util.HasBlackBoxResource

/** This black-boxes a Clock Divider by 2.
  * The output clock is phase-aligned to the input clock.
  * If you use this in synthesis, make sure your sdc
  * declares that you want it to do the same.
  *
  * Because Chisel does not support
  * blocking assignments, it is impossible
  * to create a deterministic divided clock.
  */
class ClockDivider2 extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk_out = Output(Clock())
    val clk_in  = Input(Clock())
  })

  addResource("/vsrc/ClockDivider2.v")
}
class ClockDivider3 extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk_out = Output(Clock())
    val clk_in  = Input(Clock())
  })

  addResource("/vsrc/ClockDivider3.v")
}

/** Divide the clock by power of 2 times.
 *  @param pow2 divides the clock 2 ^ pow2 times */
class Pow2ClockDivider(pow2: Int) extends Module {
  val io = IO(new Bundle {
    val clock_out = Output(Clock())
  })

  if (pow2 == 0) {
    io.clock_out := clock
  } else {
    val dividers = Seq.fill(pow2) { Module(new ClockDivider2) }

    dividers.init.zip(dividers.tail).map { case (last, next) =>
      next.io.clk_in := last.io.clk_out
    }

    dividers.head.io.clk_in := clock
    io.clock_out := dividers.last.io.clk_out
  }
}

object Pow2ClockDivider {
  def apply(pow2: Int): Clock = Module(new Pow2ClockDivider(pow2)).io.clock_out
  def apply(clock_in: Clock, pow2: Int): Clock = withClock(clock_in) { apply(pow2) }
}
