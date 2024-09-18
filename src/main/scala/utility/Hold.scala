/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utility

import chisel3._
import chisel3.util._

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool, init: Option[T] = None): T = {
    val hold_data = if (init.isDefined) RegEnable(x, init.get, en) else RegEnable(x, en)
    Mux(en, x, hold_data)
  }
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), GatedValidRegNext(en))
}

/*
 * Hold the in fire unless out fire or flush happens
 * similar to BoolStopWatch
 */
object ValidHold {
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B ) = {
    val valid = RegInit(false.B)
    when (outfire) { valid := false.B }
    when (infire) { valid := true.B }
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid
  }
}


/*
 * Hold the 'fire' for only one cycle unless new fire comes in
 */
object OneCycleValid {
  def apply(fire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    when (flush) { valid := false.B }
    valid
  }
}

/*
 * Hold the data when it is valid and bypass latest data
 */
object DataHoldBypass {
  def apply[T <: Data](data: T, valid: Bool): T = {
    Mux(valid, data, RegEnable(data, valid))
  }
}

/*
 * Data change or not
 */
object DataChanged {
  def apply(data: UInt): UInt = {
    val old_data = Reg(chiselTypeOf(data))
    val changed = data =/= old_data
    when (changed) { old_data := data }
    changed
  }
}

/**
  * Delay the data for N cycles
  */
class DelayN[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new Bundle() {
    val in = Input(gen)
    val out = Output(gen)
  })
  var out = io.in
  for (i <- 0 until n) {
    out = RegNext(out)
  }
  io.out := out
}

object DelayN {
  def apply[T <: Data](in: T, n: Int): T = {
    val delay = Module(new DelayN(chiselTypeOf(in), n))
    delay.io.in := in
    delay.io.out
  }
}

class DelayNWithValid[T <: Data](gen: T, n: Int, hasInit: Boolean = true) extends Module{
  val io = IO(new Bundle(){
    val in_bits = Input(gen)
    val in_valid = Input(Bool())
    val out_bits = Output(gen)
    val out_valid = Output(Bool())
  })
  val (res_valid, res_bits) = (0 until n).foldLeft((io.in_valid, io.in_bits)) {
    (prev, _) =>
      val valid = Wire(Bool())
      if (hasInit) {
        valid := RegNext(prev._1, init = false.B)
      } else {
        valid := RegNext(prev._1)
      }
      val data = RegEnable(prev._2, prev._1)
      (valid, data)
  }
  io.out_valid := res_valid
  io.out_bits := res_bits
}

object DelayNWithValid{
  def apply[T <: Data](in: T, valid: Bool, n: Int): (Bool, T) = {
    val pipMod = Module(new DelayNWithValid(chiselTypeOf(in), n))
    pipMod.io.in_valid := valid
    pipMod.io.in_bits := in
    (pipMod.io.out_valid, pipMod.io.out_bits)
  }

  def apply[T <: Valid[Data]](in: T, n: Int): T = {
    val pipMod = Module(new DelayNWithValid(chiselTypeOf(in.bits), n))
    pipMod.io.in_valid := in.valid
    pipMod.io.in_bits := in.bits
    val res = Wire(chiselTypeOf(in))
    res.valid := pipMod.io.out_valid
    res.bits := pipMod.io.out_bits
    res
  }

  def apply[T <: Data](in: T, valid: Bool, n: Int, hasInit: Boolean): (Bool, T) = {
    val pipMod = Module(new DelayNWithValid(chiselTypeOf(in), n, hasInit = hasInit))
    pipMod.io.in_valid := valid
    pipMod.io.in_bits := in
    (pipMod.io.out_valid, pipMod.io.out_bits)
  }

  def apply[T <: Valid[Data]](in: T, n: Int, hasInit: Boolean): T = {
    val pipMod = Module(new DelayNWithValid(chiselTypeOf(in.bits), n, hasInit = hasInit))
    pipMod.io.in_valid := in.valid
    pipMod.io.in_bits := in.bits
    val res = Wire(chiselTypeOf(in))
    res.valid := pipMod.io.out_valid
    res.bits := pipMod.io.out_bits
    res
  }
}
