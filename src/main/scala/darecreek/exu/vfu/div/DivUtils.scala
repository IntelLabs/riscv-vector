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

package darecreek.exu.vfu.div

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
// import darecreek.exu.LaneUnit
import darecreek.exu.vfu.LaneUnit

import scala.math.min

abstract class AbstractDivider(len: Int)(implicit p: Parameters) extends LaneUnit{
  val ctrl = IO(Input(new MulDivCtrl))
  val sign = ctrl.sign
}

class MulDivCtrl extends Bundle{
  val sign = Bool()
  val isW = Bool()
  val isHi = Bool() // return hi bits of result ?
}

class RightShifter(len: Int, lzc_width: Int) extends Module {
  val io = IO(new Bundle() {
    val shiftNum = Input(UInt(lzc_width.W))
    val in = Input(UInt(len.W))
    val msb = Input(Bool())
    val out = Output(UInt(len.W))
  })
  require(len == 64 || len == 32 || len == 16 || len == 8)
  val shift = io.shiftNum
  val msb = io.msb
  val s0 = Mux(shift(0), Cat(VecInit(Seq.fill(1)(msb)).asUInt, io.in(len - 1, 1)), io.in)
  val s1 = Mux(shift(1), Cat(VecInit(Seq.fill(2)(msb)).asUInt, s0(len - 1, 2)), s0)
  val s2 = Mux(shift(2), Cat(VecInit(Seq.fill(4)(msb)).asUInt, s1(len - 1, 4)), s1)
  val s3 = Wire(UInt(len.W))
  val s4 = Wire(UInt(len.W))
  val s5 = Wire(UInt(len.W))
  if (len == 64) {
    s3 := Mux(shift(3), Cat(VecInit(Seq.fill(8)(msb)).asUInt, s2(len - 1, 8)), s2)
    s4 := Mux(shift(4), Cat(VecInit(Seq.fill(16)(msb)).asUInt, s3(len - 1, 16)), s3)
    s5 := Mux(shift(5), Cat(VecInit(Seq.fill(32)(msb)).asUInt, s4(len - 1, 32)), s4)
  } else if (len == 32) {
    s3 := Mux(shift(3), Cat(VecInit(Seq.fill(8)(msb)).asUInt, s2(len - 1, 8)), s2)
    s4 := Mux(shift(4), Cat(VecInit(Seq.fill(16)(msb)).asUInt, s3(len - 1, 16)), s3)
    s5 := s4
  } else if (len == 16) {
    s3 := Mux(shift(3), Cat(VecInit(Seq.fill(8)(msb)).asUInt, s2(len - 1, 8)), s2)
    s5 := s3
  } else {
    s5 := s2
  }
  io.out := s5
}

object SignExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}

abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(Vec(m, UInt(len.W)))
    val out = Output(Vec(n, UInt(len.W)))
  })
}

class CSA2_2(len: Int) extends CarrySaveAdderMToN(2, 2)(len) {
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b) = (io.in(0)(i), io.in(1)(i))
    val sum = a ^ b
    val cout = a & b
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}

class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len){
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b = a ^ b
    val a_and_b = a & b
    val sum = a_xor_b ^ cin
    val cout = a_and_b | (a_xor_b & cin)
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}

class CSA5_3(len: Int)extends CarrySaveAdderMToN(5, 3)(len){
  val FAs = Array.fill(2)(Module(new CSA3_2(len)))
  FAs(0).io.in := io.in.take(3)
  FAs(1).io.in := VecInit(FAs(0).io.out(0), io.in(3), io.in(4))
  io.out := VecInit(FAs(1).io.out(0), FAs(0).io.out(1), FAs(1).io.out(1))
}

class C22 extends CSA2_2(1)
class C32 extends CSA3_2(1)
class C53 extends CSA5_3(1)