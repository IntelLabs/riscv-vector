/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

/**
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.4  vadc, vmadc, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     Part of 12.1
  *     Part of 12.2
  */
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector.{VIFuInfo, SewOH}
import yunsuan.vector.alu.VAluOpcode._

class VIntAdder64b extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(6.W))
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val is_sub = Input(Bool())  // subtract

    val vd = Output(UInt(64.W))
    val cmpOut = Output(UInt(8.W)) // compare or add-with-carry carry output
    // val toFixP = Output(new AdderToFixP)
  })

  val opcode = io.opcode
  val srcTypeVs2 = io.srcType(0)
  val srcTypeVs1 = io.srcType(1)
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(io.vdType(1, 0))
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vmask = io.vmask
  val vm = io.info.vm
  val sub = io.is_sub
  val signed = srcTypeVs2(3, 2) === 1.U
  val addWithCarry = opcode(5, 3) === "b000".U && (opcode(2, 0) === 3.U || opcode(2, 0) === 4.U || opcode(2, 0) === 5.U || opcode(2, 0) === 6.U)

  // /**
  //   * Input adjust: widen & subtract
  //   */
  // // Widen vs1 & vs2
  // def widenPad(x: UInt) = {
  //   val len = x.getWidth
  //   Cat(Fill(len, x(len-1) && signed), x)
  // }
  // val vs = Seq(vs1, vs2)
  // val vs_widen = Wire(Vec(2, UInt(64.W)))
  // val widenCase = Seq(widen, {widen && eewVs2 =/= eewVd})
  // for (i <- 0 until 2) {
  //   val vs_32b = Mux(uop.expdIdx(0), vs(i)(63, 32), vs(i)(31, 0))
  //   when (widenCase(i)) {
  //     vs_widen(i) := Mux1H(Seq(
  //       sew.is8  -> Cat(widenPad(vs_32b(31, 24)), widenPad(vs_32b(23, 16)), widenPad(vs_32b(15, 8)), widenPad(vs_32b(7, 0))),
  //       sew.is16 -> Cat(widenPad(vs_32b(31, 16)), widenPad(vs_32b(15, 0))),
  //       sew.is32 -> widenPad(vs_32b(31, 0))
  //     ))
  //   }.otherwise {
  //     vs_widen(i) := vs(i)
  //   }
  // }

  // Subtract: bit negate
  val vs1_adjust = vs1 ^ Fill(64, sub)

  /**
    * Chain all eight 8bit-adders
    */
  class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +
                       Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(9), bits(8, 1))
  }
  val cin  = Wire(Vec(8, Bool()))
  val cout = Wire(Vec(8, Bool()))
  val vd   = Wire(Vec(8, UInt(8.W)))
  val carryIn = Wire(Vec(8, Bool()))
  // Adjust vmask. sew==32: 00000011 -> 00010001   sew==16: 00001111 -> 01010101
  val vmask_adjust = MuxCase(vmask, Seq(
    eewVs1.is16 -> Cat(false.B, vmask(3), false.B, vmask(2), false.B, vmask(1), false.B, vmask(0)),
    eewVs1.is32 -> Cat(0.U(3.W), vmask(1), 0.U(3.W), vmask(0))
  ))

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs1_adjust(8*i+7, 8*i), vs2(8*i+7, 8*i), cin(i))
    // Generate carry-in from sub and vmask(11.4 Add-with-Carry/Sub-with_Borrow)
    carryIn(i) := Mux(addWithCarry, Mux(vm, sub, vmask_adjust(i) ^ sub), sub)
    // Generate final carry-in: cin
    cin(i) := Mux1H(Mux(opcode(5,1) === 0.U, eewVd.oneHot, eewVs1.oneHot), Seq(1, 2, 4, 8).map(n => 
      if ((i % n) == 0) carryIn(i) else cout(i-1))
    )
    cout(i) := adder_8b.cout
    vd(i) := adder_8b.out
  }

  /**
    * Integer Compare & Min/Max instructions
    */
  val lessThan_vec = Wire(Vec(8, Bool()))
  val equal_vec = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    lessThan_vec(i) := Mux(signed, (vs2(8*i+7) ^ vs1_adjust(8*i+7)) ^ cout(i), !cout(i))
    equal_vec(i) := vs2(8*i+7, 8*i) === vs1(8*i+7, 8*i)
  }
  val equal = Cat(equal_vec.reverse)
  val cmpEq = Mux1H(Seq(
    eewVs1.is8  -> equal,
    eewVs1.is16 -> Cat(Fill(2, equal(7, 6).andR), Fill(2, equal(5, 4).andR), Fill(2, equal(3, 2).andR), Fill(2, equal(1, 0).andR)),
    eewVs1.is32 -> Cat(Fill(4, equal(7, 4).andR), Fill(4, equal(3, 0).andR)),
    eewVs1.is64 -> Fill(8, equal.andR)
  ))
  val cmpNe = ~cmpEq
  val lessThan = Cat(lessThan_vec.reverse)
  val cmpResult = Mux1H(Seq(
    (opcode === vmseq) -> cmpEq,
    (opcode === vmsne) -> cmpNe,
    (opcode === vmslt) -> lessThan,
    (opcode === vmsle) -> (lessThan | cmpEq),
    (opcode === vmsgt) -> ~(lessThan | cmpEq)
  ))

  //-------- Min/Max --------
  val minMaxResult = Wire(Vec(8, UInt(8.W)))
  val selectVs1 = lessThan_vec.map(_ === !opcode(0))
  for (i <- 0 until 8) {
    val sel = Mux1H(Seq(
      eewVs1.is8  -> selectVs1(i),
      eewVs1.is16 -> selectVs1((i/2)*2+1),
      eewVs1.is32 -> selectVs1((i/4)*4+3),
      eewVs1.is64 -> selectVs1(7),
    ))
    minMaxResult(i) := Mux(sel, vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }

  io.vd := Mux(opcode === vmin || opcode === vmax, Cat(minMaxResult.reverse), Cat(vd.reverse))

  io.cmpOut := Mux(addWithCarry, Cat(cout.reverse), cmpResult)
  // io.out.cmp := Mux1H(Seq(
  //   sew.is8  -> cmpOut,
  //   sew.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
  //   sew.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
  //   sew.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
  // ))

  //---- To Fixed-Point unit ----
  // for (i <- 0 until 8) {
  //   io.toFixP.vs2H(i) := vs2(8*i+7)
  //   io.toFixP.vs1H(i) := vs1(8*i+7)
  //   io.toFixP.vd(i) := vd(i)
  //   io.toFixP.cout(i) := cout(i)
  // }
}