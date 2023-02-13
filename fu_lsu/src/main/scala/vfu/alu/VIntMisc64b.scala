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
  *     11.3  vzext, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     Part of 12.4
  *     Part of 12.5
  *     15.1  vmand.mm, ...
  */
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector.{VIFuInfo, SewOH, UIntSplit, BitsExtend}
import yunsuan.vector.alu.VAluOpcode._

class VIntMisc64b extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(6.W))
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    // val toFixP = Output(new MiscToFixP)
  })

  // val uop = io.in.uop
  // val ctrl = io.in.ctrl
  // val funct6 = uop.ctrl.funct6
  // val sew = io.in.sew
  // val (vs1, vs2, vmask) = (io.in.vs1_rs1_imm, io.in.vs2, io.in.vmask)

  val opcode = io.opcode
  val srcTypeVs2 = io.srcType(0)
  val srcTypeVs1 = io.srcType(1)
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(io.vdType(1, 0))
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vm = io.info.vm
  val signed = srcTypeVs2(3, 2) === 1.U
  val vd_sub_srcType = io.vdType(1, 0) - srcTypeVs2(1, 0)
  val expdIdx = io.info.uopIdx

  //---- Extension instructions ----
  val extSign = signed  // 0: z   1: s
  val vf2 = vd_sub_srcType === 1.U
  val vf4 = vd_sub_srcType === 2.U
  val expdIdxOH = Seq.tabulate(8)(i => expdIdx === i.U)
  val extResult = Wire(UInt(64.W))
  when (vf2) {  // sew = 16/32/64
    extResult := Mux(eewVd.is16, BitsExtend.vector(Mux1H(expdIdxOH.take(2), UIntSplit(vs2, 32)), 64, extSign, 8),
                 Mux(eewVd.is32, BitsExtend.vector(Mux1H(expdIdxOH.take(2), UIntSplit(vs2, 32)), 64, extSign, 16),
                               BitsExtend.vector(Mux1H(expdIdxOH.take(2), UIntSplit(vs2, 32)), 64, extSign, 32)))
  }.elsewhen (vf4) {  // sew = 32/64
    extResult := Mux(eewVd.is32, BitsExtend.vector(Mux1H(expdIdxOH.take(4), UIntSplit(vs2, 16)), 64, extSign, 8),
                               BitsExtend.vector(Mux1H(expdIdxOH.take(4), UIntSplit(vs2, 16)), 64, extSign, 16))
  }.otherwise {  //vf8  sew = 64
    extResult := BitsExtend(Mux1H(expdIdxOH, UIntSplit(vs2, 8)), 64, extSign)
  }

  //---- Bitwise Logical instructions ----
  val bitLogicalType = Seq.tabulate(8)(i => opcode === (i + 7).U)
  val bitLogical = Mux1H(Seq(
    bitLogicalType(0) -> (vs2 & vs1),
    bitLogicalType(1) -> ~(vs2 & vs1),
    bitLogicalType(2) -> (vs2 ^ ~vs1),
    bitLogicalType(3) -> (vs2 ^ vs1),
    bitLogicalType(4) -> (vs2 | vs1),
    bitLogicalType(5) -> ~(vs2 | vs1),
    bitLogicalType(6) -> (vs2 | ~vs1),
    bitLogicalType(7) -> ~(vs2 ^ vs1),
  ))
  val is_bitLogical = bitLogicalType.reduce(_ || _)

  /**
    * Shift: vsll, vsrl, vsra, vnsrl, vnsra (vssrl, vssra and vnclipu/vnclip)
    */
  val signed_shift = opcode === vsra || opcode === vssra
  def shiftOnce(n: Int, data: UInt): (UInt, Bool, Bool) = { // n: number of shift bits
    val len = data.getWidth
    require(len > n)
    val rnd_high = data(n-1)
    val rnd_tail = if (n == 1) true.B else {data(n-2, 0) === 0.U}
    //             result of bit-shift                     v[d-1]   v[d-2:0] == 0
    (Cat(Fill(n, data(len-1) && signed_shift), data(len-1, n)), rnd_high, rnd_tail)
  }
  // Shift amount is dynamic
  def dynamicShift(shiftAmount: UInt, data: UInt): (UInt, Bool, Bool) = {
    val width = shiftAmount.getWidth
    val shiftSeq = Seq(1, 2, 4, 8, 16, 32)
    // (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(data) {
    //   case (data, (bit, n)) => Mux(bit, shiftOnce(n, data), data)
    // }
    val dataInit = (data, false.B, true.B)
    (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(dataInit) {
      case ((data, rnd_high, rnd_tail), (bit, n)) => {
        val shiftOnceResult = shiftOnce(n, data)
        val data_update = Mux(bit, shiftOnceResult._1, data)
        val rnd_high_update = Mux(bit, shiftOnceResult._2, rnd_high)
        val rnd_tail_update = Mux(bit, shiftOnceResult._3 && rnd_tail && !rnd_high, rnd_tail)
        (data_update, rnd_high_update, rnd_tail_update)
      }
    }
  }
  // Shift SEW bits data (one element)
  def shiftOneElement(shiftAmount: UInt, data: UInt, sew: Int): (UInt, Bool, Bool) = {
    sew match {
      case 8  => dynamicShift(shiftAmount(2, 0), data)
      case 16 => dynamicShift(shiftAmount(3, 0), data)
      case 32 => dynamicShift(shiftAmount(4, 0), data)
      case 64 => {  // For sew=64, the uimm should perform zero-extending
        // val shiftAmount_uimm = Cat(Mux(uop.ctrl.vi, false.B, shiftAmount(5)), shiftAmount(4, 0))
        dynamicShift(shiftAmount(5, 0), data)
      }
    }
  }
  val is_shift = opcode === vsll || opcode === vsrl || opcode === vssrl || signed_shift

  // Handle shift left
  val leftShift = opcode === vsll
  val vs2_reverse = Cat(vs2.asBools) // bit reverse
  val vs2_adjust = Mux(leftShift, vs2_reverse, vs2)
  val vs1_revsByElem = MuxCase(vs1, Seq(  // reverse vs1 by element when left-shift
    (leftShift && eewVd.is32) -> Cat(UIntSplit(vs1, 32)),
    (leftShift && eewVd.is16) -> Cat(UIntSplit(vs1, 16)),
    (leftShift && eewVd.is8)  -> Cat(UIntSplit(vs1, 8))
  ))
  // Handle narrow instruction
  val vs1Split = UIntSplit(vs1_revsByElem, 8)
  val vs1_adjust = Wire(UInt(64.W))
  val narrow = is_shift && srcTypeVs2(1, 0) =/= io.vdType(1, 0)
  when (narrow && !expdIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(3), 0.U(8.W), vs1Split(2), 0.U(8.W), vs1Split(1), 0.U(8.W), vs1Split(0))
  }.elsewhen (narrow && expdIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(7), 0.U(8.W), vs1Split(6), 0.U(8.W), vs1Split(5), 0.U(8.W), vs1Split(4))
  }.otherwise {
    vs1_adjust := vs1_revsByElem
  }
  
  def shift(sew: Int): Seq[(UInt, Bool, Bool)] = {
    UIntSplit(vs2_adjust, sew) zip UIntSplit(vs1_adjust, sew) map {case (vs2, vs1) => shiftOneElement(vs1, vs2, sew)}
  }
  // val shiftOut = MuxCase(shift(64), Seq(
  //   Mux(uop.ctrl.narrow, sew.is16, sew.is32) -> shift(32),
  //   Mux(uop.ctrl.narrow, sew.is8,  sew.is16) -> shift(16),
  //   Mux(uop.ctrl.narrow, false.B,  sew.is8)  -> shift(8)
  // ))

  val (shift64, shift32, shift16, shift8) = (shift(64), shift(32),shift(16), shift(8))
  val shiftOut = Wire(UInt(64.W))
  // Different SEW cases
  when (Mux(narrow, eewVd.is16, eewVd.is32)) {
    shiftOut := Cat(shift32.map(_._1).reverse)
    // io.toFixP.rnd_high := Cat(Fill(4, shift32(1)._2), Fill(4, shift32(0)._2))
    // io.toFixP.rnd_tail := Cat(Fill(4, shift32(1)._3), Fill(4, shift32(0)._3))
  }.elsewhen (Mux(narrow, eewVd.is8,  eewVd.is16)) {
    shiftOut := Cat(shift16.map(_._1).reverse)
    // io.toFixP.rnd_high := Cat(Fill(2, shift16(3)._2), Fill(2, shift16(2)._2), Fill(2, shift16(1)._2), Fill(2, shift16(0)._2))
    // io.toFixP.rnd_tail := Cat(Fill(2, shift16(3)._3), Fill(2, shift16(2)._3), Fill(2, shift16(1)._3), Fill(2, shift16(0)._3))
  }.elsewhen (Mux(narrow, false.B,  eewVd.is8)) {
    shiftOut := Cat(shift8.map(_._1).reverse)
    // io.toFixP.rnd_high := Cat(shift8.map(_._2).reverse)
    // io.toFixP.rnd_tail := Cat(shift8.map(_._3).reverse)
  }.otherwise {
    shiftOut := shift64(0)._1
    // io.toFixP.rnd_high := Fill(8, shift64(0)._2)
    // io.toFixP.rnd_tail := Fill(8, shift64(0)._3)
  }
  // io.toFixP.shiftOut := shiftOut
  
  val shiftResult = Mux(leftShift, Cat(shiftOut.asBools), shiftOut)
  io.narrowVd := Mux1H(Seq(
    eewVd.is32 -> shiftOut(31, 0),
    eewVd.is16 -> Cat(shiftOut(47, 32), shiftOut(15, 0)),
    eewVd.is8  -> Cat(shiftOut(55, 48), shiftOut(39, 32), shiftOut(23, 16), shiftOut(7, 0))
  ))
  
  /**
    * Integer Merge/Move
    */
  // Adjust vmask. E.g., if sew==32: 000000ab -> aaaabbbb   
  val vmask_adjust = Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(k => 
    Cat(Seq.tabulate(8/k)(i => Fill(k, io.vmask(k*i))))
  ))
  val mergeResult = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    mergeResult(i) := Mux(vmask_adjust(i), vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }
  val mergeMove = Mux(vm, vs1, Cat(mergeResult.reverse))

  // Output arbiter
  io.vd := Mux(is_shift, shiftResult,
               Mux(opcode === vext, extResult,
               Mux(is_bitLogical, bitLogical, mergeMove)))
}