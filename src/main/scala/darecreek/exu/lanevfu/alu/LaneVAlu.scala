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

/** Integer and fixed-point (except mult and div)
  *   
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.3  vzext, ... //Todo: move to crosslane block
  *     11.4  vadc, vmadc, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  *     16.1
  *     16.2
  *     16.6
  */
package darecreek.exu.lanevfu.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.{SewOH, UIntSplit, MaskReorg}
import chipsalliance.rocketchip.config._
import darecreek.exu.vfucore.alu._
import darecreek.exu.vfucore.{VFuModule, VFuParamsKey, VFuParameters}
import darecreek.exu.vfucore.{LaneFUInput, LaneFUOutput}
import darecreek.exu.vfucoreconfig.{VUop, Redirect}

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  val fixp = Bool()
}

// finalResult = result & maskKeep | maskOff
class MaskTailDataVAlu extends Module {
  val io = IO(new Bundle {
    val mask = Input(UInt(8.W))
    val tail = Input(UInt(8.W))
    val vstart_gte_vl = Input(Bool())
    val oldVd = Input(UInt(64.W))
    val uop = Input(new VUop)
    val opi = Input(Bool())
    val maskKeep = Output(UInt(64.W))  // keep: 11..1  off: 00..0
    val maskOff = Output(UInt(64.W))   // keep: 00..0  off: old_vd or 1.U
    val maskKeep_cmp = Output(UInt(8.W)) // for compare
    val maskOff_cmp = Output(UInt(8.W))  // for compare
  })

  val maskTail = Wire(Vec(8, UInt(2.W))) // 00: keep result   10: old_vd(undisturbed)  11: write 1s(agnostic)
  val (mask, tail, oldVd, uop) = (io.mask, io.tail, io.oldVd, io.uop)
  val addWithCarry = uop.ctrl.funct6(5,2) === "b0100".U && io.opi
  val vmerge = uop.ctrl.funct6 === "b010111".U
  val isWholeRegMv = uop.ctrl.funct6 === "b100111".U && uop.ctrl.funct3 === "b011".U
  for (i <- 0 until 8) {
    when (io.vstart_gte_vl && !isWholeRegMv) {
      maskTail(i) := 2.U
    }.elsewhen (tail(i)) {
      maskTail(i) := Mux(isWholeRegMv, 0.U, Mux(uop.info.ta || uop.ctrl.narrow_to_1, 3.U, 2.U))
    }.elsewhen (addWithCarry || vmerge) {
      maskTail(i) := 0.U
    }.elsewhen (!mask(i) && !uop.ctrl.vm) {
      maskTail(i) := Mux(uop.info.ma, 3.U, 2.U)
    }.otherwise {
      maskTail(i) := 0.U
    }
  }
  //--------------------------------------------------------
  //-------- Mask/Tail for non-compare instructions --------
  //--------------------------------------------------------
  io.maskKeep := Cat(maskTail.map(x => Mux(x(1), 0.U(8.W), ~(0.U(8.W)))).reverse)
  io.maskOff := Cat(maskTail.zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(oldVd, 8)(i)))}).reverse)
  //----------------------------------------------------
  //---- Mask/Tail for compare instruction -------------
  //----------------------------------------------------
  io.maskKeep_cmp := Cat(maskTail.map(x => !x(1)).reverse)
  io.maskOff_cmp := Cat(maskTail.zipWithIndex.map({case (x, i) => 
                         Mux(!x(1), false.B, Mux(x(0), true.B, oldVd(i)))}).reverse)
}


class LaneVAlu(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val redirect = Input(new Redirect)
    val out = Decoupled(new LaneFUOutput)
  })
  
  val uop = io.in.bits.uop
  val sew = SewOH(uop.info.vsew)  // 0:8, 1:16, 2:32, 3:64
  val eewVd = SewOH(uop.info.destEew)
  val flushIn = io.in.valid && io.redirect.needFlush(io.in.bits.uop.robIdx)
  val fire = io.in.fire && !flushIn
  // broadcast rs1 or imm to all elements, assume xLen = 64
  val imm = uop.ctrl.lsrc(0)
  //                            |sign-extend imm to 64 bits|
  val rs1_imm = Mux(uop.ctrl.vi, Cat(Fill(59, imm(4)), imm), io.in.bits.rs1)
  val rs1_imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(64/n, rs1_imm(n-1, 0))))

  val vs1_rs1_imm = Mux(uop.ctrl.vv, io.in.bits.vs1, rs1_imm_repeat)
  
  // Ctrl signals from alu (funct6) decoder
  val opi = uop.ctrl.funct3(0) === uop.ctrl.funct3(1)
  val vAluDecode = Wire(new VIntFixpDecode)
  val truthTable = TruthTable(VAluTable.table, VAluTable.default)
  val decoderOut = decoder(QMCMinimizer, Cat(uop.ctrl.funct6, opi), truthTable)
  vAluDecode := decoderOut.asTypeOf(new VIntFixpDecode)
  

  /**
    *  ---- Pipeline-stage 1 (int instructions) ----
    */
  // Instantiate sub-modules
  val vIntAdder = Module(new VIntAdder64b)
  vIntAdder.io.funct6 := uop.ctrl.funct6
  vIntAdder.io.vm := uop.ctrl.vm
  vIntAdder.io.ma := uop.info.ma
  vIntAdder.io.sew := sew
  vIntAdder.io.eewVd := eewVd
  vIntAdder.io.uopIdx := uop.expdIdx
  vIntAdder.io.vs1 := vs1_rs1_imm
  vIntAdder.io.vs2 := io.in.bits.vs2
  vIntAdder.io.oldVd := io.in.bits.old_vd
  vIntAdder.io.vmask := io.in.bits.mask
  vIntAdder.io.isSub := vAluDecode.sub
  vIntAdder.io.widen := uop.ctrl.widen
  vIntAdder.io.widen2 := uop.ctrl.widen2
  vIntAdder.io.narrow_to_1 := uop.ctrl.narrow_to_1

  val vIntMisc = Module(new VIntMisc64b)
  vIntMisc.io.funct6 := uop.ctrl.funct6
  vIntMisc.io.funct3 := uop.ctrl.funct3
  vIntMisc.io.vi := uop.ctrl.vi
  vIntMisc.io.vm := uop.ctrl.vm
  vIntMisc.io.vs1_imm := uop.ctrl.lsrc(0)
  vIntMisc.io.narrow := uop.ctrl.narrow
  vIntMisc.io.sew := sew
  vIntMisc.io.uopIdx := uop.expdIdx
  vIntMisc.io.vs1 := vs1_rs1_imm
  vIntMisc.io.vs2 := io.in.bits.vs2
  vIntMisc.io.vmask := io.in.bits.mask
  vIntMisc.io.fs1:= io.in.bits.rs1(FLEN-1, 0)

  //------------------------------------
  //-------- Mask/Tail data gen --------
  //------------------------------------
  // Splash. sew = 8: unchanged, sew = 16: 0000abcd -> aabbccdd, ...
  val maskSplash = Mux(uop.ctrl.narrow_to_1, io.in.bits.mask, MaskReorg.splash(io.in.bits.mask, eewVd))
  val tailSplash = Mux(uop.ctrl.narrow_to_1, io.in.bits.tail, MaskReorg.splash(io.in.bits.tail, eewVd))
  val maskTailData = Module(new MaskTailDataVAlu)
  maskTailData.io.mask := maskSplash
  maskTailData.io.tail := tailSplash
  maskTailData.io.vstart_gte_vl := uop.info.vstart_gte_vl
  maskTailData.io.oldVd := io.in.bits.old_vd
  maskTailData.io.uop := io.in.bits.uop
  maskTailData.io.opi := opi
  val maskKeepS1 = RegEnable(maskTailData.io.maskKeep, fire)
  val maskOffS1 = RegEnable(maskTailData.io.maskOff, fire)
  val maskKeep_cmp = maskTailData.io.maskKeep_cmp
  val maskOff_cmp = maskTailData.io.maskOff_cmp

  //------------------------------------------------
  //---- output of S1Int (S1: pipeline stage 1) ----
  //------------------------------------------------
  // Output of ALU adder (not narrow-to-1)
  val aluAdderOut = Reg(UInt(64.W))
  val aluAdderValid = RegInit(false.B)
  when (fire) {
    aluAdderOut := vIntAdder.io.vd
    aluAdderValid := !vAluDecode.misc && !uop.ctrl.narrow_to_1
  }
  // Output of narrow-to-1
  val aluCmpOut = Reg(Vec(8, UInt(8.W)))
  val aluCmpValid = RegInit(false.B)
  val cmpMasked = vIntAdder.io.cmpOut & maskKeep_cmp | maskOff_cmp
  when (fire) {
    aluCmpOut(0) := Mux(uop.expdIdx === 0.U, cmpMasked, aluCmpOut(0))
    for (i <- 1 until 8) {
      aluCmpOut(i) := Mux(uop.expdIdx === 0.U, ~(0.U(8.W)), 
                          Mux(uop.expdIdx === i.U, cmpMasked, aluCmpOut(i)))
    }
    when (uop.info.vstart_gte_vl) {
      for (i <- 0 until 8) { aluCmpOut(i) := UIntSplit(io.in.bits.old_vd, 8)(i) }
    }
    aluCmpValid := uop.expdEnd && uop.ctrl.narrow_to_1
  }
  //---- Output of ALU MISC (not narrow) ----
  val aluMiscOut = Reg(UInt(64.W))
  val aluMiscValid = RegInit(false.B)
  //---- Output of shift narrow ----
  val aluNarrowOut = Reg(UInt(64.W))
  val aluNarrowValid = RegInit(false.B)
  when (fire) {
    aluMiscOut := vIntMisc.io.vd
    aluMiscValid := vAluDecode.misc && !uop.ctrl.narrow
    aluNarrowOut := Mux(uop.expdIdx(0), Cat(vIntMisc.io.narrowVd, aluNarrowOut(31, 0)),
                        Cat(0.U(32.W), vIntMisc.io.narrowVd))
    aluNarrowValid := (uop.expdIdx(0) || uop.expdEnd) && uop.ctrl.narrow
  }
  assert(PopCount(Cat(aluAdderValid, aluCmpValid, aluMiscValid, aluNarrowValid)) <= 1.U, "Error VPU: ALU_int out has more than 1 valids")

  //-------------------------------
  //---- Final output of S1Int ----
  //-------------------------------
  val vdS1Int = Mux1H(Seq(
    aluAdderValid -> aluAdderOut,
    aluMiscValid -> aluMiscOut,
    aluNarrowValid -> aluNarrowOut,
  ))
  val vdS1IntMasked = vdS1Int & maskKeepS1 | maskOffS1
  val rdValid = Reg(Bool())
  val rd = Reg(UInt(XLEN.W))
  when (fire) {
    rdValid := vIntMisc.io.rd.valid
    rd := vIntMisc.io.rd.bits
  }
  val vdS1IntFinal = Mux(rdValid, rd,
                         Mux(aluCmpValid, Cat(aluCmpOut.reverse), vdS1IntMasked))

  //--------- Ready/valid of S1Int ---------
  val readyS1Int = Wire(Bool())
  val readyS1FixP = Wire(Bool())
  val validS1Int = RegInit(false.B)
  val uopS1 = RegEnable(uop, fire)
  val flushS1Int = validS1Int && io.redirect.needFlush(uopS1.robIdx)
  when (fire && !uop.ctrl.fixP) { 
    validS1Int := true.B
  }.elsewhen (readyS1Int || flushS1Int) {
    validS1Int := false.B
  }
  // validS1Int := Mux(fire && !uop.ctrl.fixP, true.B, Mux(readyS1Int, false.B, validS1Int))
  val validS1IntFinal = Mux(uopS1.ctrl.narrow_to_1, aluCmpValid && validS1Int, 
                        Mux(uopS1.ctrl.narrow, aluNarrowValid && validS1Int, validS1Int))

  //-------------- S1FixP ------------------
  val vAluFixP = Module(new VFixPoint64b)
  vAluFixP.io.funct6 := uopS1.ctrl.funct6
  vAluFixP.io.sew := RegEnable(sew, fire)
  vAluFixP.io.vxrm := uopS1.info.vxrm
  vAluFixP.io.isFixp := RegEnable(vAluDecode.fixp, fire)
  vAluFixP.io.isSub := RegEnable(vAluDecode.sub, fire)
  vAluFixP.io.fromAdder := RegEnable(vIntAdder.io.toFixP, fire)
  vAluFixP.io.fromMisc := RegEnable(vIntMisc.io.toFixP, fire)
  
  //--------- Ready/valid of S1FixP (S1: pipeline stage 1) ---------
  val validS1FixP = RegInit(false.B)
  val readyS2FixP = Wire(Bool())
  readyS1FixP := readyS2FixP
  val flushS1FixP = validS1FixP && io.redirect.needFlush(uopS1.robIdx)
  when (fire && uop.ctrl.fixP) { 
    validS1FixP := true.B
  }.elsewhen (readyS1FixP || flushS1FixP) {
    validS1FixP := false.B
  }
  // validS1FixP := Mux(fire && uop.ctrl.fixP, true.B, Mux(readyS1FixP, false.B, validS1FixP))
  val fireS1FixP = validS1FixP && readyS1FixP && !flushS1FixP

  io.in.ready := (readyS1Int || !validS1Int) && (readyS1FixP || !validS1FixP) //S1: pipeline stage 1

  /**
    *  ---- Pipeline-stage 2 (fixed-point instructions) ----
    */
  // Output of fixed-point unit
  val aluFixPOut = Reg(UInt(64.W))
  val aluFixPVxsatOut = Reg(UInt(64.W))
  val aluFixPValid = RegInit(false.B)
  val aluFixPNarrowOut = Reg(UInt(64.W))
  val aluFixPVxsatNarrowOut = Reg(UInt(8.W))
  val aluFixPNarrowValid = RegInit(false.B)
  when (fireS1FixP) {
    aluFixPOut := vAluFixP.io.vd
    aluFixPVxsatOut := vAluFixP.io.vxsat
    aluFixPValid := uopS1.ctrl.fixP && !uopS1.ctrl.narrow
    aluFixPNarrowOut := Mux(uopS1.expdIdx(0), Cat(vAluFixP.io.narrowVd, aluFixPNarrowOut(31, 0)),
                        Cat(0.U(32.W), vAluFixP.io.narrowVd))
    aluFixPVxsatNarrowOut := Mux(uopS1.expdIdx(0), Cat(vAluFixP.io.vxsat(3, 0), aluFixPVxsatNarrowOut(3, 0)),
                        Cat(0.U(4.W), vAluFixP.io.vxsat(3, 0)))
    aluFixPNarrowValid := (uopS1.expdIdx(0) || uopS1.expdEnd) && uopS1.ctrl.narrow
  }

  val vdS2FixP = Mux1H(Seq(
    aluFixPValid -> aluFixPOut,
    aluFixPNarrowValid -> aluFixPNarrowOut
  ))
  val vxsatS2 = Mux1H(Seq(
    aluFixPValid -> aluFixPVxsatOut,
    aluFixPNarrowValid -> aluFixPVxsatNarrowOut
  ))

  val maskKeepS2 = RegEnable(maskKeepS1, fireS1FixP)
  val maskOffS2 = RegEnable(maskOffS1, fireS1FixP)
  val vdS2FixPFinal = vdS2FixP & maskKeepS2 | maskOffS2

  val uopS2 = RegEnable(uopS1, fireS1FixP) 
  // Assume vstart = 0
  val maskSplashS1 = RegEnable(maskSplash, fire)
  val tailSplashS1 = RegEnable(tailSplash, fire)
  val maskSplashS2 = RegEnable(maskSplashS1, fireS1FixP)
  val tailSplashS2 = RegEnable(tailSplashS1, fireS1FixP)
  val vxsatS2_maskTail = vxsatS2 & ~tailSplashS2 & (maskSplashS2 | Fill(8, uopS2.ctrl.vm))

  val validS2FixP = RegInit(false.B)
  val flushS2FixP = validS2FixP && io.redirect.needFlush(uopS2.robIdx)
  when (fireS1FixP) {
    validS2FixP := true.B
  }.elsewhen (readyS2FixP || flushS2FixP) {
    validS2FixP := false.B
  }
  // validS2FixP := Mux(fireS1FixP, true.B, Mux(readyS2FixP, false.B, validS2FixP))
  val validS2FixPFinal = Mux(uopS2.ctrl.narrow, aluFixPNarrowValid && validS2FixP, validS2FixP)

  /**
    *  ---- Output arbiter (int vs fix-p) ----
    *  No need to use round robin since there will be no input-fire if readyS1Int == 0.
    */
  val arb = Module(new Arbiter(new LaneFUOutput, 2))
  arb.io.in(0).valid := validS2FixPFinal
  arb.io.in(1).valid := validS1IntFinal
  readyS2FixP := arb.io.in(0).ready
  readyS1Int := arb.io.in(1).ready

  arb.io.in(0).bits.uop := uopS2
  arb.io.in(0).bits.vd := vdS2FixPFinal
  arb.io.in(0).bits.fflags := 0.U
  arb.io.in(0).bits.vxsat := vxsatS2_maskTail.orR
  arb.io.in(1).bits.uop := uopS1
  arb.io.in(1).bits.vd := vdS1IntFinal
  arb.io.in(1).bits.fflags := 0.U
  arb.io.in(1).bits.vxsat := false.B

  io.out <> arb.io.out
}