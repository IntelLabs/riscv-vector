/**
  * Top of vector ALU.
  *   Assumption: xLen = 64
  * 
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.3  vzext, ...
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
  */

package darecreek.exu.fu.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.{LaneFUInput, LaneFUOutput, SewOH, VExpdUOp}
import darecreek.UIntSplit

// finalResult = result & maskKeep | maskOff
class MaskTailData extends Module {
  val io = IO(new Bundle {
    val mask = Input(UInt(8.W))
    val tail = Input(UInt(8.W))
    val oldVd = Input(UInt(64.W))
    val uop = Input(new VExpdUOp)
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
  for (i <- 0 until 8) {
    when (tail(i)) {
      maskTail(i) := Mux(uop.info.ta || uop.ctrl.narrow_to_1, 3.U, 2.U)
    }.elsewhen (addWithCarry || vmerge) {
      maskTail(i) := 0.U
    }.elsewhen (!mask(i) && !uop.ctrl.vm) {
      maskTail(i) := Mux(uop.info.ma, 3.U, 2.U)
    }.otherwise {
      maskTail(i) := 0.U
    }
  }
  val destEew = SewOH(uop.info.destEew)

  //--------------------------------------------------------
  //-------- Mask/Tail for non-compare instructions --------
  //--------------------------------------------------------
  io.maskKeep := Mux1H(Seq(
    destEew.is8  -> Cat(maskTail.map(x => Mux(x(1), 0.U(8.W), ~(0.U(8.W)))).reverse),
    destEew.is16 -> Cat(maskTail.take(4).map(x => Mux(x(1), 0.U(16.W), ~(0.U(16.W)))).reverse),
    destEew.is32 -> Cat(maskTail.take(2).map(x => Mux(x(1), 0.U(32.W), ~(0.U(32.W)))).reverse),
    destEew.is64 -> Cat(maskTail.take(1).map(x => Mux(x(1), 0.U(64.W), ~(0.U(64.W))))),
  ))
  io.maskOff := Mux1H(Seq(
    destEew.is8  -> Cat(maskTail.zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(oldVd, 8)(i)))}).reverse),
    destEew.is16 -> Cat(maskTail.take(4).zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(16.W), Mux(x(0), ~0.U(16.W), UIntSplit(oldVd, 16)(i)))}).reverse),
    destEew.is32 -> Cat(maskTail.take(2).zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(32.W), Mux(x(0), ~0.U(32.W), UIntSplit(oldVd, 32)(i)))}).reverse),
    destEew.is64 -> Cat(maskTail.take(1).zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), 0.U(64.W), Mux(x(0), ~0.U(64.W), UIntSplit(oldVd, 64)(i)))}).reverse),
  ))

  //----------------------------------------------------
  //---- Mask/Tail for compare instruction -------------
  //----------------------------------------------------
  io.maskKeep_cmp := Mux1H(Seq(
    destEew.is8  -> Cat(maskTail.map(x => !x(1)).reverse),
    destEew.is16 -> Cat(0.U(4.W), Cat(maskTail.take(4).map(x => !x(1)).reverse)),
    destEew.is32 -> Cat(0.U(6.W), Cat(maskTail.take(2).map(x => !x(1)).reverse)),
    destEew.is64 -> Cat(0.U(7.W), Cat(maskTail.take(1).map(x => !x(1)).reverse)),
  ))
  io.maskOff_cmp := Mux1H(Seq(
    destEew.is8  -> Cat(maskTail.zipWithIndex.map({case (x, i) => 
                         Mux(!x(1), false.B, Mux(x(0), true.B, oldVd(i)))}).reverse),
    destEew.is16 -> Cat(~(0.U(4.W)), Cat(maskTail.take(4).zipWithIndex.map({case (x, i) => 
                         Mux(!x(1), false.B, Mux(x(0), true.B, oldVd(i)))}).reverse)),
    destEew.is32 -> Cat(~(0.U(6.W)), Cat(maskTail.take(2).zipWithIndex.map({case (x, i) => 
                         Mux(!x(1), false.B, Mux(x(0), true.B, oldVd(i)))}).reverse)),
    destEew.is64 -> Cat(~(0.U(7.W)), Cat(maskTail.take(1).zipWithIndex.map({case (x, i) => 
                        Mux(!x(1), false.B, Mux(x(0), true.B, oldVd(i)))}).reverse)),
  ))
}


class VAlu extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LaneFUInput))
    val out = Decoupled(new LaneFUOutput)
  })
  
  val uop = io.in.bits.uop
  val sew = SewOH(uop.info.vsew)  // 0:8, 1:16, 2:32, 3:64
  val fire = io.in.fire

  // broadcast rs1 or imm to all elements, assume xLen = 64
  val imm = uop.ctrl.lsrc(0)
  //                            |sign-extend imm to 64 bits|
  val rs1_imm = Mux(uop.ctrl.vi, Cat(Fill(59, imm(4)), imm), io.in.bits.rs1)
  val rs1_imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(64/n, rs1_imm(n-1, 0))))

  val vs1_rs1_imm = Mux(uop.ctrl.vv, io.in.bits.vs1, rs1_imm_repeat)
  
  // Ctrl signals from alu (funct6) decoder
  val opi = uop.ctrl.funct3(0) === uop.ctrl.funct3(1)
  val vAluDecode = Wire(new VAluDecode)
  val truthTable = TruthTable(VAluADD.table, VAluADD.default)
  val decoderOut = decoder(QMCMinimizer, Cat(uop.ctrl.funct6, opi), truthTable)
  vAluDecode := decoderOut.asTypeOf(new VAluDecode)
  

  /**
    *  ---- Pipeline-stage 1 (int instructions) ----
    */
  // Instantiate sub-modules
  val vAluAdder = Module(new VAluAdder)
  vAluAdder.io.in.uop := uop
  vAluAdder.io.in.ctrl := vAluDecode
  vAluAdder.io.in.sew := sew
  vAluAdder.io.in.vs1_rs1_imm := vs1_rs1_imm
  vAluAdder.io.in.vs2 := io.in.bits.vs2
  vAluAdder.io.in.vmask := io.in.bits.mask

  val vAluMisc = Module(new VAluMisc)
  vAluMisc.io.in.uop := uop
  vAluMisc.io.in.ctrl := vAluDecode
  vAluMisc.io.in.sew := sew
  vAluMisc.io.in.vs1_rs1_imm := vs1_rs1_imm
  vAluMisc.io.in.vs2 := io.in.bits.vs2
  vAluMisc.io.in.vmask := io.in.bits.mask


  //------------------------------------
  //-------- Mask/Tail data gen --------
  //------------------------------------
  val maskTailData = Module(new MaskTailData)
  maskTailData.io.mask := io.in.bits.mask
  maskTailData.io.tail := io.in.bits.tail
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
    aluAdderOut := vAluAdder.io.out.vd
    aluAdderValid := !vAluDecode.misc && !uop.ctrl.narrow_to_1
  }
  // Output of narrow-to-1
  val aluCmpOut = Reg(Vec(8, UInt(8.W)))
  val aluCmpValid = RegInit(false.B)
  val cmpMasked = vAluAdder.io.out.cmp & maskKeep_cmp | maskOff_cmp
  when (fire) {
    aluCmpOut(0) := Mux(uop.expdIdx === 0.U, cmpMasked, aluCmpOut(0))
    for (i <- 1 until 8) {
      aluCmpOut(i) := Mux(uop.expdIdx === 0.U, ~(0.U(8.W)), 
                          Mux(uop.expdIdx === i.U, cmpMasked, aluCmpOut(i)))
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
    aluMiscOut := vAluMisc.io.out.vd
    aluMiscValid := vAluDecode.misc && !uop.ctrl.narrow
    aluNarrowOut := Mux(uop.expdIdx(0), Cat(vAluMisc.io.out.narrow, aluNarrowOut(31, 0)),
                        Cat(0.U(32.W), vAluMisc.io.out.narrow))
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
  val vdS1IntFinal = Mux(aluCmpValid, Cat(aluCmpOut.reverse), vdS1IntMasked)

  //--------- Ready/valid of S1Int ---------
  val readyS1Int = Wire(Bool())
  val readyS1FixP = Wire(Bool())
  io.in.ready := !io.in.valid || (readyS1Int && readyS1FixP) //S1: pipeline stage 1
  val validS1Int = RegInit(false.B)
  validS1Int := Mux(fire && !uop.ctrl.fixP, true.B, Mux(readyS1Int, false.B, validS1Int))
  val uopS1 = RegEnable(uop, fire)
  val validS1IntFinal = Mux(uopS1.ctrl.narrow_to_1, aluCmpValid && validS1Int, 
                        Mux(uopS1.ctrl.narrow, aluNarrowValid && validS1Int, validS1Int))

  //-------------- S1FixP ------------------
  val vAluFixP = Module(new VAluFixP)
  vAluFixP.io.uop := uopS1
  vAluFixP.io.ctrl.sew := RegEnable(sew, fire)
  vAluFixP.io.ctrl.sub := RegEnable(vAluDecode.sub, fire)
  vAluFixP.io.fromAdder := RegEnable(vAluAdder.io.toFixP, fire)
  vAluFixP.io.fromMisc := RegEnable(vAluMisc.io.toFixP, fire)
  
  //--------- Ready/valid of S1FixP (S1: pipeline stage 1) ---------
  val validS1FixP = RegInit(false.B)
  val readyS2FixP = Wire(Bool())
  readyS1FixP := !validS1FixP || readyS2FixP
  validS1FixP := Mux(fire && uop.ctrl.fixP, true.B, Mux(readyS1FixP, false.B, validS1FixP))
  val fireS1FixP = validS1FixP && readyS1FixP


  /**
    *  ---- Pipeline-stage 2 (fixed-point instructions) ----
    */
  // Output of fixed-point unit
  val aluFixPOut = Reg(UInt(64.W))
  val aluFixPValid = RegInit(false.B)
  val aluFixPNarrowOut = Reg(UInt(64.W))
  val aluFixPNarrowValid = RegInit(false.B)
  when (fireS1FixP) {
    aluFixPOut := vAluFixP.io.out.vd
    aluFixPValid := uopS1.ctrl.fixP && !uopS1.ctrl.narrow
    aluFixPNarrowOut := Mux(uopS1.expdIdx(0), Cat(vAluFixP.io.out.narrow, aluFixPNarrowOut(31, 0)),
                        Cat(0.U(32.W), vAluFixP.io.out.narrow))
    aluFixPNarrowValid := (uopS1.expdIdx(0) || uopS1.expdEnd) && uopS1.ctrl.narrow
  }

  val vdS2FixP = Mux1H(Seq(
    aluFixPValid -> aluFixPOut,
    aluFixPNarrowValid -> aluFixPNarrowOut
  ))

  val maskKeepS2 = RegEnable(maskKeepS1, fireS1FixP)
  val maskOffS2 = RegEnable(maskOffS1, fireS1FixP)
  val vdS2FixPFinal = vdS2FixP & maskKeepS2 | maskOffS2

  val uopS2 = RegEnable(uopS1, fireS1FixP) 
  val vxsatS2 = RegEnable(vAluFixP.io.out.vxsat, fireS1FixP)

  val validS2FixP = RegInit(false.B)
  validS2FixP := Mux(fireS1FixP, true.B, Mux(readyS2FixP, false.B, validS2FixP))


  /**
    *  ---- Output arbiter (int vs fix-p) ----
    *  No need to use round robin since there will be no input-fire if readyS1Int == 0.
    */
  val arb = Module(new Arbiter(new LaneFUOutput, 2))
  arb.io.in(0).valid := validS2FixP
  arb.io.in(1).valid := validS1IntFinal
  readyS2FixP := arb.io.in(0).ready
  readyS1Int := arb.io.in(1).ready

  arb.io.in(0).bits.uop := uopS2
  arb.io.in(0).bits.vd := vdS2FixPFinal
  arb.io.in(0).bits.fflags := 0.U
  arb.io.in(0).bits.vxsat := vxsatS2
  arb.io.in(1).bits.uop := uopS1
  arb.io.in(1).bits.vd := vdS1IntFinal
  arb.io.in(1).bits.fflags := 0.U
  arb.io.in(1).bits.vxsat := false.B

  io.out <> arb.io.out
}

object VerilogAlu extends App {
  println("Generating the VPU ALU hardware")
  emitVerilog(new VAlu(), Array("--target-dir", "generated"))
}