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

package darecreek

import chisel3._
import chisel3.util._
import darecreek.lsu._

class VInfoAll extends Bundle {
  val veewVs1 = UInt(3.W)
  val veewVs2 = UInt(3.W)
  val veewVd = UInt(3.W)
  val vemulVs1 = UInt(3.W)
  val vemulVs2 = UInt(3.W)
  val vemulVd = UInt(3.W)
  val emulVs1 = UInt(4.W)
  val emulVs2 = UInt(4.W)
  val emulVd = UInt(4.W)
}

/** Calculate all info of the instruction
 *    Pure combinational logic
 */
class VInfoCalc extends Module {
  val io = IO(new Bundle {
    val ctrl = Input(new VCtrl)
    val csr = Input(new VCsr)
    val infoAll = Output(new VInfoAll)
  })
  val ctrl = io.ctrl
  val vsew = io.csr.vsew
  val vlmul = io.csr.vlmul
  val lsrc = ctrl.lsrc
  val ldest = ctrl.ldest

  /**
   * Load/Store
   */
  val ldst = ctrl.load || ctrl.store
  val ldstCtrl = LdstDecoder(ctrl.funct6, lsrc(1))
  val nfield = ctrl.funct6(5, 3) +& 1.U  // for segment load/store
  // Load/store: illegal when emul (= eew*lmul/sew) > 8 or < 1/8 except mask and whole-register ones
  val veew_ldst = Cat(false.B, ctrl.funct3(1,0))
  val veew_minus_vsew = veew_ldst -& vsew
  val vemul_ldst = Wire(SInt(4.W))
  vemul_ldst := vlmul.asSInt + veew_minus_vsew.asSInt
  // Segment: illegal when emul (lmul for indexed) * nfield > 8
  val lmul = Vlmul_to_lmul(vlmul)
  val emul_ldst = Vlmul_to_lmul(vemul_ldst(2, 0))
  val emul_lmul = Mux(ldstCtrl.indexed, lmul, emul_ldst)
  val emul_x_nfield = emul_lmul * nfield


  /** Arithmetic Integer */
  // Integer extension instructions
  val ext = ctrl.funct6 === "b010010".U && ctrl.funct3 === "b010".U
  val ext2 = ext && ctrl.lsrc(0)(2,1) === 3.U
  val ext4 = ext && ctrl.lsrc(0)(2,1) === 2.U
  val ext8 = ext && ctrl.lsrc(0)(2,1) === 1.U
  // Whole register move
  val wholeRegMv = ctrl.funct6 === "b100111".U && ctrl.funct3 === "b011".U //Whole register move
  val nreg = ctrl.lsrc(0)(2, 0) +& 1.U  //for whole register move

  /**
   * Register Number
   */
  val vnfield = Wire(UInt(3.W))
  vnfield := Mux(nfield === 8.U, 3.U, nfield >> 1)
  val vnreg = Wire(UInt(3.W))
  vnreg := Mux(nreg === 8.U, 3.U, nreg >> 1)
  // EMUL of Vd
  val vemulVd = Wire(UInt(3.W))
                                         // 15.1     or      //15.4/5/6: vmsb(o/i)f
  val mask_onlyOneReg = ctrl.mask && (ctrl.funct6(3) || ctrl.funct6(2) && !ctrl.lsrc(0)(4))
  when (ldst) {
    vemulVd := Mux(ldstCtrl.wholeReg, vnfield, Mux(ldstCtrl.mask, 0.U,
               Mux(ldstCtrl.indexed, vlmul, vemul_ldst.asUInt)))
  }.elsewhen (ctrl.narrow_to_1 || mask_onlyOneReg || ctrl.redu) {
    vemulVd := 0.U
  }.elsewhen (ctrl.widen || ctrl.widen2) {
    vemulVd := vlmul + 1.U 
  }.elsewhen (wholeRegMv) {//Whole register move
    vemulVd := vnreg
  }.otherwise {
    vemulVd := vlmul
  }
  // EMUL of Vs1
  val vemulVs1 = Wire(UInt(3.W))
  //           15.1 mask-logical          16.5 vcompress
  when (ctrl.mask && ctrl.funct6(3) || ctrl.perm && ctrl.funct6 === "b010111".U) {
    vemulVs1 := 0.U
  }.otherwise {
    vemulVs1 := vlmul
  }
  // EMUL of Vs2
  val vemulVs2 = Wire(UInt(3.W))
  when (ldst && ldstCtrl.indexed) {
    vemulVs2 := vemul_ldst(2, 0)
  }.elsewhen (ctrl.widen2 || ctrl.narrow) {
    vemulVs2 := vlmul + 1.U
  }.elsewhen (ext) {
    vemulVs2 := vlmul - Mux1H(Seq(ext2, ext4, ext8), Seq(1.U, 2.U, 3.U))
  }.elsewhen (ctrl.mask) {
    vemulVs2 := 0.U
  }.elsewhen (wholeRegMv) {//Whole register move
    vemulVs2 := vnreg
  }.otherwise {
    vemulVs2 := vlmul
  }
  // Illegal start number of register group
  def regGroup_start_illegal(vemul: UInt, startReg: UInt) = {
    vemul === 1.U && startReg(0) =/= 0.U ||
    vemul === 2.U && startReg(1, 0) =/= 0.U ||
    vemul === 3.U && startReg(2, 0) =/= 0.U
  }
  val ill_reg = regGroup_start_illegal(vemulVd, ldest) && (ctrl.ldestVal || ctrl.lsrcVal(2)) ||
                regGroup_start_illegal(vemulVs1, lsrc(0)) && ctrl.lsrcVal(0) ||
                regGroup_start_illegal(vemulVs2, lsrc(1)) && ctrl.lsrcVal(1)
 
  /** Register Group Overlap
   *  @note We use veew = b111 to represent EEW = 1
   */
  // EEW of Vd
  val veewVd = Wire(UInt(3.W)) // We use veew = b111 to represent EEW = 1
  when (ldst) {
    veewVd := Mux(ldstCtrl.wholeReg && ctrl.store || ldstCtrl.mask, 0.U,
              Mux(ldstCtrl.indexed, vsew, veew_ldst))
  }.elsewhen (ctrl.narrow_to_1 || mask_onlyOneReg) {
    veewVd := 7.U  // EEW = 1
  }.elsewhen (ctrl.widen || ctrl.widen2) {
    veewVd := vsew + 1.U
  }.otherwise {
    veewVd := vsew
  }
  // EEW of Vs1
  val veewVs1 = Wire(UInt(3.W)) // We use veew = b111 to represent EEW = 1
  //           15.1 mask-logical          16.5 vcompress
  when (ctrl.mask && ctrl.funct6(3) || ctrl.perm && ctrl.funct6 === "b010111".U) {
    veewVs1 := 7.U
  }.otherwise {
    veewVs1 := vsew
  }
  // EEW of Vs2
  val veewVs2 = Wire(UInt(3.W)) // We use veew = b111 to represent EEW = 1
  when (ldst && ldstCtrl.indexed) {
    veewVs2 := veew_ldst
  }.elsewhen (ctrl.widen2 || ctrl.narrow) {
    veewVs2 := vsew + 1.U
  }.elsewhen (ctrl.mask) {
    veewVs2 := 7.U
  }.elsewhen (ext2) {
    veewVs2 := vsew - 1.U
  }.elsewhen (ext4) {
    veewVs2 := vsew - 2.U
  }.elsewhen (ext8) {
    veewVs2 := 0.U
  }.otherwise {
    veewVs2 := vsew
  }
  
  io.infoAll.veewVs1 := veewVs1
  io.infoAll.veewVs2 := veewVs2
  io.infoAll.veewVd := veewVd 
  io.infoAll.vemulVs1 := vemulVs1
  io.infoAll.vemulVs2 := vemulVs2
  io.infoAll.vemulVd := vemulVd
  // Calculate EMUL
  io.infoAll.emulVd := Vlmul_to_lmul(vemulVd)
  io.infoAll.emulVs1 := Vlmul_to_lmul(vemulVs1)
  io.infoAll.emulVs2 := Vlmul_to_lmul(vemulVs2)
}