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

class VIllegalInstrn extends Module {
  val io = IO(new Bundle {
    val ctrl = Input(new VCtrl)
    val csr = Input(new VCsr)
    val infoAll = Input(new VInfoAll)
    val robPtrIn = Input(new VRobPtr)
    val validIn = Input(Bool())
    val extraInfo_for_VIllegal = Input(new Bundle {
      val ldst = Bool()
      val ldstCtrl = new LdstCtrl
      val vemul_ldst = SInt(4.W)
      val ext2 = Bool() // Integer extension instructions
      val ext4 = Bool()
      val ext8 = Bool()
      val wholeRegMv = Bool() // Whole register move
      val nreg = UInt(4.W)  // Whole register move
    })
    val ill = ValidIO(new VRobPtr)
  })
  val ctrl = io.ctrl
  val csr = io.csr
  val vsew = csr.vsew
  val vlmul = csr.vlmul
  val lsrc = ctrl.lsrc
  val ldest = ctrl.ldest

  val (veewVs1, veewVs2, veewVd) = (io.infoAll.veewVs1, io.infoAll.veewVs2, io.infoAll.veewVd)
  val (vemulVs1, vemulVs2, vemulVd) = (io.infoAll.vemulVs1, io.infoAll.vemulVs2, io.infoAll.vemulVd)
  val (emulVs1, emulVs2, emulVd) = (io.infoAll.emulVs1, io.infoAll.emulVs2, io.infoAll.emulVd)

  // vsew
  val ill_vsew = vsew(2)
  // vlmul
  val ill_vlmul = vlmul === "b100".U
  // widen/narrow, illegal when lmul = 8 or sew = 64
  val ill_widenNarrow_sew = (vsew === 3.U) && (ctrl.widen || ctrl.narrow || ctrl.widen2)
  val ill_widenNarrow_lmul = (vlmul === 3.U)  && (ctrl.widen || ctrl.narrow || ctrl.widen2) && ~(ctrl.redu && (ctrl.funct3 === "b000".U || ctrl.funct3 === "b001".U))
  val ill_widenNarrow = ill_widenNarrow_sew || ill_widenNarrow_lmul
  // vstart: non-zero vstart for arithmetic instrns
  val ill_vstart = csr.vstart =/= 0.U && ctrl.arith
  //VrgatherEi16VV, when lmul =8, sew can not be 32
  val ill_gatherE16 = (ctrl.funct3 === 0.U && ctrl.funct6 === "b001110".U && vlmul === 3.U && vsew === 0.U)

  /**
   * Load/Store
   */
  val ldst = io.extraInfo_for_VIllegal.ldst
  val ldstCtrl = io.extraInfo_for_VIllegal.ldstCtrl
  val nfield = ctrl.funct6(5, 3) +& 1.U  // for segment load/store
  val vemul_ldst = io.extraInfo_for_VIllegal.vemul_ldst
  val ill_ldstEmul = ldst && (vemul_ldst > 3.S || vemul_ldst < -3.S) &&
                     !ldstCtrl.mask && !ldstCtrl.wholeReg
  // Segment: illegal when emul (lmul for indexed) * nfield > 8
  val lmul = Vlmul_to_lmul(vlmul)
  val emul_ldst = Vlmul_to_lmul(vemul_ldst(2, 0))
  val emul_lmul = Mux(ldstCtrl.indexed, lmul, emul_ldst)
  val emul_x_nfield = emul_lmul * nfield
  val ill_seg = ldst && ldstCtrl.segment && emul_x_nfield > 8.U
  // Segment: illegal when reg numbers accessed would increment past 31
  val vdEnd_seg = ldest + emul_x_nfield - 1.U
  val ill_seg_past31 = ldst && ldstCtrl.segment && vdEnd_seg > 31.U
  // Whole regsiter load/store
  val ill_nfield = ldst && ldstCtrl.wholeReg && !(nfield === 1.U || nfield === 2.U || nfield === 4.U || nfield === 8.U)

  /** Arithmetic Integer */
  // Integer extension instructions
  val ill_ext2 = io.extraInfo_for_VIllegal.ext2 && (vlmul === 5.U || vsew === 0.U)
  val ill_ext4 = io.extraInfo_for_VIllegal.ext4 && (vlmul === 5.U || vlmul === 6.U || vsew(2, 1) === 0.U)
  val ill_ext8 = io.extraInfo_for_VIllegal.ext8 && (vlmul(2) || vsew =/= 3.U)
  val ill_ext = ill_ext2 || ill_ext4 || ill_ext8
  // Whole register move
  val wholeRegMv = io.extraInfo_for_VIllegal.wholeRegMv
  val nreg = io.extraInfo_for_VIllegal.nreg
  val ill_nreg = wholeRegMv && !(nreg === 1.U || nreg === 2.U || nreg === 4.U || nreg === 8.U)
  
  /** Arithmetic Floating-point */
  // invalid rounding mode

  //**FixMeWQW **/
  val isFp = ctrl.fp || ctrl.funct3 === "b001".U || (ctrl.funct3 === "b101".U && ~ctrl.isLdst)
  val vfncvt_xu_f = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10000".U
  val vfncvt_x_f  = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10001".U
  val vfncvt_rtz_xu_f = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10110".U
  val vfncvt_rtz_x_f  = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10111".U
  //val vfncvt_f_xu = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10010".U
  //val vfncvt_f_x  = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10011".U
  //val vfncvt_f_f = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10100".U
  //val vfncvt_rod_f_f = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.lsrc(0) === "b10101".U
  val vfwcvt_f_x = ctrl.fp && ctrl.funct6 === "b010010".U && ctrl.widen && 
                   (ctrl.lsrc(0) === "b01010".U || ctrl.lsrc(0) === "b01011".U)

  val vrgather = ctrl.perm && (ctrl.funct6 === "b001100".U || ctrl.funct6 === "b001110".U && ctrl.funct3 === "b000".U)
  val viota    = ctrl.mask && ctrl.funct6 === "b010100".U && ctrl.lsrc(0) === "b10000".U

  val convertToInt = vfncvt_xu_f || vfncvt_x_f || vfncvt_rtz_xu_f || vfncvt_rtz_x_f // || vfncvt_f_xu || vfncvt_f_x


  val ill_frm = csr.frm(2) && csr.frm(1, 0) =/= 0.U && isFp
  // invalid SEW of FP
  val ill_sewFP = (vsew === 0.U && (isFp)) || (vsew === 1.U && (isFp && ~convertToInt && ~vfwcvt_f_x))

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
  // Calculate end of register-group
  val (vd, vdEnd) = (ldest, ldest +& emulVd - 1.U)
  val (vs1, vs1End) = (lsrc(0), lsrc(0) +& emulVs1 - 1.U)
  val (vs2, vs2End) = (lsrc(1), lsrc(1) +& emulVs2 - 1.U)
  // Illegal end of reg-group
  val ill_regGrpEnd = vs1End(5) && ctrl.lsrcVal(0) || vs2End(5) && ctrl.lsrcVal(1) || vdEnd(5) && ctrl.ldestVal
  // Overlap
  def overlap(vs: UInt, vsEnd: UInt, vsVal: Bool, 
              vd: UInt, vdEnd: UInt, vdVal: Bool) = {
    !(vsEnd < vd || vdEnd < vs) && vsVal && vdVal
  }
  def overlap_isLegal(vs: UInt, vsEnd: UInt, veewVs: UInt, vemulVs: UInt, 
                      vd: UInt, vdEnd: UInt, veewVd: UInt) = {
    // Case 1: The destination EEW equals the source EEW.
    val case1 = veewVs === veewVd
    // Case 2: The destination EEW is smaller than the source EEW and the overlap is in 
    //         the lowest-numbered part of the source register group 
    //         (e.g., when LMUL=1, vnsrl.wi v0, v0, 3 is legal, but a destination of v1 is not).
    val case2 = veewVd.asSInt < veewVs.asSInt && vs === vd
    // Case 3: The destination EEW is greater than the source EEW, the source EMUL is at least 1,
    //         and the overlap is in the highestnumbered part of the destination register group 
    //         (e.g., when LMUL=8, vzext.vf4 v0, v6 is legal, but a source of v0, v2, or v4 is not).
    val case3 = veewVd.asSInt > veewVs.asSInt && !vemulVs(2) && vsEnd === vdEnd
    case1 || case2 || case3
  }
  // illegal overlap = vd/vs1_overlap || vd/vs2_overlap || vd/vmask_overlap
  val ill_regOverlap_1 = overlap(vs1, vs1End, ctrl.lsrcVal(0), vd, vdEnd, ctrl.ldestVal) &&
                        (!overlap_isLegal(vs1, vs1End, veewVs1, vemulVs1, vd, vdEnd, veewVd) || vrgather || viota)
  val ill_regOverlap_2 = overlap(vs2, vs2End, ctrl.lsrcVal(1), vd, vdEnd, ctrl.ldestVal) &&
                        (!overlap_isLegal(vs2, vs2End, veewVs2, vemulVs2, vd, vdEnd, veewVd) || vrgather || viota)
  val ill_regOverlap_m = overlap(0.U, 0.U, !ctrl.vm, vd, vdEnd, ctrl.ldestVal) &&
                        (!overlap_isLegal(0.U, 0.U, 7.U, 0.U, vd, vdEnd, veewVd) || viota)
  val ill_regOverlap = ill_regOverlap_1 || ill_regOverlap_2 || ill_regOverlap_m

  // Segment: for indexed segment, vd reg-group cannot overlap vs2 reg-group
  val ill_segOverlap = ctrl.load && ldstCtrl.segment && ldstCtrl.indexed && overlap(vs2, vs2End, ctrl.lsrcVal(1), vd, vdEnd_seg(5, 0), ctrl.ldestVal)

  /** Some instructions does not support vd = vs2:
   *    vslideup, v(f)slide1up, vrgather, vrgather16, vmsbf, vmsif, vmsof, vcompress
   */
  val slideup_gather = ctrl.funct6(5, 2) === "b0011".U && ctrl.funct6(1, 0) =/= "b11".U
  val vmsxf = ctrl.funct6 === "b010100".U && !ctrl.lsrc(0)(4) 
  val vcompress = ctrl.funct6 === "b010111".U && ctrl.opm
  val ill_vd_vs2 = ctrl.ldest === ctrl.lsrc(1) && (slideup_gather || vmsxf || vcompress)

  /** The destination vector register group for a masked vector instruction cannot overlap 
   *  the source mask register (v0), unless the destination vector register is being written
   *  with a mask value (e.g., compares) or the scalar result of a reduction.
   */
  val ill_vd_v0 = ctrl.ldest === 0.U && ctrl.ldestVal && !ctrl.vm && !(ctrl.redu ||
                  ctrl.mask && ctrl.rdVal ||
                  ctrl.funct6(5, 3) === "b011".U ||  //compare
                  ctrl.funct6(5, 2) === "b0100".U && ctrl.funct6(0) && ctrl.opi) //vmadc/vmsbc

  val illFinal = ill_vsew || ill_vlmul || ill_widenNarrow || ill_vstart || ill_gatherE16 ||
               ill_ldstEmul || ill_seg || ill_seg_past31 || ill_nfield ||
               ill_ext || ill_nreg || ill_frm || ill_sewFP ||
               ill_reg || ill_regGrpEnd || ill_regOverlap || ill_segOverlap ||
               ill_vd_vs2 || ill_vd_v0
  io.ill.valid := RegNext(illFinal || io.ctrl.illegal || io.csr.vill) && RegNext(io.validIn)
  io.ill.bits := RegNext(io.robPtrIn)
}