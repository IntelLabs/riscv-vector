package darecreek

import chisel3._
import chisel3.util._
import darecreek.lsu._

class PartialVInfo extends Bundle {
  val vRobPtr = new VRobPtr
  // val destEew = UInt(3.W)
  val emulVd = UInt(4.W)
}

class VIllegalInstrn extends Module {
  val io = IO(new Bundle {
    val ctrl = Input(new VCtrl)
    val info = Input(new VInfo)
    val robPtrIn = Input(new VRobPtr)
    val validIn = Input(Bool())
    val ill = ValidIO(new VRobPtr)
    val partialVInfo = ValidIO(new PartialVInfo)
  })
  val ctrl = io.ctrl
  val info = io.info
  val vsew = info.vsew
  val vlmul = info.vlmul
  val lsrc = ctrl.lsrc
  val ldest = ctrl.ldest
  // vsew
  val ill_vsew = vsew(2)
  // vlmul
  val ill_vlmul = vlmul === "b100".U
  // widen/narrow, illegal when lmul = 8 or sew = 64
  val ill_widenNarrow = (vlmul === 3.U || vsew === 3.U) && (ctrl.widen || ctrl.narrow)
  // vstart: non-zero vstart for arithmetic instrns
  val ill_vstart = info.vstart =/= 0.U && ctrl.arith

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
  val ill_ldstEmul = ldst && (vemul_ldst > 3.S || vemul_ldst < -3.S) &&
                     !ldstCtrl.mask && !ldstCtrl.wholeReg
  // Segment: illegal when emul (lmul for indexed) * nfield > 8
  val lmul = Vlmul_to_lmul(vlmul)
  val emul_ldst = Vlmul_to_lmul(vemul_ldst(2, 0))
  val emul_lmul = Mux(ldstCtrl.indexed, lmul, emul_ldst)
  val emul_x_nfield = emul_lmul * nfield
  val ill_seg = ldst && emul_x_nfield > 8.U
  // Segment: illegal when reg numbers accessed would increment past 31
  val vdEnd_seg = ldest + emul_x_nfield - 1.U
  val ill_seg_past31 = ldst && vdEnd_seg > 31.U
  // Whole regsiter load/store
  val ill_nfield = ldst && ldstCtrl.wholeReg && !(nfield === 1.U || nfield === 2.U || nfield === 4.U || nfield === 8.U)


  /** Arithmetic Integer */
  // Integer extension instructions
  val ext = ctrl.funct6 === "b010010".U && ctrl.funct3 === "b010".U
  val ext2 = ext && ctrl.lsrc(0)(2,1) === 3.U
  val ext4 = ext && ctrl.lsrc(0)(2,1) === 2.U
  val ext8 = ext && ctrl.lsrc(0)(2,1) === 1.U
  val ill_ext2 = ext2 && (vlmul === 5.U || vsew === 0.U)
  val ill_ext4 = ext4 && (vlmul === 5.U || vlmul === 6.U || vsew(2, 1) === 0.U)
  val ill_ext8 = ext8 && (vlmul(2) || vsew =/= 3.U)
  val ill_ext = ill_ext2 || ill_ext4 || ill_ext8
  // Whole register move
  val wholeRegMv = ctrl.funct6 === "b100111".U && ctrl.funct3 === "b011".U //Whole register move
  val nreg = ctrl.lsrc(0)(2, 0) +& 1.U  //for whole register move
  val ill_nreg = wholeRegMv && !(nreg === 1.U || nreg === 2.U || nreg === 4.U || nreg === 8.U)
  
  /** Arithmetic Floating-point */
  // invalid rounding mode
  val ill_frm = info.frm(2) && info.frm(1, 0) =/= 0.U && ctrl.fp
  // invalid SEW of FP
  val ill_sewFP = !vsew(1) && ctrl.fp

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
  // Calculate EMUL
  val emulVd = Vlmul_to_lmul(vemulVd)
  val emulVs1 = Vlmul_to_lmul(vemulVs1)
  val emulVs2 = Vlmul_to_lmul(vemulVs2)
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
                        !overlap_isLegal(vs1, vs1End, veewVs1, vemulVs1, vd, vdEnd, veewVd)
  val ill_regOverlap_2 = overlap(vs2, vs2End, ctrl.lsrcVal(1), vd, vdEnd, ctrl.ldestVal) &&
                        !overlap_isLegal(vs2, vs2End, veewVs2, vemulVs2, vd, vdEnd, veewVd)
  val ill_regOverlap_m = overlap(0.U, 0.U, !ctrl.vm, vd, vdEnd, ctrl.ldestVal) &&
                        !overlap_isLegal(0.U, 0.U, 7.U, 0.U, vd, vdEnd, veewVd)
  val ill_regOverlap = ill_regOverlap_1 || ill_regOverlap_2 || ill_regOverlap_m

  // Segment: for indexed segment, vd reg-group cannot overlap vs2 reg-group
  val ill_segOverlap = ctrl.load && overlap(vs2, vs2End, ctrl.lsrcVal(1), vd, vdEnd_seg(5, 0), ctrl.ldestVal)

  val illFinal = ill_vsew || ill_vlmul || ill_widenNarrow || ill_vstart ||
               ill_ldstEmul || ill_seg || ill_seg_past31 || ill_nfield ||
               ill_ext || ill_nreg || ill_frm || ill_sewFP ||
               ill_reg || ill_regGrpEnd || ill_regOverlap || ill_segOverlap
  io.ill.valid := RegNext(illFinal || io.ctrl.illegal || io.info.vill) && RegNext(io.validIn)
  io.ill.bits := RegNext(io.robPtrIn)

  io.partialVInfo.valid := RegNext(io.validIn)
  io.partialVInfo.bits.vRobPtr := RegEnable(io.robPtrIn, io.validIn)
  // io.partialVInfo.bits.destEew := RegEnable(?, io.validIn)
  io.partialVInfo.bits.emulVd := RegEnable(emulVd, io.validIn)
}