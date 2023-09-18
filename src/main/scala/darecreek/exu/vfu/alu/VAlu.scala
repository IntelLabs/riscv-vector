/**
  * Integer and fixed-point (except mult and div)
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
  *     16.1
  *     16.2
  *     16.6
  */
package darecreek.exu.vfu.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import chipsalliance.rocketchip.config._

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  val fixp = Bool()
}

class VIntFixpAlu64b(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val funct6 = Input(UInt(6.W))
    val funct3 = Input(UInt(3.W))
    val vi = Input(Bool()) // OPIVI: vs2 op imm
    val vm = Input(Bool())
    val vs1_imm = Input(UInt(5.W))
    val ma = Input(Bool())
    val widen = Input(Bool())  // SEW op SEW
    val widen2 = Input(Bool()) // 2*SEW op SEW
    val narrow = Input(Bool())
    val narrow_to_1 = Input(Bool())
    val sew = Input(new SewOH)
    val eewVd = Input(new SewOH) // Exclude narrow_to_1
    val uopIdx = Input(UInt(3.W))
    val vs1 = Input(UInt(64.W))
    val vs2_adder = Input(UInt(64.W))
    val vs2_misc = Input(UInt(64.W))
    val oldVd = Input(UInt(8.W))
    val vmask = Input(UInt(8.W))
    val vxrm = Input(UInt(2.W))
    val isFixp = Input(Bool())
    val isMisc = Input(Bool())
    val isSub = Input(Bool())  // subtract (exclude vrsub)

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val cmpOut = Output(UInt(8.W))
    val vxsat = Output(UInt(8.W))
    val rd = ValidIO(UInt(XLEN.W))
  })

  val vIntAdder64b = Module(new VIntAdder64b)
  vIntAdder64b.io.funct6 := io.funct6 
  vIntAdder64b.io.vm := io.vm
  vIntAdder64b.io.ma := io.ma
  vIntAdder64b.io.sew := io.sew
  vIntAdder64b.io.eewVd := io.eewVd
  vIntAdder64b.io.uopIdx := io.uopIdx
  vIntAdder64b.io.vs1 := io.vs1
  vIntAdder64b.io.vs2 := io.vs2_adder
  vIntAdder64b.io.oldVd := io.oldVd
  vIntAdder64b.io.vmask := io.vmask
  vIntAdder64b.io.isSub := io.isSub
  vIntAdder64b.io.widen := io.widen
  vIntAdder64b.io.widen2 := io.widen2
  vIntAdder64b.io.narrow_to_1 := io.narrow_to_1

  val vIntMisc64b = Module(new VIntMisc64b)
  vIntMisc64b.io.funct6 := io.funct6 
  vIntMisc64b.io.funct3 := io.funct3 
  vIntMisc64b.io.vi := io.vi
  vIntMisc64b.io.vm := io.vm
  vIntMisc64b.io.vs1_imm := io.vs1_imm
  vIntMisc64b.io.narrow := io.narrow
  vIntMisc64b.io.sew := io.sew
  vIntMisc64b.io.uopIdx := io.uopIdx
  vIntMisc64b.io.vs1 := io.vs1
  vIntMisc64b.io.vs2 := io.vs2_misc
  vIntMisc64b.io.vmask := io.vmask

  val vdAdderS1 = RegEnable(vIntAdder64b.io.vd, io.valid)
  val vdMiscS1 = RegEnable(vIntMisc64b.io.vd, io.valid)
  val isMiscS1 = RegEnable(io.isMisc, io.valid)
  val narrowVdMiscS1 = RegEnable(vIntMisc64b.io.narrowVd, io.valid)
  val cmpOutS1 = RegEnable(vIntAdder64b.io.cmpOut, io.valid)
  val isFixpS1 = RegEnable(io.isFixp, io.valid)

  val vFixPoint64b = Module(new VFixPoint64b)
  vFixPoint64b.io.funct6 := RegEnable(io.funct6, io.valid)
  vFixPoint64b.io.sew := RegEnable(io.sew, io.valid)
  vFixPoint64b.io.vxrm := RegEnable(io.vxrm, io.valid)
  vFixPoint64b.io.isFixp := RegEnable(io.isFixp, io.valid)
  vFixPoint64b.io.isSub := RegEnable(io.isSub, io.valid)
  vFixPoint64b.io.fromAdder := RegEnable(vIntAdder64b.io.toFixP, io.valid)
  vFixPoint64b.io.fromMisc := RegEnable(vIntMisc64b.io.toFixP, io.valid)

  io.vd := Mux(isFixpS1, vFixPoint64b.io.vd, Mux(isMiscS1, vdMiscS1, vdAdderS1))
  io.narrowVd := Mux(isFixpS1, vFixPoint64b.io.narrowVd, narrowVdMiscS1)
  io.cmpOut := cmpOutS1
  io.vxsat := vFixPoint64b.io.vxsat
  io.rd := RegEnable(vIntMisc64b.io.rd, io.valid)
}


class VAlu(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput)
  })

  val valid = io.in.valid
  val (funct6, funct3) = (io.in.bits.uop.ctrl.funct6, io.in.bits.uop.ctrl.funct3)
  val (vm, vs1_imm) = (io.in.bits.uop.ctrl.vm, io.in.bits.uop.ctrl.vs1_imm)
  val (widen, widen2) = (io.in.bits.uop.ctrl.widen, io.in.bits.uop.ctrl.widen2)
  val (narrow, narrow_to_1) = (io.in.bits.uop.ctrl.narrow, io.in.bits.uop.ctrl.narrow_to_1)
  val (uopIdx, uopEnd) = (io.in.bits.uop.uopIdx, io.in.bits.uop.uopEnd)
  val sew = SewOH(io.in.bits.uop.info.vsew)
  val (ma, ta) = (io.in.bits.uop.info.ma, io.in.bits.uop.info.ta)
  val (vl, vstart) = (io.in.bits.uop.info.vl, io.in.bits.uop.info.vstart)
  val oldVd = io.in.bits.oldVd
  val vs2 = io.in.bits.vs2
  //                                |sign-extend imm to 64 bits|
  val rs1Imm = Mux(io.in.bits.uop.ctrl.vi, vs1_imm.asSInt.pad(XLEN).asUInt, io.in.bits.rs1)
  val rs1Imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(128/n, rs1Imm(n-1, 0))))
  val vs1 = Mux(io.in.bits.uop.ctrl.vv, io.in.bits.vs1, rs1Imm_repeat)
  // EEW of Vd (exclude narrow_to_1)
  val veewVd = io.in.bits.uop.info.vsew + Mux(widen || widen2, 1.U, 0.U)
  val eewVd = SewOH(veewVd)

  val truthTable = TruthTable(VAluTable.table, VAluTable.default)
  val opi = funct3(0) === funct3(1)
  val decoderOut = decoder(QMCMinimizer, Cat(funct6, opi), truthTable)
  val vIntFixpDecode = decoderOut.asTypeOf(new VIntFixpDecode)

  //------- Two 64b modules form one 128b unit ------
  val vIntFixpAlu64bs = Seq.fill(2)(Module(new VIntFixpAlu64b))
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.valid := io.in.valid
    vIntFixpAlu64bs(i).io.funct6 := io.in.bits.uop.ctrl.funct6
    vIntFixpAlu64bs(i).io.funct3 := io.in.bits.uop.ctrl.funct3
    vIntFixpAlu64bs(i).io.vi := io.in.bits.uop.ctrl.vi
    vIntFixpAlu64bs(i).io.vm := io.in.bits.uop.ctrl.vm
    vIntFixpAlu64bs(i).io.vs1_imm := io.in.bits.uop.ctrl.vs1_imm
    vIntFixpAlu64bs(i).io.ma := io.in.bits.uop.info.ma
    vIntFixpAlu64bs(i).io.widen := io.in.bits.uop.ctrl.widen
    vIntFixpAlu64bs(i).io.widen2 := io.in.bits.uop.ctrl.widen2
    vIntFixpAlu64bs(i).io.narrow := io.in.bits.uop.ctrl.narrow
    vIntFixpAlu64bs(i).io.narrow_to_1 := io.in.bits.uop.ctrl.narrow_to_1
    vIntFixpAlu64bs(i).io.vxrm := io.in.bits.uop.info.vxrm
    vIntFixpAlu64bs(i).io.sew := sew
    vIntFixpAlu64bs(i).io.eewVd := eewVd
    vIntFixpAlu64bs(i).io.uopIdx := uopIdx
    vIntFixpAlu64bs(i).io.isSub := vIntFixpDecode.sub
    vIntFixpAlu64bs(i).io.isMisc := vIntFixpDecode.misc
    vIntFixpAlu64bs(i).io.isFixp := vIntFixpDecode.fixp
  }
  //------- Input extension (widen & extension instrucitons) ----
  val isVext = funct6 === "b010010".U && funct3(1, 0) === 2.U
  // Extension instructions
  val vf2 = vs1_imm(2, 1) === 3.U && isVext
  val vf4 = vs1_imm(2, 1) === 2.U && isVext
  val vf8 = vs1_imm(2, 1) === 1.U && isVext
  // Rearrange vs2
  when (widen) {
    vIntFixpAlu64bs(0).io.vs2_adder := Cat(vs2(95, 64), vs2(31, 0))
    vIntFixpAlu64bs(1).io.vs2_adder := Cat(vs2(127, 96), vs2(63, 32))
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs2_adder := vs2(63, 0)
    vIntFixpAlu64bs(1).io.vs2_adder := vs2(127, 64)
  }
  when (vf2) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(vs2(95, 64), vs2(31, 0))
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(vs2(127, 96), vs2(63, 32))
  }.elsewhen (vf4) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(vs2(111, 96), vs2(79, 64), vs2(47, 32), vs2(15, 0))
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(vs2(127, 112), vs2(95, 80), vs2(63, 48), vs2(31, 16))
  }.elsewhen (vf8) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(Seq.tabulate(8)(i => vs2(16*i+7, 16*i)).reverse)
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(Seq.tabulate(8)(i => vs2(16*i+15, 16*i+8)).reverse)
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs2_misc := vs2(63, 0)
    vIntFixpAlu64bs(1).io.vs2_misc := vs2(127, 64)
  }
  // Rearrange vs1 (need concern the case of narrow)
  when (widen || widen2 || narrow) {
    vIntFixpAlu64bs(0).io.vs1 := Cat(vs1(95, 64), vs1(31, 0))
    vIntFixpAlu64bs(1).io.vs1 := Cat(vs1(127, 96), vs1(63, 32))
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs1 := vs1(63, 0)
    vIntFixpAlu64bs(1).io.vs1 := vs1(127, 64)
  }

  //---- Input mask extraction ----
  val eewVd_is_1b = narrow_to_1
  val eewVm = Mux(eewVd_is_1b, sew, eewVd)
  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
  val mask16b = MaskExtract(io.in.bits.mask, maskIdx, eewVm)
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.vmask := 
      MaskExtract.mask16_to_2x8(mask16b, eewVm)(i)
    vIntFixpAlu64bs(i).io.oldVd := // only for compare instrution
      MaskExtract.mask16_to_2x8(MaskExtract(oldVd, uopIdx, sew), sew)(i)
  }

  /**
   * Output stage
   */
  val funct6_S1 = RegEnable(funct6, valid)
  val opi_S1 = RegEnable(opi, valid)
  val uopIdxS1 = RegEnable(uopIdx, valid)
  val old_vd_S1 = Wire(UInt(128.W))
  old_vd_S1 := RegEnable(oldVd, valid)
  val eewVs1S1 = RegEnable(sew, valid)
  val eewVdS1 = RegEnable(eewVd, valid)
  val narrowS1 = RegEnable(narrow, valid)
  val cmpFlagS1 = RegEnable(eewVd_is_1b, valid) // Compare and carry-out
  val vmS1 = RegEnable(vm, valid)
  val taS1 = RegEnable(ta, valid)
  val maS1 = RegEnable(ma, valid)
  val mask16bS1 = RegEnable(mask16b, valid)
  val vl_S1 = RegEnable(vl, valid)
  val vstartS1 = RegEnable(vstart, valid)
  //---- Narrowing vd rearrangement ----
  val catNarrowVd = Cat(vIntFixpAlu64bs(1).io.narrowVd, vIntFixpAlu64bs(0).io.narrowVd)
  val vdOfNarrow = Mux(uopIdxS1(0), Cat(catNarrowVd, old_vd_S1(63, 0)),
                       Cat(old_vd_S1(127, 64), catNarrowVd))
  //---- Compare/carry-out vd rearrangement ----
  val cmpOuts = vIntFixpAlu64bs.map(_.io.cmpOut)
  val cmpOut128b = Mux1H(eewVs1S1.oneHot, Seq(8,4,2,1).map(
                    k => Cat(0.U((128-2*k).W), cmpOuts(1)(k-1,0), cmpOuts(0)(k-1,0))))
  val cmpOutOff128b = Mux1H(eewVs1S1.oneHot, Seq(8,4,2,1).map(
                    k => Cat(0.U((128-2*k).W), ~0.U((2*k).W))))
  val shiftCmpOut = Wire(UInt(7.W))
  shiftCmpOut := Mux1H(eewVs1S1.oneHot, Seq(4,3,2,1).map(i => uopIdxS1(2, 0) << i))
  val cmpOutKeep = Wire(UInt(128.W))
  cmpOutKeep := cmpOut128b << shiftCmpOut
  val cmpOutOff = Wire(UInt(128.W))
  cmpOutOff := ~(cmpOutOff128b << shiftCmpOut)
  // val cmpOutResult = old_vd_S1 & cmpOutOff | cmpOutKeep // Compare and carry-out instrns

  /** Change cmpOutResult generation: 
   *    use a internal register to hold last compare-out
   */
  val old_cmpOutResult = Reg(UInt(128.W))
  val cmpOutResult = old_cmpOutResult & cmpOutOff | cmpOutKeep // Compare and carry-out instrns
  val uopEndS1 = RegEnable(uopEnd, valid)
  when (RegNext(valid)) {
    old_cmpOutResult := Mux(uopEndS1, 0.U, cmpOutResult)
  }
  io.out.valid := RegNext(Mux(narrow_to_1, uopEnd && valid, valid), init = false.B)

  /**
   * Output tail/prestart/mask handling for eewVd >= 8
   */
  //---- evl (for Whole Reg Move) ----
  val evl = Wire(UInt(bVL.W)) // Only used for whole reg move
  val nreg = vs1_imm(2, 0) +& 1.U  // emul = nreg, nreg = simm[2:0] + 1
  val vlen_div_sew = Mux1H(sew.oneHot, Seq(1,2,4,8).map(k => (VLENB / k).U))
  evl := Mux1H(Seq( // EMUL*VLEN/SEW
    (nreg === 1.U) -> vlen_div_sew,
    (nreg === 2.U) -> (vlen_div_sew << 1),
    (nreg === 4.U) -> (vlen_div_sew << 2),
    (nreg === 8.U) -> (vlen_div_sew << 3)
  ))
  val isWholeRegMv = funct6 === "b100111".U && funct3 === "b011".U
  //---- Tail gen ----
  val isPermVmv = funct6 === "b010000".U && !opi
  val tail = TailGen(Mux(isPermVmv, 1.U, Mux(isWholeRegMv, evl, vl)), uopIdx, eewVd, narrow)
  // val tail = TailGen(vl, uopIdx, eewVd, narrow)
  val tailS1 = RegEnable(tail, valid)
  //---- Prestart gen ----
  // val prestart = PrestartGen(vstart, uopIdx, Mux(narrow, eewVs2, eewVd))
  val prestart = PrestartGen(vstart, uopIdx, eewVd, narrow)
  val prestartS1 = RegEnable(prestart, valid)
  //---- vstart >= vl ----
  val vstart_gte_vl = Mux(isWholeRegMv, vstart >= evl, vstart >= vl)
  val vstart_gte_vl_S1 = RegEnable(vstart_gte_vl, valid)

  val tailReorg = MaskReorg.splash(tailS1, eewVdS1)
  val prestartReorg = MaskReorg.splash(prestartS1, eewVdS1)
  val mask16bReorg = MaskReorg.splash(mask16bS1, eewVdS1)
  val updateType = Wire(Vec(16, UInt(2.W))) // 00: keep result  10: old_vd  11: write 1s
  for (i <- 0 until 16) {
    when (prestartReorg(i) || vstart_gte_vl_S1) {
      updateType(i) := 2.U
    }.elsewhen (tailReorg(i)) {
      updateType(i) := Mux(taS1, 3.U, 2.U)
    // }.elsewhen (opcodeS1.isAddWithCarry || opcodeS1.isVmerge) {
    }.elsewhen (funct6_S1(5,2) === "b0100".U && opi_S1 || funct6_S1 === "b010111".U && !vmS1) {
      updateType(i) := 0.U
    }.elsewhen (!vmS1 && !mask16bReorg(i)) {
      updateType(i) := Mux(maS1, 3.U, 2.U)
    }.otherwise {
      updateType(i) := 0.U
    }
  }
  // finalResult = result & bitsKeep | bitsReplace   (all are 128 bits)
  val bitsKeep = Cat(updateType.map(x => Mux(x(1), 0.U(8.W), ~0.U(8.W))).reverse)
  val bitsReplace = Cat(updateType.zipWithIndex.map({case (x, i) => 
        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(old_vd_S1, 8)(i)))}).reverse)

  /**
   * Output tail/prestart/mask handling for eewVd == 1
   */
  val tail_1b_temp = UIntToCont0s(vl_S1(bVL-2, 0), bVL-1)
  require(tail_1b_temp.getWidth == 128)
  val tail_1b = Mux(vl_S1 === 128.U, 0.U(128.W), tail_1b_temp)
  val prestart_1b = UIntToCont1s(vstartS1, bVSTART)
  require(prestart_1b.getWidth == 128)
  val bitsKeep_1b = ~(prestart_1b | tail_1b)
  val bitsReplace_1b = Mux(vstart_gte_vl_S1, old_vd_S1, 
                       prestart_1b & old_vd_S1 | tail_1b)

  val bitsKeepFinal = Mux(cmpFlagS1, bitsKeep_1b, bitsKeep)
  val bitsReplaceFinal = Mux(cmpFlagS1, bitsReplace_1b, bitsReplace)

  val vdResult = Mux(narrowS1, vdOfNarrow, 
              Mux(cmpFlagS1, cmpOutResult, Cat(vIntFixpAlu64bs.map(_.io.vd).reverse)))
  io.out.bits.vd := Mux(vIntFixpAlu64bs(0).io.rd.valid, vIntFixpAlu64bs(0).io.rd.bits,
                        vdResult & bitsKeepFinal | bitsReplaceFinal)
  when (!narrowS1) {
    io.out.bits.vxsat := (Cat(vIntFixpAlu64bs.map(_.io.vxsat).reverse) &
                     Cat(updateType.map(_(1) === false.B).reverse)).orR
  }.otherwise {
    io.out.bits.vxsat := (Cat(vIntFixpAlu64bs.map(_.io.vxsat(VLENB/4 - 1, 0)).reverse) &
                     Mux(uopIdxS1(0), Cat(updateType.drop(VLENB/2).map(_(1) === false.B).reverse),
                                      Cat(updateType.take(VLENB/2).map(_(1) === false.B).reverse))
                     ).orR
  }
}


import xiangshan._
object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({case XSCoreParamsKey => XSCoreParameters()})
  emitVerilog(new VAlu()(p.alterPartial({case VFuParamsKey => VFuParameters()})), Array("--target-dir", "generated",
              "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}