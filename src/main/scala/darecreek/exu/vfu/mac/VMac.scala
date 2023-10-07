package darecreek.exu.vfu.mac

import chisel3._
import chisel3.util._
import darecreek.exu.vfu._
// import darecreek.exu.vfu.VFUParam._
import org.chipsalliance.cde.config._

class VMac(implicit p: Parameters) extends VFuModule {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VFuInput))
    val out = ValidIO(new VAluOutput) // Same as ALU
  })

  // Latency of MAC is 2 cycles plus
  val valid = io.in.valid
  val validS1 = RegNext(valid)
  io.out.valid := RegNext(validS1)

  val (funct6, funct3) = (io.in.bits.uop.ctrl.funct6, io.in.bits.uop.ctrl.funct3)
  val sew = SewOH(io.in.bits.uop.info.vsew)
  val widen = io.in.bits.uop.ctrl.widen
  val veewVd = io.in.bits.uop.info.vsew + Mux(widen, 1.U, 0.U)
  val eewVd = SewOH(veewVd)
  val vs2 = io.in.bits.vs2
  //                                |sign-extend imm to 64 bits|
  val rs1Imm = Mux(io.in.bits.uop.ctrl.vi, io.in.bits.uop.ctrl.vs1_imm.asSInt.pad(XLEN).asUInt, io.in.bits.rs1)
  val rs1Imm_repeat = Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => Fill(128/n, rs1Imm(n-1, 0))))
  val vs1 = Mux(io.in.bits.uop.ctrl.vv, io.in.bits.vs1, rs1Imm_repeat)
  val oldVd = io.in.bits.oldVd
  val mask = io.in.bits.mask
  val uopIdx = io.in.bits.uop.uopIdx
  val vstart_gte_vl = io.in.bits.uop.info.vstart >= io.in.bits.uop.info.vl

  val highHalf = funct6(4,2) === "b001".U && funct6(1,0) =/= "b01".U // && funct3(1,0) === "b10".U
  val isMacc = funct6(4,3) === "b01".U || funct6(4,2) === "b111".U
  val isSub = funct6(4,3) === "b01".U && funct6(1,0) === "b11".U
  val isFixP = funct6 === "b100111".U && funct3(1,0) === 0.U
  val overWriteMultiplicand = funct6(4,2) === "b010".U
  val vs2_is_signed = !(funct6(4,0) === "b11111".U || funct6(1,0) === 0.U)
  val vs1_is_signed = funct6(4,2) === "b001".U && funct6(0) || funct6(4,3) === "b01".U ||
                               funct6(4,3) === "b11".U && funct6(0)

  val vIMac64bs = Seq.fill(2)(Module(new VMac64b))
  for (i <- 0 until 2) {
    vIMac64bs(i).io.fireIn := valid
    vIMac64bs(i).io.fireS1 := validS1
    vIMac64bs(i).io.sew := sew
    vIMac64bs(i).io.uopIdx := uopIdx
    vIMac64bs(i).io.vxrm := io.in.bits.uop.info.vxrm
    vIMac64bs(i).io.vs1_is_signed := vs1_is_signed
    vIMac64bs(i).io.vs2_is_signed := vs2_is_signed
    vIMac64bs(i).io.highHalf := highHalf
    vIMac64bs(i).io.isMacc := isMacc
    vIMac64bs(i).io.isSub := isSub
    vIMac64bs(i).io.widen := widen
    vIMac64bs(i).io.isFixP := isFixP
    vIMac64bs(i).io.vs1 := Mux(widen, Cat(UIntSplit(vs1, 32)(i+2), UIntSplit(vs1, 32)(i)),
                               UIntSplit(vs1, 64)(i))
    vIMac64bs(i).io.vs2 := Mux(overWriteMultiplicand, UIntSplit(oldVd, 64)(i),
                           Mux(widen, Cat(UIntSplit(vs2, 32)(i+2), UIntSplit(vs2, 32)(i)),
                               UIntSplit(vs2, 64)(i)))
    vIMac64bs(i).io.oldVd := Mux(overWriteMultiplicand, UIntSplit(vs2, 64)(i),
                                 UIntSplit(oldVd, 64)(i))
  }

  /**
   * Output stage
   */
  val eewVdS1 = RegEnable(eewVd, valid)
  val oldVdS1 = RegEnable(oldVd, valid)
  val taS1 = RegEnable(io.in.bits.uop.info.ta, valid)
  val maS1 = RegEnable(io.in.bits.uop.info.ma, valid)
  val vmS1 = RegEnable(io.in.bits.uop.ctrl.vm, valid)
  val vstart_gte_vl_S1 = RegEnable(vstart_gte_vl, valid)
  val mask16bS1 = RegEnable(MaskExtract(mask, uopIdx, eewVd), valid)

  val eewVdS2 = RegEnable(eewVdS1, validS1)
  val oldVdS2 = Wire(UInt(128.W))
  oldVdS2 := RegEnable(oldVdS1, validS1)
  val taS2 = RegEnable(taS1, validS1)
  val maS2 = RegEnable(maS1, validS1)
  val vmS2 = RegEnable(vmS1, validS1)
  // Output tail/prestart/mask handling
  //---- Tail gen ----
  val tail = TailGen(io.in.bits.uop.info.vl, uopIdx, eewVd)
  val tailS1 = RegEnable(tail, valid)
  val tailS2 = RegEnable(tailS1, validS1)
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.uop.info.vstart, uopIdx, eewVd)
  val prestartS1 = RegEnable(prestart, valid)
  val prestartS2 = RegEnable(prestartS1, validS1)
  //---- vstart >= vl ----
  val vstart_gte_vl_S2 = RegEnable(vstart_gte_vl_S1, validS1)

  val tailReorg = MaskReorg.splash(tailS2, eewVdS2)
  val prestartReorg = MaskReorg.splash(prestartS2, eewVdS2)
  val mask16bS2 = RegEnable(mask16bS1, validS1)
  val mask16bReorg = MaskReorg.splash(mask16bS2, eewVdS2)
  val updateType = Wire(Vec(16, UInt(2.W))) // 00: keep result  10: old_vd  11: write 1s
  for (i <- 0 until 16) {
    when (prestartReorg(i) || vstart_gte_vl_S2) {
      updateType(i) := 2.U
    }.elsewhen (tailReorg(i)) {
      updateType(i) := Mux(taS2, 3.U, 2.U)
    }.elsewhen (!vmS2 && !mask16bReorg(i)) {
      updateType(i) := Mux(maS2, 3.U, 2.U)
    }.otherwise {
      updateType(i) := 0.U
    }
  }
  // finalResult = result & bitsKeep | bitsReplace   (all are 128 bits)
  val bitsKeep = Cat(updateType.map(x => Mux(x(1), 0.U(8.W), ~0.U(8.W))).reverse)
  val bitsReplace = Cat(updateType.zipWithIndex.map({case (x, i) => 
        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(oldVdS2, 8)(i)))}).reverse)

  val vdResult = Cat(vIMac64bs(1).io.vd, vIMac64bs(0).io.vd)
  io.out.bits.vd := vdResult & bitsKeep | bitsReplace
  io.out.bits.vxsat := (Cat(vIMac64bs.map(_.io.vxsat).reverse) &
                   Cat(updateType.map(_(1) === false.B).reverse)).orR
}

import xiangshan._
object Main extends App {
  println("Generating hardware")
  val p = Parameters.empty.alterPartial({case XSCoreParamsKey => XSCoreParameters()})
  emitVerilog(new VMac()(p.alterPartial({case VFuParamsKey => VFuParameters()})), Array("--target-dir", "generated",
              "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}