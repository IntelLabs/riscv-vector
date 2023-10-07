package darecreek.exu.vfu.fp

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import fudian.FDIV


// multi-cycle data module
class VFDivSqrtDataModule(implicit val p: Parameters) extends VFPUDivSubModule {
  val kill_w = IO(Input(Bool()))
  val kill_r = IO(Input(Bool()))

  val in_fire = io.in.valid && io.in.ready
//  val out_fire = out_valid && out_ready

  val uop = io.in.bits.uop
  val fpCtrl = uop.vfpCtrl
  val tag = uop.typeTag
  val dividend = io.in.bits.vs2
  val divisor = io.in.bits.vs1

  val typeSel = VecInit(VFPU.ftypes.zipWithIndex.map(_._2.U === tag))
  val outSel = RegEnable(typeSel, VecInit(Seq.fill(typeSel.length)(true.B)), in_fire)  // inelegant

  val divSqrt32 = Seq(genDivSqrt(VFPU.f32, dividend.head(32), divisor.head(32)),
                   genDivSqrt(VFPU.f32, dividend.tail(32), divisor.tail(32)))
  val divSqrt64 = Seq(genDivSqrt(VFPU.f64, dividend, divisor))

  val result32 = Cat(divSqrt32(0).io.result, divSqrt32(1).io.result)
  val result64 = divSqrt64(0).io.result
  io.out.bits.vd := Mux1H(outSel, Seq(result32, result64))
  // active signal should be buffered, since it takes multiple cycles to generate output.
  // val activeBuf = RegEnable(VecInit(Seq(0,1).map(isActive)), in_fire)
  // Why (0, 4)?  Floating-point only has sew = 32 and 64. For 64, only need bit-0. For 32, the bit-0 and bit-4 are used.
  val activeBuf32 = RegEnable(VecInit(Seq(0,4).map(isActive)), in_fire)
  val activeBuf = activeBuf32

  val fflags64 = Mux(activeBuf(0), divSqrt64(0).io.fflags, 0.U(5.W))
  // note: mind the index
  val fflags32 = Mux(activeBuf(0), divSqrt32(1).io.fflags, 0.U(5.W)) |
                    Mux (activeBuf(1), divSqrt32(0).io.fflags, 0.U(5.W))
  io.out.bits.fflags := Mux1H(outSel, Seq(fflags32, fflags64))
  io.in.ready := (divSqrt32 ++ divSqrt64).map(_.io.specialIO.in_ready).foldRight(true.B)(_ && _)

  // results of two fp32 may emerge in different cycles,
  // but the FDIV module only generates valid pulse
  val validBuf32 = RegInit(VecInit(Seq.fill(2)(false.B)))
  when(validBuf32(0) && validBuf32(1)) {
    validBuf32(0) := false.B
    validBuf32(1) := false.B
  } .otherwise {
    when(divSqrt32(0).io.specialIO.out_valid) { validBuf32(0) := true.B}
    when(divSqrt32(1).io.specialIO.out_valid) { validBuf32(1) := true.B}
  }
  io.out.valid := Mux1H(outSel, Seq(
    validBuf32.reduce(_ && _),
    divSqrt64(0).io.specialIO.out_valid)
  )
  io.out.bits.uop := io.in.bits.uop // this signal should be not used
  def genDivSqrt(t: VFPU.FType, src1: UInt, src2: UInt) = {
    val fdiv = Module(new FDIV(t.expWidth, t.precision))
    fdiv.io.a := src1
    fdiv.io.b := src2
    fdiv.io.rm := io.in.bits.uop.info.frm // CHECK it !
    fdiv.io.specialIO.in_valid := in_fire && !kill_w && (VFPU.ftypes.indexOf(t).U === tag)
    fdiv.io.specialIO.out_ready := io.out.ready
    fdiv.io.specialIO.isSqrt := fpCtrl.isSqrt
    fdiv.io.specialIO.kill := kill_r
    fdiv
  }
}

// DIV: vs2 / vs1 normally, vs1 / vs2 if reverse
// SQRT: sqrt vs2
class VFDivSqrt(implicit val p: Parameters) extends VFPUDivSubModule {

  val kill_r = false.B
  // val kill_r = !io.in.ready && uopReg.robIdx.needFlush(io.redirectIn)

  val dataModule = Module(new VFDivSqrtDataModule)

  dataModule.io.in <> io.in
  io.out <> dataModule.io.out

  // override valid signal
  dataModule.io.in.valid := io.in.valid && io.in.bits.uop.vfpCtrl.isDivSqrt
  dataModule.kill_w := false.B //io.in.bits.uop.robIdx.needFlush(io.redirectIn)
  dataModule.kill_r := kill_r
  // switch vs1 and vs2 if needed
  when(io.in.bits.uop.vfpCtrl.divReverse) {
    dataModule.io.in.bits.vs1 := io.in.bits.vs2
    dataModule.io.in.bits.vs2 := io.in.bits.vs1
  }
  // valid delay 1 cycle to match data
  io.out.valid := RegNext(dataModule.io.out.valid)
  // uop bypass datamodule
  val uopReg = RegEnable(io.in.bits.uop, dataModule.io.in.valid && io.in.ready)
  io.out.bits.uop := uopReg
}
