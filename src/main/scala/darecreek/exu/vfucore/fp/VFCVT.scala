package darecreek.exu.vfucore.fp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._


/*  VFP Conversion Module
 *   supported conversions
 *  32b float -> 64b float widen
 *  64b float -> 32b float narrow
 *
 *  32b float -> 64b (u)int  widen
 *  32b float -> 32b (u)int
 *  64b float -> 64b (u)int
 *  64b float -> 32b (u)int  narrow
 *
 *  64b (u)int -> 32b float  narrow
 *  32b (u)int  -> 32b float
 *  64b (u)int  -> 64b float
 *  32b (u)int  -> 64b float  widen
 */
class VFCVT(implicit val p: Parameters) extends VFPUSubModule {
  val cvt = Module(new VFCVTDataModule)

  cvt.io.in <> io.in
  cvt.io.redirect := io.redirect
  io.out <> cvt.io.out
  // block if is not cvt,
  cvt.io.in.valid := io.in.valid && io.in.bits.uop.vfpCtrl.isCvt
}

class VFCVTDataModule(implicit val p: Parameters) extends VFPUPipelineModule {

  override def latency = 2 //  2 stage register

  def zeroExt(in: UInt, len: Int): UInt = {
    val inLen = in.getWidth
    if (inLen > len) {
      in(len - 1, 0)
    } else {
      Cat(0.U((len - inLen).W), in)
    }
  }

  def isf2f(cmd: UInt) = cmd(0)

  def isf2i(cmd: UInt) = cmd(1)

  def isi2f(cmd: UInt) = cmd(2)

  // buffer input to reduce fan-out
  val src = S1Reg(io.in.bits.vs2)
  val uop = uopVec(1)
  val sew = uop.info.vsew
  val ctrl = uop.vfpCtrl
  val rm1 = uop.info.frm
  val isTypeSingle = uop.typeTag === VFPU.S
  val isRod = ctrl.cvtRm(0)
  val isRtz = ctrl.cvtRm(1)
  val eleActives = S1Reg(VecInit(Seq(0, 4).map(isActive)))
  val eleActives16_0 = S1Reg(VecInit(Seq(0, 2).map(isActive)))
  val eleActives16_1 = S1Reg(VecInit(Seq(4, 6).map(isActive)))

  // widening FP2FP
  // only need one, since widening insts has 2 output cycles
  val s2d = Module(new fudian.FPToFP(
    VFPU.f32.expWidth, VFPU.f32.precision,
    VFPU.f64.expWidth, VFPU.f64.precision
  )) // !! output is 64b
  s2d.io.in := Mux(uop.expdIdx(0), src.head(32), src.tail(32)) // first tail, then head
  s2d.io.rm := rm1
  val s2dOut = s2d.io.result // 64b
  val s2dFlagOut = s2d.io.fflags & Mux(uop.expdIdx(0), Fill(5, eleActives(1)), Fill(5, eleActives(0)))

  // narrowing FP2FP
  val d2s = Module(new fudian.FPToFP(
    VFPU.f64.expWidth, VFPU.f64.precision,
    VFPU.f32.expWidth, VFPU.f32.precision
  )) // !! output is 32b
  d2s.io.in := src
  // mandatory rod rounding
  d2s.io.rm := Mux(isRod, "b101".asUInt, rm1) // rounding towards odd
  val d2sNarrow32b = d2s.io.result // 32b
  val d2sNarrowFlag = Mux(Mux(uop.expdIdx(0), eleActives(1), eleActives(0)), d2s.io.fflags, empty_fflags)

  // FP2Int
  // s2i deals with fp32->int16, fp32->int32, fp32->int64(widen)
  val f322x16 = ctrl.cvtSigned && ctrl.cvtCmd(1) && uop.ctrl.narrow && (sew === 1.U)
  val f322x16u = !ctrl.cvtSigned && ctrl.cvtCmd(1) && uop.ctrl.narrow && (sew === 1.U)

  val s2iX1 = Module(new fudian.FPToInt(VFPU.f32.expWidth, VFPU.f32.precision)) // !!! output is 64b
  val s2iX2 = Module(new fudian.FPToInt(VFPU.f32.expWidth, VFPU.f32.precision)) // !!! output is 64b
  // d2i deals with fp64->int64, fp64->int32(narrow)
  val d2i = Module(new fudian.FPToInt(VFPU.f64.expWidth, VFPU.f64.precision)) // !!! output is 64b
  s2iX1.io.a := src.tail(32)
  s2iX2.io.a := src.head(32)
  d2i.io.a := src
  for (f2i <- Seq(s2iX1, s2iX2)) {
    f2i.io.rm := Mux(isRtz, "b001".asUInt, rm1)
    f2i.io.op := Cat(
      uop.ctrl.widen,
      ctrl.cvtSigned,
    )
  }
  d2i.io.rm := Mux(isRtz, "b001".asUInt, rm1)
  d2i.io.op := Cat(
    !uop.ctrl.narrow,
    ctrl.cvtSigned
  )

  val s2iX1_16 = Module(new fudian.FPToInt16(VFPU.f32.expWidth, VFPU.f32.precision)) // !!! output is 64b
  val s2iX2_16 = Module(new fudian.FPToInt16(VFPU.f32.expWidth, VFPU.f32.precision)) // !!! output is 64b
  s2iX1_16.io.a := src.tail(32)
  s2iX2_16.io.a := src.head(32)
  for (f2i_16 <- Seq(s2iX1_16, s2iX2_16)) {
    f2i_16.io.rm := Mux(isRtz, "b001".asUInt, rm1)
    f2i_16.io.op := Cat(
      uop.ctrl.widen,
      ctrl.cvtSigned,
    )
  }

  val s2iResult = Mux(
    uop.ctrl.widen,
    Mux(uop.expdIdx(0), s2iX2.io.result, s2iX1.io.result),
    Cat(s2iX2.io.result.tail(32), s2iX1.io.result.tail(32))
  )
  val f2iOut = Mux(isTypeSingle, s2iResult, d2i.io.result)
  val d2iNarrow32b = Mux(f322x16 || f322x16u, Cat(0.U(32.W), s2iX2_16.io.result(15, 0), s2iX1_16.io.result(15, 0)), d2i.io.result.tail(32))
  val s2ifflags = Seq(s2iX1, s2iX2).zipWithIndex.map(x => x._1.io.fflags & Fill(5, eleActives(x._2)))
  val s2ifflags16 = Seq(s2iX1_16, s2iX2_16).zipWithIndex.map(x => x._1.io.fflags & Mux(uop.expdIdx(0), Fill(5, eleActives16_1(x._2)), Fill(5, eleActives16_0(x._2))))
  val d2ifflags = Mux(f322x16 || f322x16u, s2ifflags16.reduce(_ | _), d2i.io.fflags & Mux(uop.expdIdx(0), Fill(5, eleActives(1)), Fill(5, eleActives(0))))
  val s2iFlagResult = Mux(
    uop.ctrl.widen,
    Mux(uop.expdIdx(0), s2ifflags(1), s2ifflags(0)),
    s2ifflags.reduce(_ | _)
  )
  val f2iFlagOut = Mux(isTypeSingle, s2iFlagResult, d2ifflags)
  val d2iNarrowFlag = d2ifflags // narrowing: d2i flag not or-ed, but outputted directly

  // Int2FP
  // i2s deals with int32->fp32 and int64->fp32(narrow)(i2sX1)
  // int16->fp32
  val i2sX1 = Module(new fudian.IntToFP(VFPU.f32.expWidth, VFPU.f32.precision)) //  !!! output is 32b, input16/32/64b
  val i2sX2 = Module(new fudian.IntToFP(VFPU.f32.expWidth, VFPU.f32.precision)) // !!! output is 32b, input16/32/64b
  // i2d deals with int64->fp64 and int32->fp64(widen), int32 input is sign-extended
  val i2d = Module(new fudian.IntToFP(VFPU.f64.expWidth, VFPU.f64.precision)) // !!! output is 64b

  val x162f = ctrl.cvtSigned && ctrl.cvtCmd(2) && uop.ctrl.widen && (sew === 1.U)
  val xu162f = !ctrl.cvtSigned && ctrl.cvtCmd(2) && uop.ctrl.widen && (sew === 1.U)
  val x162f_in = Wire(Vec(2, UInt(64.W)))
  x162f_in(0) := 0.U
  x162f_in(1) := 0.U
  when(uop.expdIdx(0)) {
    x162f_in(0) := Cat(Fill(48, Mux(x162f, src(47), 0.U(1.W))), src(47, 32))
    x162f_in(1) := Cat(Fill(48, Mux(x162f, src(63), 0.U(1.W))), src(63, 48))
  }.otherwise {
    x162f_in(0) := Cat(Fill(48, Mux(x162f, src(15), 0.U(1.W))), src(15, 0))
    x162f_in(1) := Cat(Fill(48, Mux(x162f, src(31), 0.U(1.W))), src(31, 16))
  }

  i2sX1.io.int := Mux(x162f || xu162f, x162f_in(0), Mux(uop.ctrl.narrow, src, src.tail(32))) // narrowing included, since IntToFP module extract tail32 in that case
  i2sX2.io.int := Mux(x162f || xu162f, x162f_in(1), zeroExt(src.head(32), 64))
  for (i2f <- Seq(i2sX1, i2sX2)) {
    i2f.io.sign := ctrl.cvtSigned
    i2f.io.long := uop.ctrl.narrow // input is int64
    i2f.io.rm := rm1
  }
  i2d.io.int := Mux(
    uop.ctrl.widen && uop.expdIdx(0), // widening cycle1 included
    zeroExt(src.head(32), 64),
    Mux(uop.ctrl.widen && !uop.expdIdx(0), zeroExt(src.tail(32), 64),
      src)
  )
  i2d.io.sign := ctrl.cvtSigned
  i2d.io.rm := rm1
  i2d.io.long := !uop.ctrl.widen
  val i2sResult = Cat(i2sX2.io.result, i2sX1.io.result)
  val i2sNarrow32b = i2sX1.io.result
  val i2fOut = Mux((isTypeSingle && !uop.ctrl.widen) || (x162f || xu162f), i2sResult, i2d.io.result)
  val i2sfflags = Seq(i2sX1, i2sX2).zipWithIndex.map(x => x._1.io.fflags & Fill(5, eleActives(x._2))) // X1tail, X2head
  val i2dfflags = i2d.io.fflags & Mux(uop.expdIdx(0), Fill(5, eleActives(1)), Fill(5, eleActives(0)))
  val i2sNarrowFlag = i2sX1.io.fflags & Mux(uop.expdIdx(0), Fill(5, eleActives(1)), Fill(5, eleActives(0)))
  val i2fFlagOut = Mux((isTypeSingle && !uop.ctrl.widen) || (x162f || xu162f), i2sfflags.reduce(_ | _), i2dfflags)

  // narrowing output handling
  val narrowBuf = Reg(Vec(2, UInt(32.W)))
  val narrowFlagBuf = Reg(UInt(5.W))
  // fp64->fp32
  // fp64->int32
  // int64->fp32
  val narrow32b = WireInit(UInt(32.W), d2sNarrow32b)
  val narrowFlag = WireInit(UInt(5.W), d2sNarrowFlag)
  when(isf2f(ctrl.cvtCmd)) {
    narrow32b := d2sNarrow32b
    narrowFlag := d2sNarrowFlag
  }.elsewhen(isf2i(ctrl.cvtCmd)) {
    narrow32b := d2iNarrow32b
    narrowFlag := d2iNarrowFlag
  }.elsewhen(isi2f(ctrl.cvtCmd)) {
    narrow32b := i2sNarrow32b
    narrowFlag := i2sNarrowFlag
  }
  when(regEnable(2) && uop.ctrl.narrow) {
    narrowFlagBuf := narrowFlag
    when(uop.expdIdx(0)) {
      narrowBuf(1) := narrow32b
    }.otherwise {
      narrowBuf(0) := narrow32b
    }
  }
  val narrowOutReg = Cat(narrowBuf(1), narrowBuf(0))
  val narrowFlagOutReg = narrowFlagBuf

  // final output mux
  val nonNarrowOutReg = S2Reg(Mux(
    isf2f(ctrl.cvtCmd),
    s2dOut,
    Mux(isi2f(ctrl.cvtCmd), i2fOut, f2iOut)
  ))
  val nonNarrowFlagOutReg = S2Reg(Mux(
    isf2f(ctrl.cvtCmd),
    s2dFlagOut,
    Mux(isi2f(ctrl.cvtCmd), i2fFlagOut, f2iFlagOut)
  ))
  val uopReg = uopVec(2)
  io.out.bits.vd := Mux(uopReg.ctrl.narrow, narrowOutReg, nonNarrowOutReg)
  io.out.bits.fflags := Mux(uopReg.ctrl.narrow, narrowFlagOutReg, nonNarrowFlagOutReg)
  io.out.valid := validVec.last
  io.out.bits.uop := uopVec.last
}
