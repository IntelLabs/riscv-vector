package darecreek.exu.vfucore.fp

import chisel3._
import chisel3.util._
import darecreek.exu.vfucore.fp.fudian._
import darecreek.exu.vfucore.fp.fudian.utils.CLZ
import freechips.rocketchip.config._

class VFRec(implicit val p: Parameters) extends VFPUSubModule {
  val rec = Module(new VFRecDataModule)

  rec.io.in <> io.in
  rec.io.redirect := io.redirect
  io.out <> rec.io.out
  // block if is not rec,
  val vfpCtrl = io.in.bits.uop.vfpCtrl
  rec.io.in.valid := io.in.valid && (vfpCtrl.isRec7 || vfpCtrl.isRecSqrt7)
}

class OutTypeClassify(val t: VFPU.FType) extends Module {

  val fp = IO(Input(new FloatPoint(t.expWidth, t.precision)))
  val rm = IO(Input(UInt(3.W)))
  val isSubnormal = IO(Output(Bool()))
  val rsqrtType = IO(Output(Vec(7, Bool()))) // one-hot
  val recType = IO(Output(Vec(11, Bool()))) // one-hot

  val decoded = fp.decode
  val sign = fp.sign
  isSubnormal := decoded.isSubnormal

  rsqrtType := VecInit(
    sign && ((!decoded.expIsOnes && !decoded.isZero) || (decoded.isInf)),// [-inf, -0), output:canonicalNaN, NV
    decoded.isZero && sign, // -0, output:-Inf, DZ
    decoded.isZero && !sign, // +0, output:+Inf, DZ
    !sign && (!decoded.expIsOnes && !decoded.isZero),// (+0, +inf), output:estimate
    decoded.isInf && !sign, // +inf, output:+0
    decoded.isQNaN,// qNaN, output:NaN
    decoded.isSNaN// sNaN, output:NaN, NV
  )
  val recAbnormalOutputType = VecInit(
    decoded.isInf, // inf, out:0
    fp.exp.head(t.expWidth-2).andR && fp.exp(1) && !fp.exp(0),    // output subnormal, shift 2
    fp.exp.head(t.expWidth-2).andR && !fp.exp(1) && fp.exp(0),    // output subnormal, shift 1
    sign && decoded.isSubnormal && fp.sig.head(2) === 0.U && (rm === RTZ || rm === RUP),   // RUP, RTZ, out:greatest mag, NX, OF
    sign && decoded.isSubnormal && fp.sig.head(2) === 0.U && (rm === RDN || rm === RNE || rm === RMM),   // RDN, RNE, RMM, out:-Inf, NX, OF
    !sign && decoded.isSubnormal && fp.sig.head(2) === 0.U && (rm === RTZ || rm === RDN),  // out:greatest mag, NX, OF
    !sign && decoded.isSubnormal && fp.sig.head(2) === 0.U && (rm === RUP || rm === RNE || rm === RMM),   // RUP, RNE, RMM, out:+Inf, NX, OF
    decoded.isZero, // zero, out:Inf, DZ
    decoded.isQNaN, // qNAN
    decoded.isSNaN // sNAN
  )
  recType := Mux(recAbnormalOutputType.reduce(_|_), VecInit(false.B +: recAbnormalOutputType), VecInit(true.B +: Seq.fill(10)(false.B)))
}
object OutTypeClassify {
  def apply(t: VFPU.FType)(f: FloatPoint, rm: UInt) = {
    val m = Module(new OutTypeClassify(t))
    m.fp := f
    m.rm := rm
    m
  }
}

class VFRecDataModule(implicit val p: Parameters) extends VFPUPipelineModule {
  def getElements(t: VFPU.FType, in: UInt) = {
    val repCnt = laneWidth / t.len
    val rawIn = Seq.range(0, repCnt).map(i => in((i + 1) * t.len-1, i * t.len))
    val fpElements = rawIn.map(x => FloatPoint.fromUInt(x, t.expWidth, t.precision))
    fpElements
  }

  override def latency = 1
  val typeSel = Seq(VFPU.S, VFPU.D).map(x => x === io.in.bits.uop.typeTag)

  val elmtsFP32 = getElements(VFPU.f32, io.in.bits.vs2)
  val TypesFP32 = elmtsFP32.map(e => OutTypeClassify(VFPU.f32)(e, io.in.bits.uop.info.frm))
  val leadingZeroCntFP32 = elmtsFP32.map(f => CLZ(f.sig))
  val normExpInFP32 = Wire(Vec(elmtsFP32.length, SInt((VFPU.f32.expWidth + 1).W))) // wider than expWidth for the subnormal situation
  val normSigInFP32 = Wire(Vec(elmtsFP32.length, UInt(VFPU.f32.sigWidth.W)))
  for (i <- elmtsFP32.indices) {
    normExpInFP32(i) := Mux(TypesFP32(i).isSubnormal, -leadingZeroCntFP32(i).zext, elmtsFP32(i).exp.zext)
    normSigInFP32(i) := Mux(TypesFP32(i).isSubnormal,
      Cat(elmtsFP32(i).sig.tail(1), 0.U(1.W)) << leadingZeroCntFP32(i), // << 1+leadingZero
      elmtsFP32(i).sig
    )
  }

  val elmtsFP64 = getElements(VFPU.f64, io.in.bits.vs2)
  val TypesFP64 = elmtsFP64.map(e => OutTypeClassify(VFPU.f64)(e, io.in.bits.uop.info.frm))
  val leadingZeroCntFP64 = elmtsFP64.map(f => CLZ(f.sig))
  val normExpInFP64 = Wire(Vec(elmtsFP64.length, SInt((VFPU.f64.expWidth + 1).W)))
  val normSigInFP64 = Wire(Vec(elmtsFP64.length, UInt(VFPU.f64.sigWidth.W)))
  for (i <- elmtsFP64.indices) {
    normExpInFP64(i) := Mux(TypesFP64(i).isSubnormal, -leadingZeroCntFP64(i).zext, elmtsFP64(i).exp.zext)
    normSigInFP64(i) := Mux(TypesFP64(i).isSubnormal,
      Cat(elmtsFP64(i).sig.tail(1), 0.U(1.W)) << leadingZeroCntFP64(i), // << 1+leadingZero
      elmtsFP64(i).sig
    )
  }

  val eleActives = VecInit(Seq(0,4).map(isActive))
  // vfrsqrt7
  // reuse Lookup table
  val rsqrtSigLookup0 = RsqrtLookup(
    Mux1H(typeSel,Seq(normExpInFP32(0)(0).asBool, normExpInFP64(0)(0).asBool)),
    Mux1H(typeSel,Seq(normSigInFP32(0).head(6), normSigInFP64(0).head(6)))
  )
  val rsqrtSigLookup1 = RsqrtLookup(normExpInFP32(1)(0).asBool, normSigInFP32(1).head(6))
  val rsqrtSigOutFP32 = Seq(rsqrtSigLookup0, rsqrtSigLookup1)
  val rsqrtSigOutFP64 = Seq(rsqrtSigLookup0)
  val rsqrtSConstFP32 = (3 * FloatPoint.expBias(VFPU.f32.expWidth) - 1).asSInt
  val rsqrtSConstFP64 = (3 * FloatPoint.expBias(VFPU.f64.expWidth) - 1).asSInt
  val rsqrtExpOutFP32 = normExpInFP32.map(x => (rsqrtSConstFP32 - x).asUInt(VFPU.f32.expWidth,1))  // >> 1
  require(rsqrtExpOutFP32(0).getWidth == VFPU.f32.expWidth)
  val rsqrtExpOutFP64 = normExpInFP64.map(x => (rsqrtSConstFP64 - x).asUInt(VFPU.f64.expWidth,1))  // >> 1
  require(rsqrtExpOutFP64(0).getWidth == VFPU.f64.expWidth)
  val (rsqrtResultFP32Seq, rsqrtFlagsFP32Seq) = Seq.range(0, elmtsFP32.length).map( i =>
    rsqrtOutGen(elmtsFP32(i), eleActives(i), rsqrtExpOutFP32(i), rsqrtSigOutFP32(i), TypesFP32(i).rsqrtType)
  ).unzip
  val (rsqrtResultFP32, rsqrtFlagsFP32) = outCompose(rsqrtResultFP32Seq, rsqrtFlagsFP32Seq)
  val (rsqrtResultFP64Seq, rsqrtFlagsFP64Seq) = Seq.range(0, elmtsFP64.length).map( i =>
    rsqrtOutGen(elmtsFP64(i), eleActives(i), rsqrtExpOutFP64(i), rsqrtSigOutFP64(i), TypesFP64(i).rsqrtType)
  ).unzip
  val (rsqrtResultFP64, rsqrtFlagsFP64) = outCompose(rsqrtResultFP64Seq, rsqrtFlagsFP64Seq)
  val rsqrtResult = Mux1H(typeSel, Seq(rsqrtResultFP32, rsqrtResultFP64))
  val rsqrtFlags = Mux1H(typeSel, Seq(rsqrtFlagsFP32, rsqrtFlagsFP64))

  // rec7
  // reuse Lookup table
  val recSigLookup0 = RecLookup(Mux1H(typeSel,Seq(normSigInFP32(0).head(7), normSigInFP64(0).head(7))))
  val recSigLookup1 = RecLookup(normSigInFP32(1).head(7))
  val recSigOutFP32 = Seq(recSigLookup0, recSigLookup1)
  val recSigOutFP64 = Seq(recSigLookup0)
  val recSConstFP32 = (2 * FloatPoint.expBias(VFPU.f32.expWidth) - 1).asSInt
  val recSConstFP64 = (2 * FloatPoint.expBias(VFPU.f64.expWidth) - 1).asSInt
  val recExpOutFP32 = normExpInFP32.map(x => (recSConstFP32 - x).tail(1))
  require(recExpOutFP32(0).getWidth == VFPU.f32.expWidth)
  val recExpOutFP64 = normExpInFP64.map(x => (recSConstFP64 - x).tail(1))
  require(recExpOutFP64(0).getWidth == VFPU.f64.expWidth)
  val (recResultFP32Seq, recFlagsFP32Seq) = Seq.range(0, elmtsFP32.length).map( i =>
    recOutGen(elmtsFP32(i), eleActives(i), recExpOutFP32(i), recSigOutFP32(i), TypesFP32(i).recType)
  ).unzip
  val (recResultFP32, recFlagsFP32) = outCompose(recResultFP32Seq, recFlagsFP32Seq)
  val (recResultFP64Seq, recFlagsFP64Seq) = Seq.range(0, elmtsFP64.length).map( i =>
    recOutGen(elmtsFP64(i), eleActives(i), recExpOutFP64(i), recSigOutFP64(i), TypesFP64(i).recType)
  ).unzip
  val (recResultFP64, recFlagsFP64) = outCompose(recResultFP64Seq, recFlagsFP64Seq)
  val recResult = Mux1H(typeSel, Seq(recResultFP32, recResultFP64))
  val recFlags = Mux1H(typeSel, Seq(recFlagsFP32, recFlagsFP64))

  // output mux
  val vfpCtrl = io.in.bits.uop.vfpCtrl
  io.out.bits.vd := S1Reg(Mux(vfpCtrl.isRec7, recResult, rsqrtResult))
  io.out.bits.fflags := S1Reg(Mux(vfpCtrl.isRec7, recFlags, rsqrtFlags))
  // delay 1 cycle, to sync with the output result
  io.out.valid := validVec.last
  io.out.bits.uop := uopVec.last

  def outCompose(results: Seq[UInt], flags: Seq[UInt]) = {
    val result = Cat(results.reverse)
    val fflags = flags.reduce(_|_)
    (result, fflags)
  }
  def rsqrtOutGen(f: FloatPoint, isActive: Bool, expOut: UInt, sigOut: UInt, outType: Vec[Bool]) = {
    require(expOut.getWidth == f.expWidth && sigOut.getWidth == 7)
    val result = Mux1H(outType, Seq(
      FloatPoint.defaultNaNUInt(f.expWidth, f.precision),
      FloatPoint.infUInt(f.expWidth, f.precision, isNeg = true.B),
      FloatPoint.infUInt(f.expWidth, f.precision, isNeg = false.B),
      Cat(f.sign, expOut, sigOut, 0.U((f.sigWidth - 7).W)),
      FloatPoint.zeroUInt(f.expWidth, f.precision, isNeg = false.B),
      FloatPoint.defaultNaNUInt(f.expWidth, f.precision),
      FloatPoint.defaultNaNUInt(f.expWidth, f.precision))
    )
    require(result.getWidth == (f.expWidth + f.precision))
    val fflags = Mux1H(outType, Seq(
      "b10000".U(5.W), // NV
      "b01000".U(5.W), // DZ
      "b01000".U(5.W),
      "b0".U(5.W),
      "b0".U(5.W),
      "b0".U(5.W),
      "b10000".U(5.W),
    )) & Fill(5, isActive)
    (result, fflags)
  }
  def recOutGen(f: FloatPoint, isActive: Bool, expOut: UInt, sigOut: UInt, outType: Seq[Bool]) = {
    val result = Mux1H(outType, Seq(
      Cat(f.sign, expOut, sigOut, 0.U((f.sigWidth - 7).W)),// rec7 output normal
      FloatPoint.zeroUInt(f.expWidth, f.precision, isNeg = f.sign), // +-0
      Cat(f.sign, 0.U(f.expWidth.W), 1.U(2.W), sigOut, 0.U((f.sigWidth - 9).W)),// subnormal, sig shift 2
      Cat(f.sign, 0.U(f.expWidth.W), 1.U(1.W), sigOut, 0.U((f.sigWidth - 8).W)),// subnormal, sig shift 1
      FloatPoint.greatestFinite(f.expWidth, f.precision, isNeg = true.B),// -greatest mag, NX, OF
      FloatPoint.infUInt(f.expWidth, f.precision, isNeg = true.B),// -Inf,
      FloatPoint.greatestFinite(f.expWidth, f.precision, isNeg = false.B),// +greatest mag, NX, OF
      FloatPoint.infUInt(f.expWidth, f.precision, isNeg = false.B),// +inf, NX, OF
      FloatPoint.infUInt(f.expWidth, f.precision, isNeg = f.sign), // +-Inf, DZ
      FloatPoint.defaultNaNUInt(f.expWidth, f.precision),// NAN,
      FloatPoint.defaultNaNUInt(f.expWidth, f.precision))// NAN, NV (sNAN)
    )
    val fflags = Mux1H(outType, Seq(
      "b0".U,// rec7 output normal
      "b0".U,// 0
      "b0".U,// subnormal, shift 2
      "b0".U,// subnormal, shift 1
      "b00101".U,// -greatest mag, NX, OF
      "b00101".U,// -Inf, NX, OF
      "b00101".U,// +greatest mag, NX, OF
      "b00101".U,// +inf, NX, OF
      "b01000".U,// Inf, DZ
      "b0".U,// NAN,
      "b10000".U,// NAN, NV (sNAN)) & Fill(5, isActive)
    )) & Fill(5, isActive)
    (result, fflags)
  }
}



object RsqrtLookup {
  def apply(exp0: Bool, sig:UInt) = {
    val even = MuxLookup(sig, 0.U, table(0))
    val odd = MuxLookup(sig, 0.U, table(1))
    Mux(exp0, odd, even)
  }
  val table = Seq(
    // exp[0] = 0
    Seq(
      0.U(6.W) -> 52.U(7.W),
      1.U(6.W) -> 51.U(7.W),
      2.U(6.W) -> 50.U(7.W),
      3.U(6.W) -> 48.U(7.W),
      4.U(6.W) -> 47.U(7.W),
      5.U(6.W) -> 46.U(7.W),
      6.U(6.W) -> 44.U(7.W),
      7.U(6.W) -> 43.U(7.W),
      8.U(6.W) -> 42.U(7.W),
      9.U(6.W) -> 41.U(7.W),
      10.U(6.W) -> 40.U(7.W),
      11.U(6.W) -> 39.U(7.W),
      12.U(6.W) -> 38.U(7.W),
      13.U(6.W) -> 36.U(7.W),
      14.U(6.W) -> 35.U(7.W),
      15.U(6.W) -> 34.U(7.W),
      16.U(6.W) -> 33.U(7.W),
      17.U(6.W) -> 32.U(7.W),
      18.U(6.W) -> 31.U(7.W),
      19.U(6.W) -> 30.U(7.W),
      20.U(6.W) -> 30.U(7.W),
      21.U(6.W) -> 29.U(7.W),
      22.U(6.W) -> 28.U(7.W),
      23.U(6.W) -> 27.U(7.W),
      24.U(6.W) -> 26.U(7.W),
      25.U(6.W) -> 25.U(7.W),
      26.U(6.W) -> 24.U(7.W),
      27.U(6.W) -> 23.U(7.W),
      28.U(6.W) -> 23.U(7.W),
      29.U(6.W) -> 22.U(7.W),
      30.U(6.W) -> 21.U(7.W),
      31.U(6.W) -> 20.U(7.W),
      32.U(6.W) -> 19.U(7.W),
      33.U(6.W) -> 19.U(7.W),
      34.U(6.W) -> 18.U(7.W),
      35.U(6.W) -> 17.U(7.W),
      36.U(6.W) -> 16.U(7.W),
      37.U(6.W) -> 16.U(7.W),
      38.U(6.W) -> 15.U(7.W),
      39.U(6.W) -> 14.U(7.W),
      40.U(6.W) -> 14.U(7.W),
      41.U(6.W) -> 13.U(7.W),
      42.U(6.W) -> 12.U(7.W),
      43.U(6.W) -> 12.U(7.W),
      44.U(6.W) -> 11.U(7.W),
      45.U(6.W) -> 10.U(7.W),
      46.U(6.W) -> 10.U(7.W),
      47.U(6.W) -> 9.U(7.W),
      48.U(6.W) -> 9.U(7.W),
      49.U(6.W) -> 8.U(7.W),
      50.U(6.W) -> 7.U(7.W),
      51.U(6.W) -> 7.U(7.W),
      52.U(6.W) -> 6.U(7.W),
      53.U(6.W) -> 6.U(7.W),
      54.U(6.W) -> 5.U(7.W),
      55.U(6.W) -> 4.U(7.W),
      56.U(6.W) -> 4.U(7.W),
      57.U(6.W) -> 3.U(7.W),
      58.U(6.W) -> 3.U(7.W),
      59.U(6.W) -> 2.U(7.W),
      60.U(6.W) -> 2.U(7.W),
      61.U(6.W) -> 1.U(7.W),
      62.U(6.W) -> 1.U(7.W),
      63.U(6.W) -> 0.U(7.W),
    ),
    // exp[0] = 1
    Seq(
      0.U(6.W) -> 127.U(7.W),
      1.U(6.W) -> 125.U(7.W),
      2.U(6.W) -> 123.U(7.W),
      3.U(6.W) -> 121.U(7.W),
      4.U(6.W) -> 119.U(7.W),
      5.U(6.W) -> 118.U(7.W),
      6.U(6.W) -> 116.U(7.W),
      7.U(6.W) -> 114.U(7.W),
      8.U(6.W) -> 113.U(7.W),
      9.U(6.W) -> 111.U(7.W),
      10.U(6.W) -> 109.U(7.W),
      11.U(6.W) -> 108.U(7.W),
      12.U(6.W) -> 106.U(7.W),
      13.U(6.W) -> 105.U(7.W),
      14.U(6.W) -> 103.U(7.W),
      15.U(6.W) -> 102.U(7.W),
      16.U(6.W) -> 100.U(7.W),
      17.U(6.W) -> 99.U(7.W),
      18.U(6.W) -> 97.U(7.W),
      19.U(6.W) -> 96.U(7.W),
      20.U(6.W) -> 95.U(7.W),
      21.U(6.W) -> 93.U(7.W),
      22.U(6.W) -> 92.U(7.W),
      23.U(6.W) -> 91.U(7.W),
      24.U(6.W) -> 90.U(7.W),
      25.U(6.W) -> 88.U(7.W),
      26.U(6.W) -> 87.U(7.W),
      27.U(6.W) -> 86.U(7.W),
      28.U(6.W) -> 85.U(7.W),
      29.U(6.W) -> 84.U(7.W),
      30.U(6.W) -> 83.U(7.W),
      31.U(6.W) -> 82.U(7.W),
      32.U(6.W) -> 80.U(7.W),
      33.U(6.W) -> 79.U(7.W),
      34.U(6.W) -> 78.U(7.W),
      35.U(6.W) -> 77.U(7.W),
      36.U(6.W) -> 76.U(7.W),
      37.U(6.W) -> 75.U(7.W),
      38.U(6.W) -> 74.U(7.W),
      39.U(6.W) -> 73.U(7.W),
      40.U(6.W) -> 72.U(7.W),
      41.U(6.W) -> 71.U(7.W),
      42.U(6.W) -> 70.U(7.W),
      43.U(6.W) -> 70.U(7.W),
      44.U(6.W) -> 69.U(7.W),
      45.U(6.W) -> 68.U(7.W),
      46.U(6.W) -> 67.U(7.W),
      47.U(6.W) -> 66.U(7.W),
      48.U(6.W) -> 65.U(7.W),
      49.U(6.W) -> 64.U(7.W),
      50.U(6.W) -> 63.U(7.W),
      51.U(6.W) -> 63.U(7.W),
      52.U(6.W) -> 62.U(7.W),
      53.U(6.W) -> 61.U(7.W),
      54.U(6.W) -> 60.U(7.W),
      55.U(6.W) -> 59.U(7.W),
      56.U(6.W) -> 59.U(7.W),
      57.U(6.W) -> 58.U(7.W),
      58.U(6.W) -> 57.U(7.W),
      59.U(6.W) -> 56.U(7.W),
      60.U(6.W) -> 56.U(7.W),
      61.U(6.W) -> 55.U(7.W),
      62.U(6.W) -> 54.U(7.W),
      63.U(6.W) -> 53.U(7.W),
    )
  )
}

object RecLookup {
  def apply(sig:UInt) = {
    require(sig.getWidth == 7)
    MuxLookup(sig, 0.U, table)
  }
  val table = Seq(
    0.U(7.W) -> 127.U(7.W),
    1.U(7.W) -> 125.U(7.W),
    2.U(7.W) -> 123.U(7.W),
    3.U(7.W) -> 121.U(7.W),
    4.U(7.W) -> 119.U(7.W),
    5.U(7.W) -> 117.U(7.W),
    6.U(7.W) -> 116.U(7.W),
    7.U(7.W) -> 114.U(7.W),
    8.U(7.W) -> 112.U(7.W),
    9.U(7.W) -> 110.U(7.W),
    10.U(7.W) -> 109.U(7.W),
    11.U(7.W) -> 107.U(7.W),
    12.U(7.W) -> 105.U(7.W),
    13.U(7.W) -> 104.U(7.W),
    14.U(7.W) -> 102.U(7.W),
    15.U(7.W) -> 100.U(7.W),
    16.U(7.W) -> 99.U(7.W),
    17.U(7.W) -> 97.U(7.W),
    18.U(7.W) -> 96.U(7.W),
    19.U(7.W) -> 94.U(7.W),
    20.U(7.W) -> 93.U(7.W),
    21.U(7.W) -> 91.U(7.W),
    22.U(7.W) -> 90.U(7.W),
    23.U(7.W) -> 88.U(7.W),
    24.U(7.W) -> 87.U(7.W),
    25.U(7.W) -> 85.U(7.W),
    26.U(7.W) -> 84.U(7.W),
    27.U(7.W) -> 83.U(7.W),
    28.U(7.W) -> 81.U(7.W),
    29.U(7.W) -> 80.U(7.W),
    30.U(7.W) -> 79.U(7.W),
    31.U(7.W) -> 77.U(7.W),
    32.U(7.W) -> 76.U(7.W),
    33.U(7.W) -> 75.U(7.W),
    34.U(7.W) -> 74.U(7.W),
    35.U(7.W) -> 72.U(7.W),
    36.U(7.W) -> 71.U(7.W),
    37.U(7.W) -> 70.U(7.W),
    38.U(7.W) -> 69.U(7.W),
    39.U(7.W) -> 68.U(7.W),
    40.U(7.W) -> 66.U(7.W),
    41.U(7.W) -> 65.U(7.W),
    42.U(7.W) -> 64.U(7.W),
    43.U(7.W) -> 63.U(7.W),
    44.U(7.W) -> 62.U(7.W),
    45.U(7.W) -> 61.U(7.W),
    46.U(7.W) -> 60.U(7.W),
    47.U(7.W) -> 59.U(7.W),
    48.U(7.W) -> 58.U(7.W),
    49.U(7.W) -> 57.U(7.W),
    50.U(7.W) -> 56.U(7.W),
    51.U(7.W) -> 55.U(7.W),
    52.U(7.W) -> 54.U(7.W),
    53.U(7.W) -> 53.U(7.W),
    54.U(7.W) -> 52.U(7.W),
    55.U(7.W) -> 51.U(7.W),
    56.U(7.W) -> 50.U(7.W),
    57.U(7.W) -> 49.U(7.W),
    58.U(7.W) -> 48.U(7.W),
    59.U(7.W) -> 47.U(7.W),
    60.U(7.W) -> 46.U(7.W),
    61.U(7.W) -> 45.U(7.W),
    62.U(7.W) -> 44.U(7.W),
    63.U(7.W) -> 43.U(7.W),
    64.U(7.W) -> 42.U(7.W),
    65.U(7.W) -> 41.U(7.W),
    66.U(7.W) -> 40.U(7.W),
    67.U(7.W) -> 40.U(7.W),
    68.U(7.W) -> 39.U(7.W),
    69.U(7.W) -> 38.U(7.W),
    70.U(7.W) -> 37.U(7.W),
    71.U(7.W) -> 36.U(7.W),
    72.U(7.W) -> 35.U(7.W),
    73.U(7.W) -> 35.U(7.W),
    74.U(7.W) -> 34.U(7.W),
    75.U(7.W) -> 33.U(7.W),
    76.U(7.W) -> 32.U(7.W),
    77.U(7.W) -> 31.U(7.W),
    78.U(7.W) -> 31.U(7.W),
    79.U(7.W) -> 30.U(7.W),
    80.U(7.W) -> 29.U(7.W),
    81.U(7.W) -> 28.U(7.W),
    82.U(7.W) -> 28.U(7.W),
    83.U(7.W) -> 27.U(7.W),
    84.U(7.W) -> 26.U(7.W),
    85.U(7.W) -> 25.U(7.W),
    86.U(7.W) -> 25.U(7.W),
    87.U(7.W) -> 24.U(7.W),
    88.U(7.W) -> 23.U(7.W),
    89.U(7.W) -> 23.U(7.W),
    90.U(7.W) -> 22.U(7.W),
    91.U(7.W) -> 21.U(7.W),
    92.U(7.W) -> 21.U(7.W),
    93.U(7.W) -> 20.U(7.W),
    94.U(7.W) -> 19.U(7.W),
    95.U(7.W) -> 19.U(7.W),
    96.U(7.W) -> 18.U(7.W),
    97.U(7.W) -> 17.U(7.W),
    98.U(7.W) -> 17.U(7.W),
    99.U(7.W) -> 16.U(7.W),
    100.U(7.W) -> 15.U(7.W),
    101.U(7.W) -> 15.U(7.W),
    102.U(7.W) -> 14.U(7.W),
    103.U(7.W) -> 14.U(7.W),
    104.U(7.W) -> 13.U(7.W),
    105.U(7.W) -> 12.U(7.W),
    106.U(7.W) -> 12.U(7.W),
    107.U(7.W) -> 11.U(7.W),
    108.U(7.W) -> 11.U(7.W),
    109.U(7.W) -> 10.U(7.W),
    110.U(7.W) -> 9.U(7.W),
    111.U(7.W) -> 9.U(7.W),
    112.U(7.W) -> 8.U(7.W),
    113.U(7.W) -> 8.U(7.W),
    114.U(7.W) -> 7.U(7.W),
    115.U(7.W) -> 7.U(7.W),
    116.U(7.W) -> 6.U(7.W),
    117.U(7.W) -> 5.U(7.W),
    118.U(7.W) -> 5.U(7.W),
    119.U(7.W) -> 4.U(7.W),
    120.U(7.W) -> 4.U(7.W),
    121.U(7.W) -> 3.U(7.W),
    122.U(7.W) -> 3.U(7.W),
    123.U(7.W) -> 2.U(7.W),
    124.U(7.W) -> 2.U(7.W),
    125.U(7.W) -> 1.U(7.W),
    126.U(7.W) -> 1.U(7.W),
    127.U(7.W) -> 0.U(7.W),
  )
}
