// See README.md for license details.
package matrix

import Chisel.Pipe
import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.FType
import freechips.rocketchip.config.Parameters
import hardfloat._
import freechips.rocketchip.util.ShouldBeRetimed
import darecreek.exu.vfucore.{VFuModule, VFuParameters, VFuParamsKey}
// import exu.matrix._

// import freechips.rocketchip.util.{LCG, UIntIsOneOf}
// import boom.util._
// import boom.common._
// import boom.common.MicroOpcodes._
// import scala.collection.mutable.ArrayBuffer

// acc buffer related command
object AccTileConstants {
  // access cmd
  val SLICE_READ = 0.U(1.W)
  val SLICE_WRITE = 1.U(1.W)
  // register type
  val INT8TYPE = 0.U(3.W) // cat(fp_val, sew)
  val INT16TYPE = 1.U(3.W)
  val INT32TYPE = 2.U(3.W)
  val INT64TYPE = 3.U(3.W)
  val FP16TYPE = 5.U(3.W)
  val FP32TYPE = 6.U(3.W)
  val FP64TYPE = 7.U(3.W)
  // slice groups
  val W0 = 0.U(2.W)
  val W1 = 1.U(2.W)
  val Q0 = 0.U(2.W)
  val Q1 = 1.U(2.W)
  val Q2 = 2.U(2.W)
  val Q3 = 3.U(2.W)
  // alu types
  val MACC = 0.U(4.W)
  val MULT = 1.U(4.W)
  val ADD = 2.U(4.W)
  val SUB = 3.U(4.W)
  val CVT = 4.U(4.W)
  val VECMACC = 5.U(4.W)
  val ONE_OP = 8.U(4.W)

  val numMatTrRegisters = 12
  val numMatAccRegisters = 8
  val maxNumMatTiles = 1
  val usingInnerProd = false
  val rLenbSz = 4
  val numAccTiles = 8
  val tpregSz = 5
  val vstqAddrSz = 3
  val rLen = 128
  val numReadPorts = 1
  val numVLdPorts = 1
  val mxuTileRows = 8
  val mxuTileCols = 8
  val mxuMeshRows = 1
  val mxuMeshCols = 1
  val mxuPERows = mxuMeshRows * mxuTileRows
  val mxuPECols = mxuMeshCols * mxuTileCols
  val rLenb = 16

}

import AccTileConstants._

// class MacCtrls(implicit p: Parameters) extends BoomBundle {
class MacCtrls(implicit p: Parameters) extends Bundle {
  val src1Ridx = UInt(log2Ceil(maxNumMatTiles).W)
  val src2Ridx = UInt(log2Ceil(maxNumMatTiles).W)
  val dstRidx = UInt(log2Ceil(maxNumMatTiles).W)
  val srcType = UInt(3.W)
  val outType = UInt(3.W)
  val aluType = UInt(4.W)
  val macInit = if (!usingInnerProd) Bool() else null
  val macLast = if (!usingInnerProd) Bool() else null
  val autoClr = if (!usingInnerProd) Bool() else null
  val autoCvt = if (!usingInnerProd) Bool() else null
  val prodLen = if (usingInnerProd) UInt((rLenbSz + 1).W) else null
  val dirCal = UInt(2.W)
  val rm = UInt(3.W) // rounding mode
}


// class ClrCtrls(implicit p: Parameters) extends BoomBundle {
class ClrCtrls(implicit p: Parameters) extends Bundle {
  val ridx = UInt(log2Ceil(numAccTiles).W) // register index
}

// class TileReadReq(implicit p: Parameters) extends BoomBundle {
class TileReadReq(implicit p: Parameters) extends Bundle {
  val ridx = UInt(tpregSz.W) // register index
  val sidx = UInt(rLenbSz.W) // slice index
  val sew = UInt(2.W) // SEW = 8bits, 16bits, 32bits
  val tt = UInt(2.W) // 0, acc_row; 1, acc_col; 2, tile_row; 3, tile_col
  val quad = UInt(2.W) // acc may support quad-width
  val vstq_idx = UInt(vstqAddrSz.W) // index in vstq
  val xcol = if (usingInnerProd) Bool() else null
}

// class SliceCtrls(implicit p: Parameters) extends BoomBundle {
class SliceCtrls(implicit p: Parameters) extends Bundle {
  val ridx = UInt(log2Ceil(numAccTiles).W) // register index
  val sidx = UInt(rLenbSz.W) // slice index
  val sew = UInt(2.W) // SEW = 8bits, 16bits, 32bits
}

// class AccReadReq(implicit p: Parameters) extends BoomBundle {
class AccReadReq(implicit p: Parameters) extends Bundle {
  val sCtrls = new SliceCtrls() // slice control signals
  val tt = UInt(1.W) // 0, row slice; 1, col slice
  val quad = UInt(2.W) // acc may support quad-width
  val vstq_idx = UInt(vstqAddrSz.W) // index in vstq
}

// class AccReadResp(implicit p: Parameters) extends BoomBundle {
class AccReadResp(implicit p: Parameters) extends Bundle {
  val vstq_idx = UInt(vstqAddrSz.W) // index in vstq
  val data = UInt(rLen.W)
}

// class FpMacPipe(implicit p: Parameters) extends BoomBundle {
class FpMacPipe(implicit p: Parameters) extends Bundle {
  val outType = UInt(3.W)
  val aluType = UInt(4.W)
  val src1val = UInt(33.W)
  val src1idx = UInt(log2Ceil(numAccTiles).W)
  val src2idx = UInt(log2Ceil(numAccTiles).W)
  val dstRidx = UInt(log2Ceil(numAccTiles).W)
  val macInit = Bool()
  val macLast = Bool()
  val autoClr = Bool()
  val autoCvt = Bool()
  val roundingMode = UInt(3.W)
  val detectTininess = UInt(1.W)
}

// acc 32-bits
class IntMacUnit extends Module {
  val io = IO(new Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val src3 = Input(UInt(32.W))
    val srcType = Input(UInt(3.W))
    val outType = Input(UInt(3.W))
    val aluType = Input(UInt(4.W))
    val out = Output(UInt(32.W))
  })

  val sMax = WireInit(0.U(32.W))
  val sMin = WireInit(0.U(32.W))
  sMax := Mux(io.outType === INT8TYPE, 0x7F.U,
    Mux(io.outType === INT16TYPE, 0x7FFF.U, 0x7FFFFFFF.U))
  sMin := Mux(io.outType === INT8TYPE, Fill(24, 1.U(1.W)) ## 0x80.U(8.W),
    Mux(io.outType === INT16TYPE, Fill(16, 1.U(1.W)) ## 0x8000.U(16.W),
      1.U(1.W) ## Fill(31, 0.U(1.W))))

  // macc, mult
  val lhs0 = io.src1(7, 0).asSInt
  val rhs0 = io.src2(7, 0).asSInt
  val lhs1 = io.src1(15, 8).asSInt
  val rhs1 = io.src2(15, 8).asSInt
  val lhs2 = io.src1(23, 16).asSInt
  val rhs2 = io.src2(23, 16).asSInt
  val lhs3 = io.src1(31, 24).asSInt
  val rhs3 = io.src2(31, 24).asSInt

  val acc = WireInit(0.U(32.W))
  acc := Mux(io.outType === INT8TYPE, Fill(24, io.src3(7)) ## io.src3(7, 0),
    Mux(io.outType === INT16TYPE, Fill(16, io.src3(15)) ## io.src3(15, 0), io.src3))
  val macc = lhs0 * rhs0 +& lhs1 * rhs1 +& lhs2 * rhs2 +& lhs3 * rhs3 +& acc.asSInt

  // add, sub
  val in1 = Mux(io.srcType === INT8TYPE, Fill(24, io.src1(7)) ## io.src1(7, 0),
    Mux(io.srcType === INT16TYPE, Fill(16, io.src1(15)) ## io.src1(15, 0), io.src1))
  val in2 = Mux(io.srcType === INT8TYPE, Fill(24, io.src3(7)) ## io.src3(7, 0),
    Mux(io.srcType === INT16TYPE, Fill(16, io.src3(15)) ## io.src3(15, 0), io.src3))
  val in2_inv = Mux(io.aluType === SUB, ~in2, in2)
  val adder = in1.asSInt +& in2_inv.asSInt + Mux(io.aluType === SUB, 1.S, 0.S)

  val result = Mux(io.aluType(1), adder, macc)

  io.out := Mux(result > sMax.asSInt, sMax,
    Mux(result < sMin.asSInt, sMin, result(31, 0)))
}

// may use blackbox hardfloat modules
class FpMacUnit(
                 val fpLatency: Int = 3,
                 val acc2Wider: Boolean = true
                 // )(implicit p: Parameters) extends BoomModule with ShouldBeRetimed {
               )(implicit p: Parameters) extends Module with ShouldBeRetimed {
  val io = IO(new Bundle {
    val validin = Input(Bool())
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(16.W))
    val src3 = Input(UInt(32.W))
    val srcType = Input(UInt(3.W))
    val outType = Input(UInt(3.W))
    val aluType = Input(UInt(4.W))
    val src1idx = Input(UInt(log2Ceil(numAccTiles).W))
    val src2idx = Input(UInt(log2Ceil(numAccTiles).W))
    val dstRidx = Input(UInt(log2Ceil(numAccTiles).W))
    val macInit = Input(Bool())
    val macLast = Input(Bool())
    val autoClr = Input(Bool())
    val autoCvt = Input(Bool())
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val validout = Output(Bool())
    val out = Output(UInt(32.W))
    val idx = Output(UInt(log2Ceil(numAccTiles).W))
  })

  require(fpLatency > 0)

  val H = new FType(5, 11)
  val S = new FType(8, 24)

  // Pipe stages.
  val pipeStage0 = Wire(Valid(new FpMacPipe))
  val pipeStage1 = Wire(Valid(new FpMacPipe))
  val pipeStage2 = Wire(Valid(new FpMacPipe))

  val pipeLatency = if (fpLatency >= 3) 1 else 0

  // Converter.
  val recA16ToRec32 = Module(new RecFNToRecFN(H.exp, H.sig, S.exp, S.sig))
  val recA32ToRec16 = Module(new RecFNToRecFN(S.exp, S.sig, H.exp, H.sig))
  val recC16ToRec32 = Module(new RecFNToRecFN(H.exp, H.sig, S.exp, S.sig))
  val recF16ToRec32 = Module(new RecFNToRecFN(H.exp, H.sig, S.exp, S.sig))

  // Multiplier.
  val mulRawFN = Module(new MulFullRawFN(H.exp, H.sig))
  val mulRawToRec16 = Module(new RoundAnyRawFNToRecFN(H.exp, H.sig * 2 - 1, H.exp, H.sig, 0))
  val mulRawToRec32 = Module(new RoundAnyRawFNToRecFN(H.exp, H.sig * 2 - 1, S.exp, S.sig, 0))

  // Adder.
  val addRawFN = Module(new AddRawFN(S.exp, S.sig))
  val addRawToRec16 = Module(new RoundAnyRawFNToRecFN(S.exp, S.sig + 2, H.exp, H.sig, 0))
  val addRawToRec32 = Module(new RoundAnyRawFNToRecFN(S.exp, S.sig + 2, S.exp, S.sig, 0))

  // Common signals.
  val fpOut = Wire(UInt(32.W))

  val mulRecOut16 = mulRawToRec16.io.out
  val mulRecOut32 = mulRawToRec32.io.out
  val pipeMulRecOut16 = Pipe(pipeStage1.valid, mulRecOut16, pipeLatency).bits
  val pipeMulRecOut32 = Pipe(pipeStage1.valid, mulRecOut32, pipeLatency).bits

  val addRecOut16 = addRawToRec16.io.out
  val addRecOut32 = addRawToRec32.io.out
  val pipeAddRecOut16 = Pipe(pipeStage1.valid, addRecOut16, pipeLatency).bits
  val pipeAddRecOut32 = Pipe(pipeStage1.valid, addRecOut32, pipeLatency).bits

  val addRecFwd32 = Wire(UInt((S.exp + S.sig + 1).W))
  val pipeAddRecFwd32 = Wire(UInt((S.exp + S.sig + 1).W))
  val addRec32From16 = recF16ToRec32.io.out
  val pipeAddRec32From16 = Pipe(pipeStage1.valid, addRec32From16, pipeLatency).bits
  if (acc2Wider) {
    //addRecFwd32 := addRecOut32
    //pipeAddRecFwd32 := pipeAddRecOut32
    addRecFwd32 := Mux((pipeStage1.bits.outType === FP16TYPE)
      && pipeStage1.bits.macInit, addRec32From16, addRecOut32)
    pipeAddRecFwd32 := Mux((pipeStage2.bits.outType === FP16TYPE)
      && pipeStage1.bits.macInit, pipeAddRec32From16, pipeAddRecOut32)
  } else {
    addRecFwd32 := Mux(pipeStage1.bits.outType === FP16TYPE, addRec32From16, addRecOut32)
    pipeAddRecFwd32 := Mux(pipeStage2.bits.outType === FP16TYPE, pipeAddRec32From16, pipeAddRecOut32)
  }

  // ------------------- Conversions. --------------------
  // Convert fp16 A, B, and C to recoded format
  val recAFP16 = recFNFromFN(H.exp, H.sig, io.src1(15, 0))
  val recBFP16 = recFNFromFN(H.exp, H.sig, io.src2(15, 0))
  val recCFP16 = recFNFromFN(H.exp, H.sig, io.src3(15, 0))
  // Convert fp32 A, B and C to recoded format
  val recAFP32 = recFNFromFN(S.exp, S.sig, io.src1)
  val recCFP32 = recFNFromFN(S.exp, S.sig, io.src3)

  // Convert fp16 to fp32 for float add/sub/rsub
  recA16ToRec32.io.in := recAFP16
  recA16ToRec32.io.roundingMode := pipeStage0.bits.roundingMode
  recA16ToRec32.io.detectTininess := pipeStage0.bits.detectTininess

  recC16ToRec32.io.in := recCFP16
  recC16ToRec32.io.roundingMode := pipeStage0.bits.roundingMode
  recC16ToRec32.io.detectTininess := pipeStage0.bits.detectTininess

  val recA = Mux(io.srcType === FP32TYPE, recAFP32, recA16ToRec32.io.out)
  val recC = Mux(io.srcType === FP32TYPE, recCFP32, recC16ToRec32.io.out)

  // FNCVT: convert fp32 to fp16 recoded format
  recA32ToRec16.io.in := recAFP32
  recA32ToRec16.io.roundingMode := pipeStage0.bits.roundingMode
  recA32ToRec16.io.detectTininess := pipeStage0.bits.detectTininess

  // Convert adder out rec16 to rec32.
  recF16ToRec32.io.in := addRecOut16
  recF16ToRec32.io.roundingMode := pipeStage1.bits.roundingMode
  recF16ToRec32.io.detectTininess := pipeStage1.bits.detectTininess

  // ------------------ Pipeline Stages. -------------------
  pipeStage0.valid := io.validin
  pipeStage0.bits.outType := io.outType
  pipeStage0.bits.aluType := io.aluType
  pipeStage0.bits.src1val := recAFP32
  pipeStage0.bits.src1idx := io.src1idx
  pipeStage0.bits.src2idx := io.src2idx
  pipeStage0.bits.dstRidx := io.dstRidx
  pipeStage0.bits.macInit := io.macInit
  pipeStage0.bits.macLast := io.macLast
  pipeStage0.bits.autoClr := io.autoClr
  pipeStage0.bits.autoCvt := io.autoCvt
  pipeStage0.bits.roundingMode := io.roundingMode
  pipeStage0.bits.detectTininess := io.detectTininess
  pipeStage1 := Pipe(pipeStage0.valid, pipeStage0.bits, pipeLatency)
  pipeStage2 := Pipe(pipeStage1.valid, pipeStage1.bits, pipeLatency)

  // ----------------- Recoded fp16 * fp16. -----------------
  val mulRecSrcA = recAFP16
  val mulRecSrcB = recBFP16
  val mulRawSrcA = rawFloatFromRecFN(H.exp, H.sig, mulRecSrcA)
  val mulRawSrcB = rawFloatFromRecFN(H.exp, H.sig, mulRecSrcB)
  mulRawFN.io.a := mulRawSrcA
  mulRawFN.io.b := mulRawSrcB

  // Round mul raw results to recFN(16)
  mulRawToRec16.io.invalidExc := mulRawFN.io.invalidExc
  mulRawToRec16.io.infiniteExc := false.B
  mulRawToRec16.io.in := mulRawFN.io.rawOut
  mulRawToRec16.io.roundingMode := pipeStage0.bits.roundingMode
  mulRawToRec16.io.detectTininess := pipeStage0.bits.detectTininess

  // Round mul raw results to recFN(32)
  mulRawToRec32.io.invalidExc := mulRawFN.io.invalidExc
  mulRawToRec32.io.infiniteExc := false.B
  mulRawToRec32.io.in := mulRawFN.io.rawOut
  mulRawToRec32.io.roundingMode := pipeStage0.bits.roundingMode
  mulRawToRec32.io.detectTininess := pipeStage0.bits.detectTininess

  // ----------------- Recoded fp32 + fp32. -----------------
  val bypRecSrcA = Mux(pipeStage1.valid && (pipeStage0.bits.src1idx === pipeStage1.bits.dstRidx), addRecFwd32,
    Mux(pipeStage2.valid && (pipeStage0.bits.src1idx === pipeStage2.bits.dstRidx), pipeAddRecFwd32, recA))
  val bypRecSrcB = Mux(pipeStage1.valid && (pipeStage0.bits.src2idx === pipeStage1.bits.dstRidx), addRecFwd32,
    Mux(pipeStage2.valid && (pipeStage0.bits.src2idx === pipeStage2.bits.dstRidx), pipeAddRecFwd32, recC))
  val addRecSrcA = Mux(pipeStage0.bits.aluType(3), 0.U,
    Mux(pipeStage0.bits.aluType(1), if (fpLatency > 1) bypRecSrcA else recA, mulRawToRec32.io.out))
  val addRecSrcB = if (fpLatency > 1) {
    Mux((pipeStage0.bits.aluType === MULT) || pipeStage0.bits.autoClr, 0.U, bypRecSrcB)
  } else recC
  val addRawSrcA = rawFloatFromRecFN(S.exp, S.sig, addRecSrcA)
  val addRawSrcB = rawFloatFromRecFN(S.exp, S.sig, addRecSrcB)
  val addSubOp = pipeStage0.bits.aluType === SUB
  val pipeAddRawSrcA = Pipe(pipeStage0.valid, addRawSrcA, pipeLatency).bits
  val pipeAddRawSrcB = Pipe(pipeStage0.valid, addRawSrcB, pipeLatency).bits
  val pipeAddSubOp = Pipe(pipeStage0.valid, addSubOp, pipeLatency).bits
  addRawFN.io.a := pipeAddRawSrcA
  addRawFN.io.b := pipeAddRawSrcB
  addRawFN.io.subOp := pipeAddSubOp
  addRawFN.io.roundingMode := pipeStage1.bits.roundingMode

  // Raw to rec16 & rec32.
  addRawToRec16.io.invalidExc := addRawFN.io.invalidExc
  addRawToRec16.io.infiniteExc := false.B
  addRawToRec16.io.in := addRawFN.io.rawOut
  addRawToRec16.io.roundingMode := pipeStage1.bits.roundingMode
  addRawToRec16.io.detectTininess := pipeStage1.bits.detectTininess

  addRawToRec32.io.invalidExc := addRawFN.io.invalidExc
  addRawToRec32.io.infiniteExc := false.B
  addRawToRec32.io.in := addRawFN.io.rawOut
  addRawToRec32.io.roundingMode := pipeStage1.bits.roundingMode
  addRawToRec32.io.detectTininess := pipeStage1.bits.detectTininess

  // ----------------- Output. -----------------
  // Select CVT source.
  val cvtRecSrc1 = Mux(pipeStage2.valid && (pipeStage2.bits.dstRidx === pipeStage0.bits.src1idx),
    pipeAddRecOut32, recAFP32)
  val pipeCvtRecSrc1 = Pipe(pipeStage0.valid, cvtRecSrc1, pipeLatency).bits

  val cvtRecSrc2 = Mux(pipeStage2.valid && (pipeStage2.bits.dstRidx === pipeStage1.bits.src1idx),
    pipeAddRecOut32, pipeCvtRecSrc1)
  val pipeCvtRecSrc2 = Pipe(pipeStage1.valid, cvtRecSrc2, pipeLatency).bits

  // REC FP32 to REC FP16.
  val cvtRec32ToRec16 = Module(new RecFNToRecFN(S.exp, S.sig, H.exp, H.sig))
  cvtRec32ToRec16.io.in := pipeCvtRecSrc2
  cvtRec32ToRec16.io.roundingMode := pipeStage2.bits.roundingMode
  cvtRec32ToRec16.io.detectTininess := pipeStage2.bits.detectTininess
  val cvtRecOut = cvtRec32ToRec16.io.out

  // Output value.
  val fp32Out = fNFromRecFN(S.exp, S.sig, pipeAddRecOut32)
  val fp16Out = Mux(pipeStage2.bits.aluType === CVT, fNFromRecFN(H.exp, H.sig, cvtRecOut),
    fNFromRecFN(H.exp, H.sig, pipeAddRecOut16))

  val autoCvt = (pipeStage2.bits.outType === FP16TYPE) || pipeStage2.bits.autoCvt
  if (acc2Wider) {
    fpOut := Mux(autoCvt && pipeStage2.bits.macLast, Cat(0.U(16.W), fp16Out), fp32Out)
  } else {
    fpOut := Mux(autoCvt, Cat(0.U(16.W), fp16Out), fp32Out)
  }

  // Output pipeline.
  //val outLatency = if (fpLatency >= 3) (fpLatency - 3) else if (fpLatency >= 1) (fpLatency - 1) else 0
  val outLatency = 0
  io.validout := Pipe(pipeStage2.valid, false.B, outLatency).valid
  io.out := Pipe(pipeStage2.valid, fpOut, outLatency).bits
  io.idx := Pipe(pipeStage2.valid, pipeStage2.bits.dstRidx, outLatency).bits
}

/**
 * A PE implementing a fp16 MAC operation or two int8 MAC operations.
 * c1 = a1 * b1 + c1 (for fp16 and int8)
 * c2 = a2 * b2 + c2 (for int8 only)
 * A PE is located in a two-dimensional MESH, rowIndex and colIndex indicate its location in horizontal and vertical directions.
 */
class PE(
          val rowIndex: Int,
          val colIndex: Int,
          val numReadPorts: Int = 1
          //      )(implicit p: Parameters) extends BoomModule {
        )(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // matrix multiplication related: mutiply-accumulate
    val macReqIn = Input(Valid(new MacCtrls())) // control signals
    val macReqSrcA = Input(UInt(32.W))
    val macReqSrcB = Input(UInt(32.W))
    val macReqSrcC = Input(UInt(16.W))
    val macReqSrcD = Input(UInt(16.W))
    val macReqOut = Output(Valid(new MacCtrls()))
    val macOutSrcA = Output(UInt(32.W)) // for systolic horizontally
    val macOutSrcB = Output(UInt(32.W)) // for systolic vertically
    val macOutSrcC = Output(UInt(16.W)) // for systolic vertically
    val macOutSrcD = Output(UInt(16.W)) // for systolic horizontally
    val accout = Output(Valid(UInt(32.W)))
    // clear tile slices, control signals propagated vertically
    val clrReqIn = Input(Valid(new ClrCtrls()))
    val clrReqOut = Output(Valid(new ClrCtrls()))
    // read row slices, control signals propagated vertically
    val rowReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadDin = Input(Vec(numReadPorts, UInt(64.W)))
    val rowReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadDout = Output(Vec(numReadPorts, UInt(64.W)))
    // write row slices, control signals propagated vertically
    val rowWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val rowWriteDin = Input(Vec(numVLdPorts, UInt(64.W)))
    val rowWriteMask = Input(Vec(numVLdPorts, UInt(2.W)))
    val rowWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val rowWriteDout = Output(Vec(numVLdPorts, UInt(64.W)))
    val rowWriteMout = Output(Vec(numVLdPorts, UInt(2.W)))
    // read col slices, control signals propagated horizontally
    val colReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadDin = Input(Vec(numReadPorts, UInt(32.W)))
    val colReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadDout = Output(Vec(numReadPorts, UInt(32.W)))
    // write col slices, control signals propagated horizontally
    val colWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val colWriteDin = Input(Vec(numVLdPorts, UInt(32.W)))
    val colWriteMask = Input(Vec(numVLdPorts, UInt(1.W)))
    val colWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val colWriteDout = Output(Vec(numVLdPorts, UInt(32.W)))
    val colWriteMout = Output(Vec(numVLdPorts, UInt(1.W)))
  })

  // acc registers
  val c0 = RegInit(VecInit(Seq.fill(numAccTiles)(0.U(32.W))))
  val c1 = RegInit(VecInit(Seq.fill(numAccTiles)(0.U(32.W))))

  val fpMultiCycles = true
  val fpAcc2Wider = false

  // -----------------------------------------------------------------------------------
  // matrix multiply-accumulate
  // -----------------------------------------------------------------------------------
  val macReqValid = io.macReqIn.valid
  val macReqCtrls = io.macReqIn.bits
  val fpMacValid = io.macReqIn.valid && macReqCtrls.srcType(2)
  // fp16*fp16+fp32
  val fpMac = Module(new FpMacUnit((if (fpMultiCycles) 3 else 1), fpAcc2Wider))
  fpMac.io.validin := fpMacValid
  fpMac.io.src1 := Mux(macReqCtrls.aluType === MACC && macReqCtrls.dirCal === 0.U, io.macReqSrcA, c0(macReqCtrls.src1Ridx))

  fpMac.io.src2 := Mux(macReqCtrls.dirCal === 1.U, io.macReqSrcC,
    Mux(macReqCtrls.dirCal === 2.U, io.macReqSrcA, io.macReqSrcB(15, 0)))
  fpMac.io.src3 := Mux(macReqCtrls.aluType === MULT, 0.U,
    Mux(macReqCtrls.dirCal === 1.U, io.macReqSrcB,
      Mux(macReqCtrls.dirCal === 2.U, io.macReqSrcD, c0(macReqCtrls.src2Ridx))))
  fpMac.io.srcType := macReqCtrls.outType // Attention here, fpMac support fp16 multiply only
  fpMac.io.outType := macReqCtrls.outType
  fpMac.io.aluType := macReqCtrls.aluType
  fpMac.io.src1idx := macReqCtrls.src1Ridx
  fpMac.io.src2idx := macReqCtrls.src2Ridx
  fpMac.io.dstRidx := macReqCtrls.dstRidx
  fpMac.io.macInit := macReqCtrls.macInit
  fpMac.io.macLast := macReqCtrls.macLast
  fpMac.io.autoClr := macReqCtrls.autoClr
  fpMac.io.autoCvt := macReqCtrls.autoCvt
  fpMac.io.roundingMode := macReqCtrls.rm
  fpMac.io.detectTininess := hardfloat.consts.tininess_afterRounding

  // int8 MAC function units
  val intMac = Module(new IntMacUnit())
  intMac.io.src1 := Mux(macReqCtrls.aluType === MACC, io.macReqSrcA, c0(macReqCtrls.src1Ridx))
  intMac.io.src2 := io.macReqSrcB
  intMac.io.src3 := Mux(macReqCtrls.aluType === MULT, 0.U, c0(macReqCtrls.src2Ridx))
  intMac.io.srcType := macReqCtrls.outType
  intMac.io.outType := macReqCtrls.outType
  intMac.io.aluType := macReqCtrls.aluType

  // TODO: Optimization, latency = 1 for int8 mac; 3 for fp16 mac
  when(fpMac.io.validout) {
    c0(fpMac.io.idx) := fpMac.io.out
  }

  when(macReqValid && (!macReqCtrls.srcType(2))) {
    c0(macReqCtrls.dstRidx) := intMac.io.out
  }

  io.macReqOut := io.macReqIn
  io.macOutSrcA := io.macReqSrcA
  io.macOutSrcB := io.macReqSrcB
  io.macOutSrcC := io.macReqSrcC
  io.macOutSrcD := io.macReqSrcD

  // -----------------------------------------------------------------------------------
  // clear acc tiles
  // -----------------------------------------------------------------------------------
  val clrReqValid = io.clrReqIn.valid
  val clrReqCtrls = io.clrReqIn.bits
  when(clrReqValid) {
    c0(clrReqCtrls.ridx) := 0.U
    c1(clrReqCtrls.ridx) := 0.U
  }

  io.clrReqOut := io.clrReqIn

  // -----------------------------------------------------------------------------------
  // read row slices
  // -----------------------------------------------------------------------------------
  for (i <- 0 until numReadPorts) {
    val rowReadValid = io.rowReadReq(i).valid
    val rowReadCtrls = io.rowReadReq(i).bits
    val rowRdata = WireInit(0.U(64.W))
    val rowReadHit = rowReadValid && rowReadCtrls.sidx === rowIndex.U
    rowRdata := Mux(!rowReadHit, io.rowReadDin(i), Cat(c1(rowReadCtrls.ridx), c0(rowReadCtrls.ridx)))

    io.rowReadResp(i) := io.rowReadReq(i)
    io.rowReadDout(i) := rowRdata
  }

  io.accout.valid := RegNext(io.macReqIn.valid)
  io.accout.bits := c0(0)
  // -----------------------------------------------------------------------------------
  // write row slices
  // -----------------------------------------------------------------------------------
  for (w <- 0 until numVLdPorts) {
    val rowWriteValid = io.rowWriteReq(w).valid
    val rowWrtieCtrls = io.rowWriteReq(w).bits
    val rowWriteHit = rowWriteValid && rowWrtieCtrls.sidx === rowIndex.U
    val rowWriteHitC0 = rowWriteHit && io.rowWriteMask(w)(0).asBool
    val rowWriteHitC1 = rowWriteHit && io.rowWriteMask(w)(1).asBool

    when(rowWriteHitC0) {
      c0(rowWrtieCtrls.ridx) := io.rowWriteDin(w)(31, 0)
    }
    when(rowWriteHitC1) {
      c1(rowWrtieCtrls.ridx) := io.rowWriteDin(w)(63, 32)
    }

    io.rowWriteResp(w) := io.rowWriteReq(w)
    io.rowWriteDout(w) := io.rowWriteDin(w)
    io.rowWriteMout(w) := io.rowWriteMask(w)
  }
  // -----------------------------------------------------------------------------------
  // read col slices
  // -----------------------------------------------------------------------------------
  // c0 index = colIndex; c1 index = colIndex+numMeshCols/2
  for (i <- 0 until numReadPorts) {
    val colReadValid = io.colReadReq(i).valid
    val colReadCtrls = io.colReadReq(i).bits
    val colRdata = WireInit(0.U(32.W))
    val colReadHitC0 = colReadValid && colReadCtrls.sidx === colIndex.U
    val colReadHitC1 = colReadValid && colReadCtrls.sidx === (colIndex + mxuPECols / 2).U
    colRdata := Mux(colReadHitC0, c0(colReadCtrls.ridx),
      Mux(colReadHitC1, c1(colReadCtrls.ridx), io.colReadDin(i)))

    io.colReadResp(i) := io.colReadReq(i)
    io.colReadDout(i) := colRdata
  }

  // -----------------------------------------------------------------------------------
  // write col slices
  // -----------------------------------------------------------------------------------
  for (w <- 0 until numVLdPorts) {
    val colWriteValid = io.colWriteReq(w).valid
    val colWriteCtrls = io.colWriteReq(w).bits
    val colWriteHitC0 = colWriteValid && colWriteCtrls.sidx === colIndex.U
    val colWriteHitC1 = colWriteValid && colWriteCtrls.sidx === (colIndex + mxuPECols / 2).U

    when(colWriteHitC0 && io.colWriteMask(w).asBool) {
      c0(colWriteCtrls.ridx) := io.colWriteDin(w)
    }.elsewhen(colWriteHitC1 && io.colWriteMask(w).asBool) {
      c1(colWriteCtrls.ridx) := io.colWriteDin(w)
    }

    io.colWriteResp(w) := io.colWriteReq(w)
    io.colWriteDout(w) := io.colWriteDin(w)
    io.colWriteMout(w) := io.colWriteMask(w)
  }
}

/**
 * A Tile is a purely combinational 2D array of passThrough PEs.
 */
class Tile(
            val indexh: Int,
            val indexv: Int,
            val numReadPorts: Int = 1
          )(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // matrix multiplication related: mutiply-accumulate
    val macReqIn = Input(Valid(new MacCtrls()))
    val macReqSrcA = Input(UInt((mxuTileRows * 32).W))
    val macReqSrcB = Input(UInt((mxuTileCols * 32).W))
    val macReqSrcC = Input(UInt((mxuTileCols * 16).W))
    val macReqSrcD = Input(UInt((mxuTileRows * 16).W))
    val macReqOut = Output(Valid(new MacCtrls()))
    val macOutSrcA = Output(UInt((mxuTileRows * 32).W))
    val macOutSrcB = Output(UInt((mxuTileCols * 32).W))
    val macOutSrcC = Output(UInt((mxuTileCols * 16).W))
    val macOutSrcD = Output(UInt((mxuTileRows * 16).W))
    // clear tile slices, control signals propagated vertically
    val clrReqIn = Input(Valid(new ClrCtrls()))
    val clrReqOut = Output(Valid(new ClrCtrls()))
    val accout = Output(Vec(mxuTileRows * mxuTileCols, Valid(UInt(32.W))))
    // read row slices, control signals propagated vertically
    val rowReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadDin = Input(Vec(numReadPorts, UInt((mxuTileCols * 64).W)))
    val rowReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadDout = Output(Vec(numReadPorts, UInt((mxuTileCols * 64).W)))
    // write row slices, control signals propagated vertically
    val rowWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val rowWriteDin = Input(Vec(numVLdPorts, UInt((mxuTileCols * 64).W)))
    val rowWriteMask = Input(Vec(numVLdPorts, UInt((mxuTileCols * 2).W)))
    val rowWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val rowWriteDout = Output(Vec(numVLdPorts, UInt((mxuTileCols * 64).W)))
    val rowWriteMout = Output(Vec(numVLdPorts, UInt((mxuTileCols * 2).W)))
    // read col slice, control signals propagated horizontally
    val colReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadDin = Input(Vec(numReadPorts, UInt((mxuTileRows * 32).W)))
    val colReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadDout = Output(Vec(numReadPorts, UInt((mxuTileRows * 32).W)))
    // write col slice, control signals propagated horizontally
    val colWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val colWriteDin = Input(Vec(numVLdPorts, UInt((mxuTileRows * 32).W)))
    val colWriteMask = Input(Vec(numVLdPorts, UInt(mxuTileRows.W)))
    val colWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val colWriteDout = Output(Vec(numVLdPorts, UInt((mxuTileRows * 32).W)))
    val colWriteMout = Output(Vec(numVLdPorts, UInt(mxuTileRows.W)))
  })

  val tile = Seq.tabulate(mxuTileRows, mxuTileCols)((i, j) => Module(new PE(indexh + i, indexv + j, numReadPorts)))
  val tileT = tile.transpose

  for (i <- 0 until mxuTileRows) {
    for (j <- 0 until mxuTileCols) {
      val index = i * mxuTileCols + j
      io.accout(index).bits := tile(i)(j).io.accout.bits
      io.accout(index).valid := tile(i)(j).io.accout.valid
    }
  }

  // broadcast horizontally across the tile
  for (r <- 0 until mxuTileRows) {
    val peSrcA = io.macReqSrcA(32 * r + 31, 32 * r)
    tile(r).foldLeft(peSrcA) {
      case (macReqSrcA, pe) => {
        pe.io.macReqSrcA := macReqSrcA
        pe.io.macOutSrcA
      }
    }
    val peSrcD = io.macReqSrcD(16 * r + 15, 16 * r)
    tile(r).foldLeft(peSrcD) {
      case (macReqSrcD, pe) => {
        pe.io.macReqSrcD := macReqSrcD
        pe.io.macOutSrcD
      }
    }
    for (w <- 0 until numVLdPorts) {
      tile(r).foldLeft(io.colWriteReq(w)) {
        case (colWriteReq, pe) => {
          pe.io.colWriteReq(w) := colWriteReq
          pe.io.colWriteResp(w)
        }
      }
      val peColWdata = io.colWriteDin(w)(32 * r + 31, 32 * r)
      tile(r).foldLeft(peColWdata) {
        case (colWriteData, pe) => {
          pe.io.colWriteDin(w) := colWriteData
          pe.io.colWriteDout(w)
        }
      }
      val peColWmask = io.colWriteMask(w)(r).asUInt
      tile(r).foldLeft(peColWmask) {
        case (colWriteMask, pe) => {
          pe.io.colWriteMask(w) := colWriteMask
          pe.io.colWriteMout(w)
        }
      }
    }
    for (i <- 0 until numReadPorts) {
      tile(r).foldLeft(io.colReadReq(i)) {
        case (colReadReq, pe) => {
          pe.io.colReadReq(i) := colReadReq
          pe.io.colReadResp(i)
        }
      }

      val peColRdata = io.colReadDin(i)(32 * r + 31, 32 * r)
      tile(r).foldLeft(peColRdata) {
        case (colReadData, pe) => {
          pe.io.colReadDin(i) := colReadData
          pe.io.colReadDout(i)
        }
      }
    }
  }

  // broadcast vertically
  for (c <- 0 until mxuTileCols) {
    tileT(c).foldLeft(io.macReqIn) {
      case (macReq, pe) => {
        pe.io.macReqIn := macReq
        pe.io.macReqOut
      }
    }
    val peSrcB = io.macReqSrcB(32 * c + 31, 32 * c)
    tileT(c).foldLeft(peSrcB) {
      case (macReqSrcB, pe) => {
        pe.io.macReqSrcB := macReqSrcB
        pe.io.macOutSrcB
      }
    }
    val peSrcC = io.macReqSrcC(16 * c + 15, 16 * c)
    tileT(c).foldLeft(peSrcC) {
      case (macReqSrcC, pe) => {
        pe.io.macReqSrcC := macReqSrcC
        pe.io.macOutSrcC
      }
    }
    tileT(c).foldLeft(io.clrReqIn) {
      case (clrReq, pe) => {
        pe.io.clrReqIn := clrReq
        pe.io.clrReqOut
      }
    }
    for (w <- 0 until numVLdPorts) {
      tileT(c).foldLeft(io.rowWriteReq(w)) {
        case (rowWriteReq, pe) => {
          pe.io.rowWriteReq(w) := rowWriteReq
          pe.io.rowWriteResp(w)
        }
      }
      val peRowWdata = io.rowWriteDin(w)(64 * c + 63, 64 * c)
      tileT(c).foldLeft(peRowWdata) {
        case (rowWriteData, pe) => {
          pe.io.rowWriteDin(w) := rowWriteData
          pe.io.rowWriteDout(w)
        }
      }
      val peRowWmask = io.rowWriteMask(w)(2 * c + 1, 2 * c)
      tileT(c).foldLeft(peRowWmask) {
        case (rowWriteMask, pe) => {
          pe.io.rowWriteMask(w) := rowWriteMask
          pe.io.rowWriteMout(w)
        }
      }
    }
    for (i <- 0 until numReadPorts) {
      tileT(c).foldLeft(io.rowReadReq(i)) {
        case (rowReadReq, pe) => {
          pe.io.rowReadReq(i) := rowReadReq
          pe.io.rowReadResp(i)
        }
      }
      val peRowRdata = io.rowReadDin(i)(64 * c + 63, 64 * c)
      tileT(c).foldLeft(peRowRdata) {
        case (rowReadData, pe) => {
          pe.io.rowReadDin(i) := rowReadData
          pe.io.rowReadDout(i)
        }
      }
    }
  }

  // tile's bottom IO
  io.macReqOut := io.macReqIn
  io.clrReqOut := io.clrReqIn
  for (w <- 0 until numVLdPorts) {
    io.rowWriteResp(w) := io.rowWriteReq(w)
    io.colWriteResp(w) := io.colWriteReq(w)
  }
  val macOutSrcAMux = WireInit(VecInit(Seq.fill(mxuTileRows)(0.U(16.W))))
  val macOutSrcDMux = WireInit(VecInit(Seq.fill(mxuTileRows)(0.U(16.W))))
  // val colWriteDataMux = WireInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuTileRows)(0.U(32.W)))))
  // val colWriteMaskMux = WireInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuTileRows)(0.U(1.W)))))
  val colWriteDataMux = Wire(Vec(numVLdPorts, Vec(mxuTileRows, UInt(32.W))))
  val colWriteMaskMux = Wire(Vec(numVLdPorts, Vec(mxuTileRows, UInt(1.W))))
  for (i <- 0 until numVLdPorts) {
    for (j <- 0 until mxuTileRows) {
      colWriteDataMux(i)(j) := 0.U
      colWriteMaskMux(i)(j) := 0.U
    }
  }
  for (r <- 0 until mxuTileRows) {
    macOutSrcAMux(r) := tile(r)(mxuTileCols - 1).io.macOutSrcA
    macOutSrcDMux(r) := tile(r)(mxuTileCols - 1).io.macOutSrcD
    for (w <- 0 until numVLdPorts) {
      colWriteDataMux(w)(r) := tile(r)(mxuTileCols - 1).io.colWriteDout(w)
      colWriteMaskMux(w)(r) := tile(r)(mxuTileCols - 1).io.colWriteMout(w)
    }
  }

  val macOutSrcBMux = WireInit(VecInit(Seq.fill(mxuTileCols)(0.U(16.W))))
  val macOutSrcCMux = WireInit(VecInit(Seq.fill(mxuTileCols)(0.U(16.W))))
  // val rowWriteDataMux = WireInit(VecInit(Seq.fill(mxuTileCols)(0.U(64.W))))
  val rowWriteDataMux = Wire(Vec(numVLdPorts, Vec(mxuTileCols, UInt(64.W))))
  val rowWriteMaskMux = Wire(Vec(numVLdPorts, Vec(mxuTileCols, UInt(2.W))))
  for (i <- 0 until numVLdPorts) {
    for (j <- 0 until mxuTileCols) {
      rowWriteDataMux(i)(j) := 0.U
      rowWriteMaskMux(i)(j) := 0.U
    }
  }
  for (c <- 0 until mxuTileCols) {
    macOutSrcBMux(c) := tile(mxuTileRows - 1)(c).io.macOutSrcB
    macOutSrcCMux(c) := tile(mxuTileRows - 1)(c).io.macOutSrcC
    for (w <- 0 until numVLdPorts) {
      rowWriteDataMux(w)(c) := tile(mxuTileRows - 1)(c).io.rowWriteDout(w)
      rowWriteMaskMux(w)(c) := tile(mxuTileRows - 1)(c).io.rowWriteMout(w)
    }
  }
  io.macOutSrcA := macOutSrcAMux.asUInt
  io.macOutSrcD := macOutSrcDMux.asUInt
  io.macOutSrcB := macOutSrcBMux.asUInt
  io.macOutSrcC := macOutSrcCMux.asUInt
  for (w <- 0 until numVLdPorts) {
    io.colWriteDout(w) := colWriteDataMux(w).asUInt
    io.colWriteMout(w) := colWriteMaskMux(w).asUInt
    io.rowWriteDout(w) := rowWriteDataMux(w).asUInt
    io.rowWriteMout(w) := rowWriteMaskMux(w).asUInt
  }

  for (i <- 0 until numReadPorts) {
    val colReadDataMux = WireInit(VecInit(Seq.fill(mxuTileRows)(0.U(32.W))))
    for (r <- 0 until mxuTileRows) {
      colReadDataMux(r) := tile(r)(mxuTileCols - 1).io.colReadDout(i)
    }

    val rowReadDataMux = WireInit(VecInit(Seq.fill(mxuTileCols)(0.U(64.W))))
    for (c <- 0 until mxuTileCols) {
      rowReadDataMux(c) := tile(mxuTileRows - 1)(c).io.rowReadDout(i)
    }

    io.rowReadResp(i) := io.rowReadReq(i)
    io.colReadResp(i) := io.colReadReq(i)
    io.colReadDout(i) := colReadDataMux.asUInt
    io.rowReadDout(i) := rowReadDataMux.asUInt
  }
}

object VerilogMMA extends App {
  println("Generating hardware")
  val p = Parameters.empty
  emitVerilog(new Mesh()(p.alterPartial({ case VFuParamsKey =>
    VFuParameters(VLEN = 256)
  })), Array("--target-dir", "generated",
    "--emission-options=disableMemRandomization,disableRegisterRandomization"))
}

class Mesh(
            val numReadPorts: Int = 1
            //        )(implicit p: Parameters) extends BoomModule {
          )(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // matrix multiplication related: mutiply-accumulate
    val macReq = Input(Vec(mxuMeshCols, Valid(new MacCtrls())))
    val macReqSrcA = Input(Vec(mxuMeshRows, UInt((mxuTileRows * 32).W)))
    val macReqSrcB = Input(Vec(mxuMeshCols, UInt((mxuTileCols * 32).W)))
    val macReqSrcC = Input(Vec(mxuMeshCols, UInt((mxuTileCols * 16).W)))
    val macReqSrcD = Input(Vec(mxuMeshRows, UInt((mxuTileRows * 16).W)))
    val macResp = Output(Valid(new MacCtrls()))
    // clear tile slices, control signals propagated vertically
    val clrReq = Input(Valid(new ClrCtrls()))
    val clrResp = Output(Valid(new ClrCtrls()))

    val accout = Output(Vec(mxuMeshRows * mxuMeshCols, Valid(UInt((mxuTileRows * mxuTileCols * 32).W))))
    // read row slices, control signals propagated vertically
    val rowReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val rowReadData = Output(Vec(numReadPorts, UInt((mxuPECols * 32).W)))
    // write row slices, control signals propagated vertically
    val rowWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val rowWriteData = Input(Vec(numVLdPorts, UInt((mxuPECols * 32).W)))
    val rowWriteMask = Input(Vec(numVLdPorts, UInt((mxuPECols).W)))
    val rowWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
    // read col slice, control signals propagated horizontally
    val colReadReq = Input(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadResp = Output(Vec(numReadPorts, Valid(new SliceCtrls())))
    val colReadData = Output(Vec(numReadPorts, UInt((mxuPERows * 32).W)))
    // write col slice, control signals propagated horizontally
    val colWriteReq = Input(Vec(numVLdPorts, Valid(new SliceCtrls())))
    val colWriteData = Input(Vec(numVLdPorts, UInt((mxuPERows * 32).W)))
    val colWriteMask = Input(Vec(numVLdPorts, UInt((mxuPERows).W)))
    val colWriteResp = Output(Vec(numVLdPorts, Valid(new SliceCtrls())))
  })

  val mesh = Seq.tabulate(mxuMeshRows, mxuMeshCols)((i, j) => Module(new Tile(i * mxuTileRows, j * mxuTileCols, numReadPorts)))
  val meshT = mesh.transpose

  for (i <- 0 until mxuMeshRows) {
    for (j <- 0 until mxuMeshCols) {
      val index = i * mxuMeshRows + j
      io.accout(index).bits := Cat(mesh(i)(j).io.accout.map(_.bits).reverse)
      io.accout(index).valid := mesh(i)(j).io.accout(0).valid
    }
  }

  // propagate horizontally across the mesh
  for (r <- 0 until mxuMeshRows) {
    mesh(r).foldLeft(io.macReqSrcA(r)) {
      case (macReqSrcA, tile) => {
        tile.io.macReqSrcA := RegNext(macReqSrcA)
        tile.io.macOutSrcA
      }
    }
    mesh(r).foldLeft(io.macReqSrcD(r)) {
      case (macReqSrcD, tile) => {
        tile.io.macReqSrcD := RegNext(macReqSrcD)
        tile.io.macOutSrcD
      }
    }
    for (w <- 0 until numVLdPorts) {
      mesh(r).foldLeft(io.colWriteReq(w)) {
        case (colWriteReq, tile) => {
          tile.io.colWriteReq(w) := RegNext(colWriteReq)
          tile.io.colWriteResp(w)
        }
      }
      mesh(r).foldLeft(io.colWriteData(w)(32 * mxuTileRows * (r + 1) - 1, 32 * mxuTileRows * r)) {
        case (colWriteData, tile) => {
          tile.io.colWriteDin(w) := RegNext(colWriteData)
          tile.io.colWriteDout(w)
        }
      }
      mesh(r).foldLeft(io.colWriteMask(w)(mxuTileRows * (r + 1) - 1, mxuTileRows * r)) {
        case (colWriteMask, tile) => {
          tile.io.colWriteMask(w) := RegNext(colWriteMask)
          tile.io.colWriteMout(w)

        }
      }
    }
    for (i <- 0 until numReadPorts) {
      mesh(r).foldLeft(io.colReadReq(i)) {
        case (colReadReq, tile) => {
          tile.io.colReadReq(i) := RegNext(colReadReq)
          tile.io.colReadResp(i)
        }
      }
      mesh(r).foldLeft(0.U(32.W)) {
        case (colReadData, tile) => {
          tile.io.colReadDin(i) := RegNext(colReadData)
          tile.io.colReadDout(i)
        }
      }
    }
  }

  // propagate vertically across the mesh
  for (c <- 0 until mxuMeshCols) {
    meshT(c).foldLeft(io.macReq(c)) {
      case (macReq, tile) => {
        tile.io.macReqIn := RegNext(macReq)
        tile.io.macReqOut
      }
    }
    meshT(c).foldLeft(io.macReqSrcB(c)) {
      case (macReqSrcB, tile) => {
        tile.io.macReqSrcB := RegNext(macReqSrcB)
        tile.io.macOutSrcB
      }
    }
    meshT(c).foldLeft(io.macReqSrcC(c)) {
      case (macReqSrcC, tile) => {
        tile.io.macReqSrcC := RegNext(macReqSrcC)
        tile.io.macOutSrcC
      }
    }
    meshT(c).foldLeft(io.clrReq) {
      case (clrReq, tile) => {
        tile.io.clrReqIn := RegNext(clrReq)
        tile.io.clrReqOut
      }
    }
    for (w <- 0 until numVLdPorts) {
      meshT(c).foldLeft(io.rowWriteReq(w)) {
        case (rowWriteReq, tile) => {
          tile.io.rowWriteReq(w) := RegNext(rowWriteReq)
          tile.io.rowWriteResp(w)
        }
      }
      // meshT(c).foldLeft(io.rowWriteData(w)(64 * mxuTileCols * (c + 1) - 1, 64 * mxuTileCols * c)) {
      meshT(c).foldLeft(io.rowWriteData(w)(32 * mxuTileCols * (c + 1) - 1, 32 * mxuTileCols * c)) {
        case (rowWriteData, tile) => {
          tile.io.rowWriteDin(w) := RegNext(rowWriteData)
          tile.io.rowWriteDout(w)
        }
      }
      // meshT(c).foldLeft(io.rowWriteMask(w)(2 * mxuTileCols * (c + 1) - 1, 2 * mxuTileCols * c)) {
      meshT(c).foldLeft(io.rowWriteMask(w)(1 * mxuTileCols * (c + 1) - 1, 1 * mxuTileCols * c)) {
        case (rowWriteMask, tile) => {
          tile.io.rowWriteMask(w) := RegNext(rowWriteMask)
          tile.io.rowWriteMout(w)
        }
      }
    }
    for (i <- 0 until numReadPorts) {
      meshT(c).foldLeft(io.rowReadReq(i)) {
        case (rowReadReq, tile) => {
          tile.io.rowReadReq(i) := RegNext(rowReadReq)
          tile.io.rowReadResp(i)
        }
      }
      meshT(c).foldLeft(0.U(64.W)) {
        case (rowReadData, tile) => {
          tile.io.rowReadDin(i) := RegNext(rowReadData)
          tile.io.rowReadDout(i)
        }
      }
    }
  }

  // bottom IOs
  io.macResp := mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.macReqOut
  io.clrResp := mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.clrReqOut
  for (w <- 0 until numVLdPorts) {
    io.rowWriteResp(w) := RegNext(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.rowWriteResp(w))
    io.colWriteResp(w) := RegNext(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.colWriteResp(w))
  }

  for (i <- 0 until numReadPorts) {
    io.rowReadResp(i).valid := RegNext(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.rowReadResp(i).valid)
    io.rowReadResp(i).bits := RegEnable(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.rowReadResp(i).bits, mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.rowReadResp(i).valid)
    io.colReadResp(i).valid := RegNext(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.colReadResp(i).valid)
    io.colReadResp(i).bits := RegEnable(mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.colReadResp(i).bits, mesh(mxuMeshRows - 1)(mxuMeshCols - 1).io.colReadResp(i).valid)
    val colRdataMux = WireInit(VecInit(Seq.fill(mxuMeshRows)(0.U((mxuTileRows * 32).W))))
    for (r <- 0 until mxuMeshRows) {
      // use RegEnable to lock valid data
      colRdataMux(r) := RegEnable(mesh(r)(mxuMeshCols - 1).io.colReadDout(i), mesh(r)(mxuMeshCols - 1).io.colReadResp(i).valid)
    }

    val rowRdataMux = WireInit(VecInit(Seq.fill(mxuMeshCols)(0.U((mxuTileCols * 64).W))))
    for (c <- 0 until mxuMeshCols) {
      rowRdataMux(c) := RegEnable(mesh(mxuMeshRows - 1)(c).io.rowReadDout(i), mesh(mxuMeshRows - 1)(c).io.rowReadResp(i).valid)
    }
    io.colReadData(i) := colRdataMux.asUInt
    io.rowReadData(i) := rowRdataMux.asUInt
  }
}

// instantiate MESH with Delays
// class OuterProductUnit(implicit p: Parameters) extends BoomModule {
//   class OuterProductUnit(implicit p: Parameters) extends Module {
//   val io = IO(new Bundle {
//     // matrix multiplication relate
//     val macReq = Flipped(Decoupled(new MacCtrls()))
//     val macReqUop = Input(new MicroOp())
//     val macReqSrcA = Input(UInt(rLen.W))
//     val macReqSrcB = Input(UInt(rLen.W))
//     val macResp = Output(Valid(new MacCtrls()))
//     val macRespUop = Output(new MicroOp())
//     // clear tile slices, control signals propagated vertically
//     val clrReq = Flipped(Decoupled(new ClrCtrls()))
//     val clrReqUop = Input(new MicroOp())
//     val clrResp = Output(Valid(new ClrCtrls()))
//     val clrRespUop = Output(new MicroOp())
//     // read row slices, m-pipeline
//     val rowReadReq = Flipped(Decoupled(new SliceCtrls()))
//     val rowReadReqUop = Input(new MicroOp())
//     val rowReadResp = Output(Valid(new SliceCtrls()))
//     val rowReadRespUop = Output(new MicroOp())
//     val rowReadData = Decoupled(UInt(rLen.W))
//     val rowReadMask = Output(UInt(rLenb.W))
//     // write row slices
//     val rowWriteReq = Flipped(Vec(numVLdPorts,Decoupled(new SliceCtrls())))
//     val rowWriteReqUop = Input(Vec(numVLdPorts,new MicroOp()))
//     val rowWriteData = Input(Vec(numVLdPorts,UInt(rLen.W)))
//     val rowWriteMask = Input(Vec(numVLdPorts,UInt(rLenb.W)))
//     val rowWriteResp = Output(Vec(numVLdPorts,Valid(new SliceCtrls())))
//     val rowWriteRespUop = Output(Vec(numVLdPorts,new MicroOp()))
//     // read col slices, m-pipeline
//     val colReadReq = Flipped(Decoupled(new SliceCtrls()))
//     val colReadReqUop = Input(new MicroOp())
//     val colReadResp = Output(Valid(new SliceCtrls()))
//     val colReadRespUop = Output(new MicroOp())
//     val colReadData = Decoupled(UInt(rLen.W))
//     val colReadMask = Output(UInt(rLenb.W))
//     // write col slices
//     val colWriteReq = Flipped(Vec(numVLdPorts,Decoupled(new SliceCtrls())))
//     val colWriteReqUop = Input(Vec(numVLdPorts,new MicroOp()))
//     val colWriteData = Input(Vec(numVLdPorts,UInt(rLen.W)))
//     val colWriteMask = Input(Vec(numVLdPorts,UInt(rLenb.W)))
//     val colWriteResp = Output(Vec(numVLdPorts,Valid(new SliceCtrls())))
//     val colWriteRespUop = Output(Vec(numVLdPorts,new MicroOp()))
//     // read row slices from acc, lsu
//     val accReadReq = Input(Valid(new AccReadReq()))
//     val accReadResp = Output(Valid(new AccReadResp()))
//   })
//
//   require(rLen >= mxuPERows * 16)
//
//   val macReqFire = io.macReq.valid & io.macReq.ready
//   val clrReqFire = io.clrReq.valid & io.clrReq.ready
//
//   val mesh = Module(new Mesh(numReadPorts = 2))
//
//   // row slice writes and col slice writes may occured simultaneously when writing different acc tiles,
//   // should be guaranteed by rename/issue logic
//   val sliceReady :: sliceWait :: sliceResp :: trSliceResp :: Nil = Enum(4)
//   val rowReadState = RegInit(sliceReady)
//   val colReadState = RegInit(sliceReady)
//   val rowVsCount = RegInit(0.U(2.W))
//   val colVsCount = RegInit(0.U(2.W))
//   val rowReadReqValid = RegInit(false.B)
//   val rowWriteReqValid = RegInit(VecInit(Seq.fill(numVLdPorts)(false.B)))
//   val colReadReqValid = RegInit(false.B)
//   val colWriteReqValid = RegInit(VecInit(Seq.fill(numVLdPorts)(false.B)))
//   val rowWriteDataAggr = RegInit(VecInit(Seq.fill(numVLdPorts)(VecInit(Seq.fill(mxuPECols)(0.U(32.W))))))
//   val rowWriteMaskAggr = RegInit(VecInit(Seq.fill(numVLdPorts)(VecInit(Seq.fill(mxuPECols)(0.U(1.W))))))
//   val colWriteDataAggr = RegInit(VecInit(Seq.fill(numVLdPorts)(VecInit(Seq.fill(mxuPERows)(0.U(32.W))))))
//   val colWriteMaskAggr = RegInit(VecInit(Seq.fill(numVLdPorts)(VecInit(Seq.fill(mxuPERows)(0.U(1.W))))))
//   //val rowWriteDataAggr = RegInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuPECols)(0.U(32.W)))))
//   // val rowWriteMaskAggr = RegInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuPECols)(0.U(1.W)))))
//   // val colWriteDataAggr = RegInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuPERows)(0.U(32.W)))))
//   // val colWriteMaskAggr = RegInit(Vec(numVLdPorts,VecInit(Seq.fill(mxuPERows)(0.U(1.W)))))
//
//   // -----------------------------------------------------------------------------------
//   // read row slices, write data to long-latency vector registers
//   // add back-pressure mechanism (not pipelined)
//   // -----------------------------------------------------------------------------------
//   val is_mmv = io.macReqUop.uopc.isOneOf(uopMMV_V)
//   val trRowValid = ShiftRegister(io.rowReadReq.valid && io.rowReadReqUop.rt(RS1, isTrTile), mxuMeshRows + 2)
//   val trRowData = Pipe(io.rowReadReq.valid, io.macReqSrcA, mxuMeshRows + 2).bits
//   val rowReadCtrls = io.rowReadReq.bits
//   val rowVregIdx = RegInit(0.U(3.W))
//   rowReadReqValid := false.B
//   switch(rowReadState) {
//     is(sliceReady) {
//       when(io.rowReadReq.valid && io.rowReadReqUop.rt(RS1, isAccTile)) {
//         rowVsCount := Mux(is_mmv,0.U,Mux(io.rowReadReqUop.vd_emul === 2.U, 3.U, io.rowReadReqUop.vd_emul))
//         rowReadState := sliceWait
//         rowReadReqValid := true.B
//       }.elsewhen(io.rowReadReq.valid) {
//         rowReadState := sliceWait
//       }
//     }
//     is(sliceWait) {
//       when(io.rowReadData.fire && rowVsCount === 0.U) {
//         rowReadState := sliceReady
//       }.elsewhen(io.rowReadData.fire) {
//         rowReadState := sliceResp
//         rowVregIdx := rowVregIdx + 1.U
//       }.elsewhen(mesh.io.rowReadResp(0).valid) {
//         rowReadState := sliceResp
//       }.elsewhen(trRowValid) {
//         rowReadState := trSliceResp
//       }
//     }
//     is(sliceResp) {
//       when(io.rowReadData.fire && rowVregIdx === rowVsCount) {
//         rowReadState := sliceReady
//         rowVregIdx := 0.U
//       }.elsewhen(io.rowReadData.fire) {
//         rowVregIdx := rowVregIdx + 1.U
//       }
//     }
//     is(trSliceResp) {
//       when(io.rowReadData.fire) {
//         rowReadState := sliceReady
//       }
//     }
//   }
//
//   // -----------------------------------------------------------------------------------
//   // write row slices
//   // -----------------------------------------------------------------------------------
//   for (w <- 0 until numVLdPorts){
//     val rowWriteCtrls = io.rowWriteReq(w).bits
//     rowWriteReqValid(w) := false.B
//
//     val rowSliceIdx = io.rowWriteReqUop(w).m_slice_quad
//     when(io.rowWriteReq(w).valid) {
//     (0 until mxuMeshCols * mxuTileCols * 2).foreach { i =>
//       rowWriteDataAggr(w)(i) := 0.U
//       rowWriteMaskAggr(w)(i) := 0.U
//     }
//     when(rowWriteCtrls.sew === 0.U) {
//       (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//         rowWriteDataAggr(w)(2 * i) := io.rowWriteData(w)(8 * i + 7, 8 * i)
//         rowWriteMaskAggr(w)(2 * i) := io.rowWriteMask(w)(i)
//         rowWriteDataAggr(w)(2 * i + 1) := io.rowWriteData(w)(8 * mxuMeshCols * mxuTileCols + 8 * i + 7, 8 * mxuMeshCols * mxuTileCols + 8 * i)
//         rowWriteMaskAggr(w)(2 * i + 1) := io.rowWriteMask(w)(mxuMeshCols * mxuTileCols + i)
//       }
//     }.elsewhen(rowWriteCtrls.sew === 1.U) {
//       when(rowSliceIdx === W0) {
//         (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//           rowWriteDataAggr(w)(2 * i) := io.rowWriteData(w)(16 * i + 15, 16 * i)
//           rowWriteMaskAggr(w)(2 * i) := io.rowWriteMask(w)(2 * i)
//         }
//       }.otherwise {
//         (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//           rowWriteDataAggr(w)(2 * i + 1) := io.rowWriteData(w)(16 * i + 15, 16 * i)
//           rowWriteMaskAggr(w)(2 * i + 1) := io.rowWriteMask(w)(2 * i)
//         }
//       }
//     }.otherwise {
//       assert(rowWriteCtrls.sew === 2.U, "sew === 3.U (64 bits) not supported!\n")
//       when(rowSliceIdx === Q0) {
//         (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//           rowWriteDataAggr(w)(2 * i) := io.rowWriteData(w)(32 * i + 31, 32 * i)
//           rowWriteMaskAggr(w)(2 * i) := io.rowWriteMask(w)(4 * i)
//         }
//       }.elsewhen(rowSliceIdx === Q1) {
//         (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//           rowWriteDataAggr(w)(mxuMeshCols * mxuTileCols + 2 * i) := io.rowWriteData(w)(32 * i + 31, 32 * i)
//           rowWriteMaskAggr(w)(mxuMeshCols * mxuTileCols + 2 * i) := io.rowWriteMask(w)(4 * i)
//         }
//       }.elsewhen(rowSliceIdx === Q2) {
//         (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//           rowWriteDataAggr(w)(2 * i + 1) := io.rowWriteData(w)(32 * i + 31, 32 * i)
//           rowWriteMaskAggr(w)(2 * i + 1) := io.rowWriteMask(w)(4 * i)
//         }
//       }.otherwise {
//         (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//           rowWriteDataAggr(w)(mxuMeshCols * mxuTileCols + 2 * i + 1) := io.rowWriteData(w)(32 * i + 31, 32 * i)
//           rowWriteMaskAggr(w)(mxuMeshCols * mxuTileCols + 2 * i + 1) := io.rowWriteMask(w)(4 * i)
//         }
//       }
//     }
//     }
//   }
//
//   // -----------------------------------------------------------------------------------
//   // read col slices, write data to long-latency vector registers
//   // add back-pressure mechanism (not pipelined)
//   // -----------------------------------------------------------------------------------
//   val trColValid = ShiftRegister(io.colReadReq.valid && io.colReadReqUop.rt(RS1, isTrTile), mxuMeshCols + 2)
//   val trColData = Pipe(io.colReadReq.valid, io.macReqSrcA, mxuMeshCols).bits
//   val colReadCtrls = io.colReadReq.bits
//   val colVregIdx = RegInit(0.U(3.W))
//   colReadReqValid := false.B
//   switch(colReadState) {
//     is(sliceReady) {
//       when(io.colReadReq.valid && io.colReadReqUop.rt(RS1, isAccTile)) {
//         colVsCount := io.colReadReqUop.vd_emul
//         colReadState := sliceWait
//         colReadReqValid := true.B
//       }.elsewhen(io.colReadReq.valid) {
//         colReadState := sliceWait
//       }
//     }
//     is(sliceWait) {
//       when(io.colReadData.fire && colVsCount === 0.U) {
//         colReadState := sliceReady
//       }.elsewhen(io.colReadData.fire) {
//         colReadState := sliceResp
//         colVregIdx := colVregIdx + 1.U
//       }.elsewhen(mesh.io.colReadResp(0).valid) {
//         colReadState := sliceResp
//       }.elsewhen(trColValid) {
//         colReadState := trSliceResp
//       }
//     }
//     is(sliceResp) {
//       when(io.colReadData.fire && colVregIdx === colVsCount) {
//         colReadState := sliceReady
//         colVregIdx := 0.U
//       }.elsewhen(io.colReadData.fire) {
//         colVregIdx := colVregIdx + 1.U
//       }
//     }
//     is(trSliceResp) {
//       when(io.colReadData.fire) {
//         colReadState := sliceReady
//       }
//     }
//   }
//
//   // -----------------------------------------------------------------------------------
//   // write col slices
//   // -----------------------------------------------------------------------------------
//   for (w <- 0 until numVLdPorts){
//     val colWriteCtrls = io.colWriteReq(w).bits
//     rowWriteReqValid(w) := false.B
//     val colSliceIdx = io.colWriteReqUop(w).m_slice_quad
//     when(io.colWriteReq(w).valid) {
//       (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//         colWriteDataAggr(w)(i) := 0.U
//         colWriteMaskAggr(w)(i) := 0.U
//       }
//       when(colWriteCtrls.sew === 0.U) {
//         (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//           colWriteDataAggr(w)(i) := io.colWriteData(w)(8 * i + 7, 8 * i)
//           colWriteMaskAggr(w)(i) := io.colWriteMask(w)(i)
//         }
//       }.elsewhen(colWriteCtrls.sew === 1.U) {
//         (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//           colWriteDataAggr(w)(i) := io.colWriteData(w)(16 * i + 15, 16 * i)
//           colWriteMaskAggr(w)(i) := io.colWriteMask(w)(2 * i)
//         }
//       }.otherwise {
//           when(colSliceIdx === Q0) {
//             (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//             colWriteDataAggr(w)(i) := io.colWriteData(w)(32 * i + 31, 32 * i)
//             colWriteMaskAggr(w)(i) := io.colWriteMask(w)(4 * i)
//             }
//           }.otherwise {
//             (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//             colWriteDataAggr(w)(mxuMeshRows * mxuTileRows / 2 + i) := io.colWriteData(w)(32 * i + 31, 32 * i)
//             colWriteMaskAggr(w)(mxuMeshRows * mxuTileRows / 2 + i) := io.colWriteMask(w)(4 * i)
//             }
//           }
//         assert(colWriteCtrls.sew === 2.U, "INT64 and FP64 not supported\n")
//       }
//     }
//   }
//
//   // -----------------------------------------------------------------------------------
//   // mopa
//   // -----------------------------------------------------------------------------------
//   mesh.io.clrReq.valid := clrReqFire
//   mesh.io.clrReq.bits := io.clrReq.bits
//   for (w <- 0 until numVLdPorts) {
//     mesh.io.rowWriteReq(w).valid := RegNext(io.rowWriteReq(w).valid)
//     mesh.io.rowWriteReq(w).bits := RegNext(io.rowWriteReq(w).bits)
//     mesh.io.rowWriteData(w) := rowWriteDataAggr(w).asUInt
//     mesh.io.rowWriteMask(w) := rowWriteMaskAggr(w).asUInt
//     mesh.io.colWriteReq(w).valid := RegNext(io.colWriteReq(w).valid)
//     mesh.io.colWriteReq(w).bits := RegNext(io.colWriteReq(w).bits)
//     mesh.io.colWriteData(w) := colWriteDataAggr(w).asUInt
//     mesh.io.colWriteMask(w) := colWriteMaskAggr(w).asUInt
//   }
//   mesh.io.rowReadReq(0).valid := rowReadReqValid
//   mesh.io.rowReadReq(0).bits := RegNext(io.rowReadReq.bits)
//   mesh.io.colReadReq(0).valid := colReadReqValid
//   mesh.io.colReadReq(0).bits := RegNext(io.colReadReq.bits)
//   mesh.io.rowReadReq(1).valid := io.accReadReq.valid && !io.accReadReq.bits.tt.asBool
//   mesh.io.rowReadReq(1).bits := io.accReadReq.bits.sCtrls
//   mesh.io.colReadReq(1).valid := io.accReadReq.valid && io.accReadReq.bits.tt.asBool
//   mesh.io.colReadReq(1).bits := io.accReadReq.bits.sCtrls
//   for (c <- 0 until mxuMeshCols) {
//     mesh.io.macReq(c).valid := ShiftRegister(macReqFire, c, true.B)
//     mesh.io.macReq(c).bits := ShiftRegister(io.macReq.bits, c, true.B)
//   }
//
//   val rs1_is_vec = io.macReqUop.rt(RS1, isVector)
//   val rs2_is_vec = io.macReqUop.rt(RS2, isVector)
//   val has_one_vec = rs1_is_vec ^ rs2_is_vec
//   val is_transpose = io.macReqUop.transposed
//
//   val is_row_cal = has_one_vec && !is_transpose
//   val is_col_cal = has_one_vec && is_transpose
//
//   val widenSrcA = WireInit(VecInit(Seq.fill(mxuMeshRows * mxuTileRows)(0.U(16.W))))
//   (0 until mxuMeshRows * mxuTileRows).foreach(i => widenSrcA(i) := io.macReqSrcA(8 * i + 7, 8 * i))
//   val tmp_muxSrcA = Mux(io.macReq.bits.srcType(2), io.macReqSrcA ,widenSrcA.asUInt)
//   val muxSrcA = Mux(!rs1_is_vec, tmp_muxSrcA,Mux(io.macReqUop.m_sidx === 0.U && !is_transpose, tmp_muxSrcA.asUInt & Fill(128,1.U), Fill(8,0x0.U(16.W))))
//   val muxSrcD = Mux(is_row_cal && io.macReqUop.m_sidx === 0.U, io.macReqSrcB, 0.U)
//   for (r <- 0 until mxuMeshRows) {
//     mesh.io.macReqSrcA(r) := ShiftRegister(muxSrcA(16 * mxuTileRows * (r + 1) - 1, 16 * mxuTileRows * r), r, true.B)
//     mesh.io.macReqSrcD(r) := ShiftRegister(muxSrcD(16 * mxuTileRows * (r + 1) - 1, 16 * mxuTileRows * r), r, true.B)
//   }
//
//   val shuffSrcB = WireInit(VecInit(Seq.fill(mxuMeshCols * mxuTileCols * 2)(0.U(8.W))))
//   val shuffSrcC = WireInit(VecInit(Seq.fill(mxuMeshCols * mxuTileCols * 2)(0.U(8.W))))
//   when(io.macReq.bits.srcType(2)) {
//     (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//       shuffSrcB(2 * i) := io.macReqSrcB(16 * i + 7, 16 * i)
//       shuffSrcB(2 * i + 1) := io.macReqSrcB(16 * i + 15, 16 * i + 8)
//       shuffSrcC(2 * i) := io.macReqSrcA(16 * i + 7, 16 * i)
//       shuffSrcC(2 * i + 1) := io.macReqSrcA(16 * i + 15, 16 * i + 8)
//     }
//   }.otherwise {
//     (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//       shuffSrcB(2 * i) := io.macReqSrcB(8 * i + 7, 8 * i)
//       shuffSrcB(2 * i + 1) := io.macReqSrcB(8 * mxuMeshCols * mxuTileCols + 8 * i + 7, 8 * mxuMeshCols * mxuTileCols + 8 * i)
//       shuffSrcC(2 * i) := io.macReqSrcA(8 * i + 7, 8 * i)
//       shuffSrcC(2 * i + 1) := io.macReqSrcA(8 * mxuMeshCols * mxuTileCols + 8 * i + 7, 8 * mxuMeshCols * mxuTileCols + 8 * i)
//     }
//   }
//
//   val muxSrcB = Mux(!rs2_is_vec, shuffSrcB.asUInt, Mux(io.macReqUop.m_sidx === 0.U && is_transpose, shuffSrcB.asUInt & Fill(128,1.U), Fill(8,0x0.U(16.W))))
//   val muxSrcC = Mux(!rs1_is_vec, shuffSrcB.asUInt, Mux(io.macReqUop.m_sidx === 0.U && is_transpose, shuffSrcC.asUInt& Fill(128,1.U), 0.U))
//   for (c <- 0 until mxuMeshCols) {
//     mesh.io.macReqSrcB(c) := ShiftRegister(muxSrcB(16 * mxuTileCols * (c + 1) - 1, 16 * mxuTileCols * c), c, true.B)
//     mesh.io.macReqSrcC(c) := ShiftRegister(muxSrcC(16 * mxuTileCols * (c + 1) - 1, 16 * mxuTileCols * c), c, true.B)
//   }
//
//   // ready control
//   io.macReq.ready := true.B
//   io.clrReq.ready := true.B
//   for (w <- 0 until numVLdPorts) {
//     io.rowWriteReq(w).ready := true.B
//     io.colWriteReq(w).ready := true.B
//   }
//   io.rowReadReq.ready := (rowReadState === sliceReady)
//   io.colReadReq.ready := (colReadState === sliceReady)
//   // output control
//   io.macResp := mesh.io.macResp
//   io.macRespUop := Pipe(macReqFire, io.macReqUop, mxuMeshRows + mxuMeshCols - 1).bits
//   io.clrResp := mesh.io.clrResp
//   io.clrRespUop := Pipe(clrReqFire, io.clrReqUop, mxuMeshRows).bits
//   for (w <- 0 until numVLdPorts) {
//     io.rowWriteResp(w) := mesh.io.rowWriteResp(w)
//     io.rowWriteRespUop(w) := Pipe(io.rowWriteReq(w).valid, io.rowWriteReqUop(w), mxuMeshRows + 2).bits
//     io.colWriteResp(w) := mesh.io.colWriteResp(w)
//     io.colWriteRespUop(w) := Pipe(io.colWriteReq(w).valid, io.colWriteReqUop(w), mxuMeshCols + 2).bits
//   }
//   io.rowReadResp := mesh.io.rowReadResp(0)
//   io.colReadResp := mesh.io.colReadResp(0)
//
//   //------------------------------------------------------------
//   // read response, m-pipeline
//   val rowRespCtrls = mesh.io.rowReadResp(0).bits
//   val rowReadDataMux = WireInit(VecInit(Seq.fill(mxuMeshCols * mxuTileCols * 2)(0.U(8.W))))
//   when(rowRespCtrls.sew === 0.U) {
//     (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//       rowReadDataMux(i) := mesh.io.rowReadData(0)(64 * i + 7, 64 * i)
//       rowReadDataMux(mxuMeshCols * mxuTileCols + i) := mesh.io.rowReadData(0)(64 * i + 39, 64 * i + 32)
//     }
//   }.elsewhen(rowRespCtrls.sew === 1.U) {
//     when(rowVregIdx === 0.U) {
//       (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//         rowReadDataMux(2 * i) := mesh.io.rowReadData(0)(64 * i + 7, 64 * i)
//         rowReadDataMux(2 * i + 1) := mesh.io.rowReadData(0)(64 * i + 15, 64 * i + 8)
//       }
//     }.otherwise {
//       (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//         rowReadDataMux(2 * i) := mesh.io.rowReadData(0)(64 * i + 39, 64 * i + 32)
//         rowReadDataMux(2 * i + 1) := mesh.io.rowReadData(0)(64 * i + 47, 64 * i + 40)
//       }
//     }
//   }.otherwise {
//     when(rowVregIdx === 0.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         rowReadDataMux(4 * i) := mesh.io.rowReadData(0)(64 * i + 7, 64 * i)
//         rowReadDataMux(4 * i + 1) := mesh.io.rowReadData(0)(64 * i + 15, 64 * i + 8)
//         rowReadDataMux(4 * i + 2) := mesh.io.rowReadData(0)(64 * i + 23, 64 * i + 16)
//         rowReadDataMux(4 * i + 3) := mesh.io.rowReadData(0)(64 * i + 31, 64 * i + 24)
//       }
//     }.elsewhen(rowVregIdx === 1.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         rowReadDataMux(4 * i) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 7, 32 * mxuMeshCols * mxuTileCols + 64 * i)
//         rowReadDataMux(4 * i + 1) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 15, 32 * mxuMeshCols * mxuTileCols + 64 * i + 8)
//         rowReadDataMux(4 * i + 2) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 23, 32 * mxuMeshCols * mxuTileCols + 64 * i + 16)
//         rowReadDataMux(4 * i + 3) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 31, 32 * mxuMeshCols * mxuTileCols + 64 * i + 24)
//       }
//     }.elsewhen(rowVregIdx === 2.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         rowReadDataMux(4 * i) := mesh.io.rowReadData(0)(64 * i + 39, 64 * i + 32)
//         rowReadDataMux(4 * i + 1) := mesh.io.rowReadData(0)(64 * i + 47, 64 * i + 40)
//         rowReadDataMux(4 * i + 2) := mesh.io.rowReadData(0)(64 * i + 55, 64 * i + 48)
//         rowReadDataMux(4 * i + 3) := mesh.io.rowReadData(0)(64 * i + 63, 64 * i + 56)
//       }
//     }.otherwise {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         rowReadDataMux(4 * i) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 39, 32 * mxuMeshCols * mxuTileCols + 64 * i + 32)
//         rowReadDataMux(4 * i + 1) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 47, 32 * mxuMeshCols * mxuTileCols + 64 * i + 40)
//         rowReadDataMux(4 * i + 2) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 55, 32 * mxuMeshCols * mxuTileCols + 64 * i + 48)
//         rowReadDataMux(4 * i + 3) := mesh.io.rowReadData(0)(32 * mxuMeshCols * mxuTileCols + 64 * i + 63, 32 * mxuMeshCols * mxuTileCols + 64 * i + 56)
//       }
//     }
//   }
//
//   val colRespCtrls = mesh.io.colReadResp(0).bits
//   val colReadDataMux = WireInit(VecInit(Seq.fill(mxuMeshRows * mxuTileRows * 2)(0.U(8.W))))
//   when(colRespCtrls.sew === 0.U) {
//     (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//       colReadDataMux(i) := mesh.io.colReadData(0)(32 * i + 7, 32 * i)
//     }
//   }.elsewhen(colRespCtrls.sew === 1.U) {
//     (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//       colReadDataMux(2 * i) := mesh.io.colReadData(0)(32 * i + 7, 32 * i)
//       colReadDataMux(2 * i + 1) := mesh.io.colReadData(0)(32 * i + 15, 32 * i + 8)
//     }
//   }.otherwise {
//     when(colVregIdx === 0.U) {
//       (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//         colReadDataMux(4 * i) := mesh.io.colReadData(0)(32 * i + 7, 32 * i)
//         colReadDataMux(4 * i + 1) := mesh.io.colReadData(0)(32 * i + 15, 32 * i + 8)
//         colReadDataMux(4 * i + 2) := mesh.io.colReadData(0)(32 * i + 23, 32 * i + 16)
//         colReadDataMux(4 * i + 3) := mesh.io.colReadData(0)(32 * i + 31, 32 * i + 24)
//       }
//     }.otherwise {
//       (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//         colReadDataMux(4 * i) := mesh.io.colReadData(0)(16 * mxuMeshRows * mxuTileRows + 32 * i + 7, 16 * mxuMeshRows * mxuTileRows + 32 * i)
//         colReadDataMux(4 * i + 1) := mesh.io.colReadData(0)(16 * mxuMeshRows * mxuTileRows + 32 * i + 15, 16 * mxuMeshRows * mxuTileRows + 32 * i + 8)
//         colReadDataMux(4 * i + 2) := mesh.io.colReadData(0)(16 * mxuMeshRows * mxuTileRows + 32 * i + 23, 16 * mxuMeshRows * mxuTileRows + 32 * i + 16)
//         colReadDataMux(4 * i + 3) := mesh.io.colReadData(0)(16 * mxuMeshRows * mxuTileRows + 32 * i + 31, 16 * mxuMeshRows * mxuTileRows + 32 * i + 24)
//       }
//     }
//   }
//
//   // mask generation:
//   val rowVregOff = rowVregIdx << (rLenbSz.U - io.rowReadRespUop.ts1_eew)
//   val rowMask = Cat((0 until rLenb).map(i => i.U + rowVregOff < io.rowReadRespUop.m_slice_len).reverse)
//   val rowByteMask = Mux1H(UIntToOH(io.rowReadRespUop.ts1_eew),
//     Seq(rowMask,
//       FillInterleaved(2, rowMask(rLen / 16 - 1, 0)),
//       FillInterleaved(4, rowMask(rLen / 32 - 1, 0)),
//       FillInterleaved(8, rowMask(rLen / 64 - 1, 0))))
//   val colVregOff = colVregIdx << (rLenbSz.U - io.colReadRespUop.ts1_eew)
//   val colMask = Cat((0 until rLenb).map(i => i.U + colVregOff < io.colReadRespUop.m_slice_len).reverse)
//   val colByteMask = Mux1H(UIntToOH(io.colReadRespUop.ts1_eew),
//     Seq(colMask,
//       FillInterleaved(2, colMask(rLen / 16 - 1, 0)),
//       FillInterleaved(4, colMask(rLen / 32 - 1, 0)),
//       FillInterleaved(8, colMask(rLen / 64 - 1, 0))))
//
//   io.rowReadData.valid := mesh.io.rowReadResp(0).valid || (rowReadState === sliceResp) || trRowValid || (rowReadState === trSliceResp)
//   io.rowReadData.bits := Mux(trRowValid || (rowReadState === trSliceResp), trRowData, rowReadDataMux.asUInt)
//   io.rowReadMask := rowByteMask
//
//   io.colReadData.valid := mesh.io.colReadResp(0).valid || (colReadState === sliceResp) || trColValid || (colReadState === trSliceResp)
//   io.colReadData.bits := Mux(trColValid || (colReadState === trSliceResp), trColData, colReadDataMux.asUInt)
//   io.colReadMask := colByteMask
//
//   val pipeRowReadUop = Pipe(io.rowReadReq.valid, io.rowReadReqUop, mxuMeshRows + 2).bits
//   io.rowReadRespUop := pipeRowReadUop
//   io.rowReadRespUop.m_split_last := Mux(pipeRowReadUop.uopc.isOneOf(uopMMV_V),pipeRowReadUop.m_split_last,rowVregIdx === rowVsCount)
//   io.rowReadRespUop.v_split_last := rowVregIdx === rowVsCount
//   io.rowReadRespUop.pdst := Mux(pipeRowReadUop.uopc.isOneOf(uopMMV_V), pipeRowReadUop.pdst,pipeRowReadUop.pvd(rowVregIdx).bits)
//   io.rowReadRespUop.stale_pdst := pipeRowReadUop.stale_pvd(rowVregIdx).bits
//
//   val pipeColReadUop = Pipe(io.colReadReq.valid, io.colReadReqUop, mxuMeshCols + 2).bits
//   io.colReadRespUop := pipeColReadUop
//   io.colReadRespUop.m_split_last := colVregIdx === colVsCount
//   io.colReadRespUop.v_split_last := colVregIdx === colVsCount
//   io.colReadRespUop.pdst := pipeColReadUop.pvd(colVregIdx).bits
//   io.colReadRespUop.stale_pdst := pipeColReadUop.stale_pvd(colVregIdx).bits
//
//   //------------------------------------------------------------
//   // read response, lsu
//   val accRowReadCtrls = Pipe(io.accReadReq, mxuMeshRows + 1)
//   val accRowReadData = WireInit(VecInit(Seq.fill(mxuMeshCols * mxuTileCols * 2)(0.U(8.W))))
//
//   val accRowReadSew = accRowReadCtrls.bits.sCtrls.sew
//   val accRowReadQuad = accRowReadCtrls.bits.quad
//   when(accRowReadSew === 0.U) {
//     (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//       accRowReadData(i) := mesh.io.rowReadData(1)(64 * i + 7, 64 * i)
//       accRowReadData(mxuMeshCols * mxuTileCols + i) := mesh.io.rowReadData(1)(64 * i + 39, 64 * i + 32)
//     }
//   }.elsewhen(accRowReadSew === 1.U) {
//     when(accRowReadQuad === 0.U) {
//       (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//         accRowReadData(2 * i) := mesh.io.rowReadData(1)(64 * i + 7, 64 * i)
//         accRowReadData(2 * i + 1) := mesh.io.rowReadData(1)(64 * i + 15, 64 * i + 8)
//       }
//     }.otherwise {
//       (0 until mxuMeshCols * mxuTileCols).foreach { i =>
//         accRowReadData(2 * i) := mesh.io.rowReadData(1)(64 * i + 39, 64 * i + 32)
//         accRowReadData(2 * i + 1) := mesh.io.rowReadData(1)(64 * i + 47, 64 * i + 40)
//       }
//     }
//   }.otherwise {
//     when(accRowReadQuad === 0.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         accRowReadData(4 * i) := mesh.io.rowReadData(1)(64 * i + 7, 64 * i)
//         accRowReadData(4 * i + 1) := mesh.io.rowReadData(1)(64 * i + 15, 64 * i + 8)
//         accRowReadData(4 * i + 2) := mesh.io.rowReadData(1)(64 * i + 23, 64 * i + 16)
//         accRowReadData(4 * i + 3) := mesh.io.rowReadData(1)(64 * i + 31, 64 * i + 24)
//       }
//     }.elsewhen(accRowReadQuad === 1.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         accRowReadData(4 * i) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 7, 32 * mxuMeshCols * mxuTileCols + 64 * i)
//         accRowReadData(4 * i + 1) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 15, 32 * mxuMeshCols * mxuTileCols + 64 * i + 8)
//         accRowReadData(4 * i + 2) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 23, 32 * mxuMeshCols * mxuTileCols + 64 * i + 16)
//         accRowReadData(4 * i + 3) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 31, 32 * mxuMeshCols * mxuTileCols + 64 * i + 24)
//       }
//     }.elsewhen(accRowReadQuad === 2.U) {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         accRowReadData(4 * i) := mesh.io.rowReadData(1)(64 * i + 39, 64 * i + 32)
//         accRowReadData(4 * i + 1) := mesh.io.rowReadData(1)(64 * i + 47, 64 * i + 40)
//         accRowReadData(4 * i + 2) := mesh.io.rowReadData(1)(64 * i + 55, 64 * i + 48)
//         accRowReadData(4 * i + 3) := mesh.io.rowReadData(1)(64 * i + 63, 64 * i + 56)
//       }
//     }.otherwise {
//       (0 until mxuMeshCols * mxuTileCols / 2).foreach { i =>
//         accRowReadData(4 * i) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 39, 32 * mxuMeshCols * mxuTileCols + 64 * i + 32)
//         accRowReadData(4 * i + 1) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 47, 32 * mxuMeshCols * mxuTileCols + 64 * i + 40)
//         accRowReadData(4 * i + 2) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 55, 32 * mxuMeshCols * mxuTileCols + 64 * i + 48)
//         accRowReadData(4 * i + 3) := mesh.io.rowReadData(1)(32 * mxuMeshCols * mxuTileCols + 64 * i + 63, 32 * mxuMeshCols * mxuTileCols + 64 * i + 56)
//       }
//     }
//   }
//
//   val accColReadCtrls = Pipe(io.accReadReq, mxuMeshCols + 1)
//   val accColReadData = WireInit(VecInit(Seq.fill(mxuMeshRows * mxuTileRows * 2)(0.U(8.W))))
//
//   val accColReadSew = accColReadCtrls.bits.sCtrls.sew
//   val accColReadQuad = accColReadCtrls.bits.quad
//   when(accColReadSew === 0.U) {
//     (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//       accColReadData(i) := mesh.io.colReadData(1)(32 * i + 7, 32 * i)
//     }
//   }.elsewhen(accColReadSew === 1.U) {
//     (0 until mxuMeshRows * mxuTileRows).foreach { i =>
//       accColReadData(2 * i) := mesh.io.colReadData(1)(32 * i + 7, 32 * i)
//       accColReadData(2 * i + 1) := mesh.io.colReadData(1)(32 * i + 15, 32 * i + 8)
//     }
//   }.otherwise {
//     when(accColReadQuad === 0.U) {
//       (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//         accColReadData(4 * i) := mesh.io.colReadData(1)(32 * i + 7, 32 * i)
//         accColReadData(4 * i + 1) := mesh.io.colReadData(1)(32 * i + 15, 32 * i + 8)
//         accColReadData(4 * i + 2) := mesh.io.colReadData(1)(32 * i + 23, 32 * i + 16)
//         accColReadData(4 * i + 3) := mesh.io.colReadData(1)(32 * i + 31, 32 * i + 24)
//       }
//     }.otherwise {
//       (0 until mxuMeshRows * mxuTileRows / 2).foreach { i =>
//         accColReadData(4 * i) := mesh.io.colReadData(1)(16 * mxuMeshRows * mxuTileRows + 32 * i + 7, 16 * mxuMeshRows * mxuTileRows + 32 * i)
//         accColReadData(4 * i + 1) := mesh.io.colReadData(1)(16 * mxuMeshRows * mxuTileRows + 32 * i + 15, 16 * mxuMeshRows * mxuTileRows + 32 * i + 8)
//         accColReadData(4 * i + 2) := mesh.io.colReadData(1)(16 * mxuMeshRows * mxuTileRows + 32 * i + 23, 16 * mxuMeshRows * mxuTileRows + 32 * i + 16)
//         accColReadData(4 * i + 3) := mesh.io.colReadData(1)(16 * mxuMeshRows * mxuTileRows + 32 * i + 31, 16 * mxuMeshRows * mxuTileRows + 32 * i + 24)
//       }
//     }
//   }
//
//   // align acc read latency
//   val pipeAccReadCtrls = WireInit(accRowReadCtrls)
//   val pipeAccRowReadData = WireInit(accRowReadData)
//   val pipeAccColReadData = WireInit(accColReadData)
//   if (mxuMeshCols > mxuMeshRows) {
//     pipeAccReadCtrls := accColReadCtrls
//     pipeAccRowReadData := ShiftRegister(accRowReadData, mxuMeshCols - mxuMeshRows, true.B)
//   } else if (mxuMeshCols < mxuMeshRows) {
//     pipeAccReadCtrls := accRowReadCtrls
//     pipeAccColReadData := ShiftRegister(accColReadData, mxuMeshRows - mxuMeshCols, true.B)
//   }
//
//   io.accReadResp.valid := pipeAccReadCtrls.valid
//   io.accReadResp.bits.data := Mux(pipeAccReadCtrls.valid && pipeAccReadCtrls.bits.tt.asBool, pipeAccColReadData.asUInt, pipeAccRowReadData.asUInt)
//   io.accReadResp.bits.vstq_idx := pipeAccReadCtrls.bits.vstq_idx
// }
