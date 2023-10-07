package darecreek.exu.vfu.fp

import chisel3._
import chisel3.util._
// import darecreek.LaneFUInput
import darecreek.exu.vfu.LaneFUInput
import VFInsts._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.DecodeLogic

trait VFPDecodeConstants {
  val table: Array[(BitPat, List[BitPat])]
  def default: List[BitPat]

  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  def S = BitPat(VFPU.S)
  def D = BitPat(VFPU.D)
  def I = BitPat(VFPU.D) // Invalid
}

class VFMADecode extends VFPDecodeConstants {

  // fmaCmd, negVs1, negVs2, negVd, switchVdVs2
  // fmaCmd: 10->only add/sub, 01->only mul, 11->fma (both add/sub and mul)
  val default: List[BitPat] = List(BitPat("b00"),X,X,X,X)

  // 41 fma ops
  override val table: Array[(BitPat, List[BitPat])] = Array(
    // add/sub/mul
    VFADD_VF -> List(BitPat("b10"),N,N,X,N),
    VFADD_VV -> List(BitPat("b10"),N,N,X,N),
    VFRSUB_VF -> List(BitPat("b10"),N,Y,X,N),
    VFSUB_VF -> List(BitPat("b10"),Y,N,X,N),
    VFSUB_VV -> List(BitPat("b10"),Y,N,X,N),
    VFMUL_VF -> List(BitPat("b01"),N,N,X,N),
    VFMUL_VV -> List(BitPat("b01"),N,N,X,N),
    VFWMUL_VF -> List(BitPat("b01"),N,N,X,N),
    VFWMUL_VV -> List(BitPat("b01"),N,N,X,N),
    VFWADD_VF -> List(BitPat("b10"),N,N,X,N),
    VFWADD_VV -> List(BitPat("b10"),N,N,X,N),
    VFWADD_WF -> List(BitPat("b10"),N,N,X,N),
    VFWADD_WV -> List(BitPat("b10"),N,N,X,N),
    VFWSUB_VF -> List(BitPat("b10"),Y,N,X,N),
    VFWSUB_VV -> List(BitPat("b10"),Y,N,X,N),
    VFWSUB_WF -> List(BitPat("b10"),Y,N,X,N),
    VFWSUB_WV -> List(BitPat("b10"),Y,N,X,N),
    // fma
    VFMACC_VF -> List(BitPat("b11"),N,N,N,N),
    VFMACC_VV -> List(BitPat("b11"),N,N,N,N),
    VFMADD_VF -> List(BitPat("b11"),N,N,N,Y),
    VFMADD_VV -> List(BitPat("b11"),N,N,N,Y),
    VFMSAC_VF -> List(BitPat("b11"),N,N,Y,N),
    VFMSAC_VV -> List(BitPat("b11"),N,N,Y,N),
    VFMSUB_VF -> List(BitPat("b11"),N,Y,N,Y),
    VFMSUB_VV -> List(BitPat("b11"),N,Y,N,Y),
    VFNMACC_VF -> List(BitPat("b11"),Y,N,Y,N),
    VFNMACC_VV -> List(BitPat("b11"),Y,N,Y,N),
    VFNMADD_VF -> List(BitPat("b11"),Y,Y,N,Y),
    VFNMADD_VV -> List(BitPat("b11"),Y,Y,N,Y),
    VFNMSAC_VF -> List(BitPat("b11"),Y,N,N,N),
    VFNMSAC_VV -> List(BitPat("b11"),Y,N,N,N),
    VFNMSUB_VF -> List(BitPat("b11"),Y,N,N,Y),
    VFNMSUB_VV -> List(BitPat("b11"),Y,N,N,Y),
    VFWMACC_VF -> List(BitPat("b11"),N,N,N,N),
    VFWMACC_VV -> List(BitPat("b11"),N,N,N,N),
    VFWMSAC_VF -> List(BitPat("b11"),N,N,Y,N),
    VFWMSAC_VV -> List(BitPat("b11"),N,N,Y,N),
    VFWNMACC_VF -> List(BitPat("b11"),Y,N,Y,N),
    VFWNMACC_VV -> List(BitPat("b11"),Y,N,Y,N),
    VFWNMSAC_VF -> List(BitPat("b11"),Y,N,N,N),
    VFWNMSAC_VV -> List(BitPat("b11"),Y,N,N,N),
  )
}

class VFCvtDecode extends VFPDecodeConstants {
  //  isCvt, cvtSigned ,cvtCmd(i2f,f2i,f2f), rm(rod, rtz)
  val default: List[BitPat] = List(N,X,BitPat("b???"),BitPat("b??"))

  // 21 cvt ops
  override val table: Array[(BitPat, List[BitPat])] = Array(
    // cvt, wcvt, ncvt
    VFCVT_F_X_V          ->      List(Y,Y,BitPat("b100"),BitPat("b00")),
    VFCVT_F_XU_V         ->      List(Y,N,BitPat("b100"),BitPat("b00")),
    VFCVT_RTZ_X_F_V      ->      List(Y,Y,BitPat("b010"),BitPat("b10")),
    VFCVT_RTZ_XU_F_V     ->      List(Y,N,BitPat("b010"),BitPat("b10")),
    VFCVT_X_F_V          ->      List(Y,Y,BitPat("b010"),BitPat("b00")),
    VFCVT_XU_F_V         ->      List(Y,N,BitPat("b010"),BitPat("b00")),
    VFWCVT_F_F_V         ->      List(Y,X,BitPat("b001"),BitPat("b00")),
    VFWCVT_F_X_V         ->      List(Y,Y,BitPat("b100"),BitPat("b00")),
    VFWCVT_F_XU_V        ->      List(Y,N,BitPat("b100"),BitPat("b00")),
    VFWCVT_RTZ_X_F_V     ->      List(Y,Y,BitPat("b010"),BitPat("b10")),
    VFWCVT_RTZ_XU_F_V    ->      List(Y,N,BitPat("b010"),BitPat("b10")),
    VFWCVT_X_F_V         ->      List(Y,Y,BitPat("b010"),BitPat("b00")),
    VFWCVT_XU_F_V        ->      List(Y,N,BitPat("b010"),BitPat("b00")),
    VFNCVT_F_F_W         ->      List(Y,X,BitPat("b001"),BitPat("b00")),
    VFNCVT_F_X_W         ->      List(Y,Y,BitPat("b100"),BitPat("b00")),
    VFNCVT_F_XU_W        ->      List(Y,N,BitPat("b100"),BitPat("b00")),
    VFNCVT_ROD_F_F_W     ->      List(Y,X,BitPat("b001"),BitPat("b01")),
    VFNCVT_RTZ_X_F_W     ->      List(Y,Y,BitPat("b010"),BitPat("b10")),
    VFNCVT_RTZ_XU_F_W    ->      List(Y,N,BitPat("b010"),BitPat("b10")),
    VFNCVT_X_F_W         ->      List(Y,Y,BitPat("b010"),BitPat("b00")),
    VFNCVT_XU_F_W        ->      List(Y,N,BitPat("b010"),BitPat("b00")),
  )
}

class VFMiscDecode extends VFPDecodeConstants {
  //  isMisc, MiscCmd, MiscSubCmd
  //  In fact, these bits can be extracted from inst32 directly
  val default: List[BitPat] = List(N, BitPat("b?????"),BitPat("b???"))

  // 21 misc ops
  override val table: Array[(BitPat, List[BitPat])] = Array(
    VFCLASS_V    ->  List(Y, BitPat("b10000"),BitPat("b???")),
    VFMAX_VF    ->   List(Y, BitPat("b00010"),BitPat("b110")),
    VFMAX_VV    ->   List(Y, BitPat("b00010"),BitPat("b110")),
    VFMIN_VF    ->   List(Y, BitPat("b00010"),BitPat("b100")),
    VFMIN_VV    ->   List(Y, BitPat("b00010"),BitPat("b100")),
    VFSGNJ_VF    ->  List(Y, BitPat("b00100"),BitPat("b000")),
    VFSGNJ_VV    ->  List(Y, BitPat("b00100"),BitPat("b000")),
    VFSGNJN_VF    -> List(Y, BitPat("b00100"),BitPat("b001")),
    VFSGNJN_VV    -> List(Y, BitPat("b00100"),BitPat("b001")),
    VFSGNJX_VF    -> List(Y, BitPat("b00100"),BitPat("b010")),
    VFSGNJX_VV    -> List(Y, BitPat("b00100"),BitPat("b010")),
    VMFEQ_VF    ->   List(Y, BitPat("b01000"),BitPat("b000")),
    VMFEQ_VV    ->   List(Y, BitPat("b01000"),BitPat("b000")),
    VMFGE_VF    ->   List(Y, BitPat("b01000"),BitPat("b111")),
    VMFGT_VF    ->   List(Y, BitPat("b01000"),BitPat("b101")),
    VMFLE_VF    ->   List(Y, BitPat("b01000"),BitPat("b001")),
    VMFLE_VV    ->   List(Y, BitPat("b01000"),BitPat("b001")),
    VMFLT_VF    ->   List(Y, BitPat("b01000"),BitPat("b011")),
    VMFLT_VV    ->   List(Y, BitPat("b01000"),BitPat("b011")),
    VMFNE_VF    ->   List(Y, BitPat("b01000"),BitPat("b100")),
    VMFNE_VV    ->   List(Y, BitPat("b01000"),BitPat("b100")),
    VFMV_V_F      -> List(Y, BitPat("b00001"),BitPat("b???")),
    VFMERGE_VFM   -> List(Y, BitPat("b00001"),BitPat("b???")),
  )
}

class VFRecDecode extends VFPDecodeConstants {
  // isRec7, isRsqrt7,
  val default: List[BitPat] = List(N,N)

  override val table: Array[(BitPat, List[BitPat])] = Array(
    VFREC7_V     ->  List(Y, N),
    VFRSQRT7_V   ->  List(N, Y),
  )
}

class VFDivDecode extends VFPDecodeConstants {
  // isDivSqrt, isSqrt, divReverse
  val default: List[BitPat] = List(N,N,N)

  override val table: Array[(BitPat, List[BitPat])] = Array(
    VFDIV_VF     ->  List(Y, N, N),
    VFDIV_VV     ->  List(Y, N, N),
    VFRDIV_VF    ->  List(Y, N, Y),
    VFSQRT_V     ->  List(Y, Y, N)
  )
}

object VFDecoder {
  def apply(instr: UInt)(implicit p: Parameters): VFDecoder = {
    require(instr.getWidth == 25)
    val d = Module(new VFDecoder)
    d.io.instr := instr
    d
  }
}
class VFDecoder(implicit val p: Parameters) extends VFPUBaseModule {
  val io = IO(new Bundle() {
    val instr = Input(UInt(25.W))
    val fpCtrl = Output(new VFPUCtrlSigs)
  })

  val decoders: UInt = Cat(Seq(
    new VFMADecode,
    new VFCvtDecode,
    new VFMiscDecode,
    new VFRecDecode,
    new VFDivDecode,
  ).flatMap(d => {
    DecodeLogic(io.instr, d.default, d.table)
  }))
  io.fpCtrl <> decoders.asTypeOf(new VFPUCtrlSigs)
}




