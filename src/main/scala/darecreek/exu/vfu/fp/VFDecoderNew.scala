package darecreek.exu.vfu.fp

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
// import darecreek.LaneFUInput
import darecreek.exu.vfu.LaneFUInput
import VFInsts._
import org.chipsalliance.cde.config.Parameters
// import freechips.rocketchip.rocket.DecodeLogic
import chisel3.util.experimental.decode._
import darecreek.exu.vfu._

trait VFPDecode {
  val default: BitPat
  val table: Seq[(BitPat, BitPat)]
}

class VFMADecode extends VFPDecode {

  // fmaCmd, negVs1, negVs2, negVd, switchVdVs2
  // fmaCmd: 10->only add/sub, 01->only mul, 11->fma (both add/sub and mul)
  val default = BitPat("b00 ? ? ? ?")

  // 41 fma ops
  val table = Seq(
    // add/sub/mul
    VFADD_VF -> BitPat("b10 0 0 ? 0"),
    VFADD_VV -> BitPat("b10 0 0 ? 0"),
    VFRSUB_VF -> BitPat("b10 0 1 ? 0"),
    VFSUB_VF -> BitPat("b10 1 0 ? 0"),
    VFSUB_VV -> BitPat("b10 1 0 ? 0"),
    VFMUL_VF -> BitPat("b01 0 0 ? 0"),
    VFMUL_VV -> BitPat("b01 0 0 ? 0"),
    VFWMUL_VF -> BitPat("b01 0 0 ? 0"),
    VFWMUL_VV -> BitPat("b01 0 0 ? 0"),
    VFWADD_VF -> BitPat("b10 0 0 ? 0"),
    VFWADD_VV -> BitPat("b10 0 0 ? 0"),
    VFWADD_WF -> BitPat("b10 0 0 ? 0"),
    VFWADD_WV -> BitPat("b10 0 0 ? 0"),
    VFWSUB_VF -> BitPat("b10 1 0 ? 0"),
    VFWSUB_VV -> BitPat("b10 1 0 ? 0"),
    VFWSUB_WF -> BitPat("b10 1 0 ? 0"),
    VFWSUB_WV -> BitPat("b10 1 0 ? 0"),
    // fma
    VFMACC_VF -> BitPat("b11 0 0 0 0"),
    VFMACC_VV -> BitPat("b11 0 0 0 0"),
    VFMADD_VF -> BitPat("b11 0 0 0 1"),
    VFMADD_VV -> BitPat("b11 0 0 0 1"),
    VFMSAC_VF -> BitPat("b11 0 0 1 0"),
    VFMSAC_VV -> BitPat("b11 0 0 1 0"),
    VFMSUB_VF -> BitPat("b11 0 1 0 1"),
    VFMSUB_VV -> BitPat("b11 0 1 0 1"),
    VFNMACC_VF -> BitPat("b11 1 0 1 0"),
    VFNMACC_VV -> BitPat("b11 1 0 1 0"),
    VFNMADD_VF -> BitPat("b11 1 1 0 1"),
    VFNMADD_VV -> BitPat("b11 1 1 0 1"),
    VFNMSAC_VF -> BitPat("b11 1 0 0 0"),
    VFNMSAC_VV -> BitPat("b11 1 0 0 0"),
    VFNMSUB_VF -> BitPat("b11 1 0 0 1"),
    VFNMSUB_VV -> BitPat("b11 1 0 0 1"),
    VFWMACC_VF -> BitPat("b11 0 0 0 0"),
    VFWMACC_VV -> BitPat("b11 0 0 0 0"),
    VFWMSAC_VF -> BitPat("b11 0 0 1 0"),
    VFWMSAC_VV -> BitPat("b11 0 0 1 0"),
    VFWNMACC_VF -> BitPat("b11 1 0 1 0"),
    VFWNMACC_VV -> BitPat("b11 1 0 1 0"),
    VFWNMSAC_VF -> BitPat("b11 1 0 0 0"),
    VFWNMSAC_VV -> BitPat("b11 1 0 0 0"),
  )
}



class VFCvtDecode extends VFPDecode {
  //  isCvt, cvtSigned ,cvtCmd(i2f,f2i,f2f), rm(rod, rtz)
  val default = BitPat("b0? ??? ??")

  // 21 cvt ops
  val table = Seq(
    // cvt, wcvt, ncvt
    VFCVT_F_X_V          ->      BitPat("b11 100 00"),
    VFCVT_F_XU_V         ->      BitPat("b10 100 00"),
    VFCVT_RTZ_X_F_V      ->      BitPat("b11 010 10"),
    VFCVT_RTZ_XU_F_V     ->      BitPat("b10 010 10"),
    VFCVT_X_F_V          ->      BitPat("b11 010 00"),
    VFCVT_XU_F_V         ->      BitPat("b10 010 00"),
    VFWCVT_F_F_V         ->      BitPat("b1? 001 00"),
    VFWCVT_F_X_V         ->      BitPat("b11 100 00"),
    VFWCVT_F_XU_V        ->      BitPat("b10 100 00"),
    VFWCVT_RTZ_X_F_V     ->      BitPat("b11 010 10"),
    VFWCVT_RTZ_XU_F_V    ->      BitPat("b10 010 10"),
    VFWCVT_X_F_V         ->      BitPat("b11 010 00"),
    VFWCVT_XU_F_V        ->      BitPat("b10 010 00"),
    VFNCVT_F_F_W         ->      BitPat("b1? 001 00"),
    VFNCVT_F_X_W         ->      BitPat("b11 100 00"),
    VFNCVT_F_XU_W        ->      BitPat("b10 100 00"),
    VFNCVT_ROD_F_F_W     ->      BitPat("b1? 001 01"),
    VFNCVT_RTZ_X_F_W     ->      BitPat("b11 010 10"),
    VFNCVT_RTZ_XU_F_W    ->      BitPat("b10 010 10"),
    VFNCVT_X_F_W         ->      BitPat("b11 010 00"),
    VFNCVT_XU_F_W        ->      BitPat("b10 010 00"),
  )
}

class VFMiscDecode extends VFPDecode {
  //  isMisc, MiscCmd, MiscSubCmd
  //  In fact, these bits can be extracted from inst32 directly
  val default = BitPat("b0 ????? ???")

  // 21 misc ops
  val table = Seq(
    VFCLASS_V    ->  BitPat("b1 10000 ???"),
    VFMAX_VF    ->   BitPat("b1 00010 110"),
    VFMAX_VV    ->   BitPat("b1 00010 110"),
    VFMIN_VF    ->   BitPat("b1 00010 100"),
    VFMIN_VV    ->   BitPat("b1 00010 100"),
    VFSGNJ_VF    ->  BitPat("b1 00100 000"),
    VFSGNJ_VV    ->  BitPat("b1 00100 000"),
    VFSGNJN_VF    -> BitPat("b1 00100 001"),
    VFSGNJN_VV    -> BitPat("b1 00100 001"),
    VFSGNJX_VF    -> BitPat("b1 00100 010"),
    VFSGNJX_VV    -> BitPat("b1 00100 010"),
    VMFEQ_VF    ->   BitPat("b1 01000 000"),
    VMFEQ_VV    ->   BitPat("b1 01000 000"),
    VMFGE_VF    ->   BitPat("b1 01000 111"),
    VMFGT_VF    ->   BitPat("b1 01000 101"),
    VMFLE_VF    ->   BitPat("b1 01000 001"),
    VMFLE_VV    ->   BitPat("b1 01000 001"),
    VMFLT_VF    ->   BitPat("b1 01000 011"),
    VMFLT_VV    ->   BitPat("b1 01000 011"),
    VMFNE_VF    ->   BitPat("b1 01000 100"),
    VMFNE_VV    ->   BitPat("b1 01000 100"),
    VFMV_V_F      -> BitPat("b1 00001 ???"),
    VFMERGE_VFM   -> BitPat("b1 00001 ???"),
  )
}

class VFRecDecode extends VFPDecode {
  // isRec7, isRsqrt7,
  val default = BitPat("b0 0")

  val table = Seq(
    VFREC7_V     ->  BitPat("b1 0"),
    VFRSQRT7_V   ->  BitPat("b0 1"),
  )
}

class VFDivDecode extends VFPDecode {
  // isDivSqrt, isSqrt, divReverse
  val default = BitPat("b0 0 0")

  val table = Seq(
    VFDIV_VF     ->  BitPat("b1 0 0"),
    VFDIV_VV     ->  BitPat("b1 0 0"),
    VFRDIV_VF    ->  BitPat("b1 0 1"),
    VFSQRT_V     ->  BitPat("b1 1 0")
  )
}

object VFDecoder {
  def apply(instr: UInt): VFDecoder = {
    require(instr.getWidth == 25)
    val d = Module(new VFDecoder)
    d.io.instr := instr
    d
  }
}
class VFDecoder extends Module {
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
  ).map(d => {
    decoder(QMCMinimizer, io.instr, TruthTable(d.table, d.default))
  }))
  io.fpCtrl <> decoders.asTypeOf(new VFPUCtrlSigs)
}

object Main extends App {
  ChiselStage.emitSystemVerilogFile(
    new VFDecoder,
    Array("--target-dir", "generated"),
  )
}



