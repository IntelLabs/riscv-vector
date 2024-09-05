// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package gpc.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.rocket.CustomInstructions._

abstract trait DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends Bundle {

  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = Bits(A2_X.getWidth.W)
  val sel_alu1 = Bits(A1_X.getWidth.W)
  val sel_imm = Bits(IMM_X.getWidth.W)
  val alu_dw = Bool()
  val alu_fn = Bits(aluFn.FN_X.getWidth.W)
  val mem = Bool()
  val mem_cmd = Bits(M_SZ.W)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(CSR.SZ.W)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()
  val pipe0 = Bool()
  val pipe1 = Bool()
  val alu = Bool()

  def default: List[BitPat] =
                //           jal                                                                 renf1               fence.i
                //   val     | jalr                                                              | renf2             |
                //   | fp_val| | renx2                                                           | | renf3           |
                //   | | rocc| | | renx1       s_alu1                              mem_val       | | | wfd           |
                //   | | | br| | | |   s_alu2  |       imm    dw     alu           | mem_cmd     | | | | mul         |
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | div       | fence  pipe0
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | | wxd     | | amo  | pipe1
                //   | | | | | | | |   |       |       |      |      |             | |           | | | | | | |       | | | dp | | alu
                List(N,X,X,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,   N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X, X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp, pipe0, pipe1, alu)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_XPR,aluFn.FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,Y,Y),

    JAL->       List(Y,N,N,N,Y,N,N,N,A2_IMM, A1_PC,  IMM_UJ,DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,Y),
    JALR->      List(Y,N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,Y),
    AUIPC->     List(Y,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),

    LB->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    LH->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    LW->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    LBU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    LHU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    SB->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    SH->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    SW->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),

    LUI->       List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    ADDI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    ANDI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    ORI->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    XORI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    ADD->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SUB->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLT->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    AND->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    OR->        List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    XOR->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRA->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),

    FENCE->     List(Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N, Y,N,N),

    ECALL->     List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N),
    EBREAK->    List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N),
    MRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N),
    WFI->       List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N, Y,N,N),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N, Y,N,N),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N, Y,N,N),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N, Y,N,N),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N, Y,N,N),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,aluFn.FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N, Y,N,N))
}

// class CeaseDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(
//     CEASE->     List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N))
// }


class FenceIDecode(flushDCache: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N, Y,N,N))
}

// class ConditionalZeroDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(
//     CZERO_EQZ-> List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZEQZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CZERO_NEZ-> List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_CZNEZ, N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N))
// }

// class CFlushDecode(supportsFlushLine: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   private def zapRs1(x: BitPat) = if (supportsFlushLine) x else BitPat(x.value.U)

//   val table: Array[(BitPat, List[BitPat])] = Array(
//     zapRs1(CFLUSH_D_L1)->
//                 List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),
//     zapRs1(CDISCARD_D_L1)->
//                 List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N))
// }

class SVMDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N))
}

class SDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N))
}

// class HypervisorDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(

//     HFENCE_VVMA->List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEV,  N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),
//     HFENCE_GVMA->List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HFENCEG,  N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),

//     HLV_B ->    List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLV_BU->    List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLV_H ->    List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLV_HU->    List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLVX_HU->   List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLV_W->     List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HLVX_WU->   List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_HLVX,     N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),

//     HSV_B->     List(Y,N,N,N,N,N,Y,Y,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),
//     HSV_H->     List(Y,N,N,N,N,N,Y,Y,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),
//     HSV_W->     List(Y,N,N,N,N,N,Y,Y,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N))
// }

class DebugDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, Y,N,N))
}

// class NMIDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(
//     MNRET->     List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N))
// }

class I32Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    Instructions32.SLLI->
                List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N,Y),
    Instructions32.SRLI->
                List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N,Y),
    Instructions32.SRAI->
                List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N,Y))
}

class I64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    LWU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,N,N),
    SD->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),

    SLLI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRLI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRAI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,aluFn.FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N, Y,Y,Y))
}

// class Hypervisor64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(
//     HLV_D->     List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N),
//     HSV_D->     List(Y,N,N,N,N,N,Y,Y,A2_ZERO, A1_RS1, IMM_I, DW_XPR, aluFn.FN_ADD, Y,M_XWR,      N,N,N,N,N,N,N,CSR.I,N,N,N,N, N,N),
//     HLV_WU->    List(Y,N,N,N,N,N,N,Y,A2_ZERO, A1_RS1, IMM_X, DW_XPR, aluFn.FN_ADD, Y,M_XRD,      N,N,N,N,N,N,Y,CSR.I,N,N,N,N, N,N))
// }

class MDecode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, N,Y,N),
    MULH->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, N,Y,N),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, N,Y,N),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, N,Y,N),

    DIV->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    REM->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    REMU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N))
}

class M64Decode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N, N,Y,N),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    REMW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, aluFn.FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N, N,Y,N))
}

class ADecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N))
}

class A64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N, Y,N,N))
}

class HDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_H_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSGNJ_H->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSGNJX_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSGNJN_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMIN_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMAX_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FADD_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSUB_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMUL_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMADD_H->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMSUB_H->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FNMADD_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FNMSUB_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCLASS_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FMV_X_H->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_W_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_WU_H-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FEQ_H->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FLT_H->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FLE_H->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FMV_H_X->   List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_H_W->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_H_WU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FLH->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    FSH->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    FDIV_H->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSQRT_H->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N))
}

class FDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FMV_X_W->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FMV_W_X->   List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FLW->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    FSW->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N, Y,N,N),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N))
}

class DDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FLD->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,aluFn.FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSD->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,aluFn.FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N))
}

class HDDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_D_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_H_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N))
}

class H64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_H->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_LU_H-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_H_L->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_H_LU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N))
}

class F64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,Y,N),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N, N,Y,N))
}

class D64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y, N,Y,N),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  aluFn.FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y, N,Y,N))
}

// class RoCCDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
// {
//   val table: Array[(BitPat, List[BitPat])] = Array(
//     CUSTOM0->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM1->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM2->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM3->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,N,CSR.N,N,N,N,N, N,N),
//     CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N),
//     CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,aluFn.FN_ADD,   N,M_X,N,N,N,N,N,N,Y,CSR.N,N,N,N,N, N,N))
// }
