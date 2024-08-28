package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import utils._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._

class VLSUXcpt extends Bundle {
  val exception_vld = Bool()
  val update_vl     = Bool()
  val update_data   = UInt(bVL.W)
  val xcpt_cause    = new HellaCacheExceptions()
  val xcpt_addr     = UInt(addrWidth.W)
}

class LsuOutput extends Bundle {
  val data        = UInt(VLEN.W)
  val rfWriteEn   = Bool()
  val rfWriteMask = UInt((VLEN / 8).W)
  val rfWriteIdx  = UInt(5.W)
  val muopEnd     = Bool()
  val isSegLoad   = Bool()
  val regStartIdx = UInt(5.W)
  val regCount    = UInt(4.W) // 2~8
  val xcpt        = new VLSUXcpt
}

class LdstIO(
    implicit p: Parameters
) extends ParameterizedBundle()(p) {
  val mUop          = Input(ValidIO(new Muop()(p)))
  val mUopMergeAttr = Input(ValidIO(new MuopMergeAttr))
  val lsuOut        = Output(ValidIO(new LsuOutput))
  val dataExchange  = new RVUMemory()
  val lsuReady      = Output(Bool())
}

class LSUReq(
    implicit p: Parameters
) extends ParameterizedBundle()(p) {
  val ldstCtrl = new LSULdstCtrl
  val muopInfo = new mUopInfo
  val vstart   = UInt(bVstart.W)
  val vl       = UInt(bVL.W)
  val uopIdx   = UInt(6.W)
  val uopEnd   = Bool()

}

class LSUIO(
    implicit p: Parameters
) extends ParameterizedBundle()(p) {
  val lsuReq       = Flipped(DecoupledIO(new LSUReq()(p)))
  val lsuOut       = Output(ValidIO(new LsuOutput))
  val dataExchange = new RVUMemory()
}

object VRegSegmentStatus {
  //     0          1          2           3          4       5
  val invalid :: srcData :: needLdst :: notReady :: ready :: xcpt :: Nil = Enum(6)
}

object Mop {
  val unit_stride     = "b0001".U
  val index_unodered  = "b0010".U
  val constant_stride = "b0100".U
  val index_ordered   = "b1000".U
}

object UnitStrideMop {
  val not_unit_strde   = "b11111".U
  val unit_stride      = "b00000".U
  val whole_register   = "b01000".U
  val mask             = "b01011".U
  val fault_only_first = "b10000".U
}

object VMemCmd {
  val read  = false.B
  val write = true.B
}

object LdstUopStatus {
  val notReady = 0.U
  val ready    = 1.U
}

class HLSUMeta extends Bundle {
  val valid         = Bool()
  val ldstCtrl      = new LSULdstCtrl
  val muopInfo      = new mUopInfo
  val vregDataVec   = Vec(vlenb, UInt(8.W))
  val vregDataValid = Vec(vlenb, Bool())
  val zeroStride    = Bool()
  val negStride     = Bool()
  val log2Stride    = UInt(log2Ceil(dataWidth / 8).W)
  val xcpt          = new LdstXcpt()
  val xcptVl        = UInt(bVL.W)
}

class CommitInfoRecorded extends Bundle {
  val muopEnd     = Bool()
  val regCount    = UInt(4.W)
  val regStartIdx = UInt(5.W)
  val rfWriteEn   = Bool()
  val rfWriteIdx  = UInt(5.W)
  val isFof       = Bool()
  val xcpt        = new LdstXcpt()
}

class LdstXcpt extends Bundle {
  val xcptValid = Bool()
  val ma        = Bool()
  val pf        = Bool()
  val ae        = Bool()
  val gf        = Bool()

  def generateHellaXcpt(isStore: Bool): HellaCacheExceptions = {
    val xcpt = Wire(new HellaCacheExceptions)
    xcpt.ma.ld := ma & !isStore
    xcpt.ma.st := ma & isStore
    xcpt.pf.ld := pf & !isStore
    xcpt.pf.st := pf & isStore
    xcpt.ae.ld := ae & !isStore
    xcpt.ae.st := ae & isStore
    xcpt.gf.ld := gf & !isStore
    xcpt.gf.st := gf & isStore
    xcpt
  }

  def fromHellaXcpt(xcpt: HellaCacheExceptions): Unit = {
    ma := xcpt.ma.ld | xcpt.ma.st
    pf := xcpt.pf.ld | xcpt.pf.st
    ae := xcpt.ae.ld | xcpt.ae.st
    gf := xcpt.gf.ld | xcpt.gf.st
  }
}

class LdstUop extends Bundle {
  val valid        = Bool()
  val status       = UInt(1.W)                         // ready to commit?
  val memOp        = Bool()                            // load or store
  val size         = UInt(log2Ceil(dataWidth / 8).W)   // element size
  val addr         = UInt(addrWidth.W)
  val pos          = UInt(bVL.W)                       // position in vl
  val destElem     = UInt(bVL.W)                       // data position in vreg
  val destVRegEnd  = Bool()
  val elemCnt      = UInt((log2Ceil(dataBytes) + 1).W) // 1~8
  val xcptValid    = Bool()
  val addrMisalign = Bool()
  val metaPtr  = UInt(nHLsuMetaWidth.W)
}

class SegLdstUop extends Bundle {
  val valid      = Bool()
  val status     = UInt(1.W)                       // ready to commit?
  val memOp      = Bool()                          // load or store
  val masked     = Bool()                          // masked load
  val size       = UInt(log2Ceil(dataWidth / 8).W) // element size
  val addr       = UInt(addrWidth.W)
  val pos        = UInt(bVL.W)                     // position in vl
  val destElem   = UInt(bVL.W)                     // data position in vreg
  val data       = UInt(dataWidth.W)
  val commitInfo = new CommitInfoRecorded()
}

class mUopInfo extends Bundle {
  val uopIdx        = UInt(6.W)
  val uopEnd        = Bool()
  val segIdx        = UInt(log2Ceil(8).W)
  val destVRegStart = Bool()
  val destVRegEnd   = Bool()

  val rs1Val = UInt(XLEN.W)
  val rs2Val = UInt(XLEN.W)
  val vs2    = UInt(VLEN.W)
  val old_vd = UInt(VLEN.W)
  val mask   = UInt(VLEN.W)

  // merge attr
  val muopEnd   = Bool()
  val rfWriteEn = Bool()
  val ldest     = UInt(5.W)
  val regCount  = UInt(4.W)
  val regDstIdx = UInt(5.W)

}

object mUopInfoSelecter {
  def apply(mUop: Muop, mUopMergeAttr: MuopMergeAttr): mUopInfo = {
    val info = Wire(new mUopInfo)

    info.uopIdx        := mUop.uop.uopIdx
    info.uopEnd        := mUop.uop.uopEnd
    info.segIdx        := mUop.uop.segIndex
    info.destVRegStart := mUop.uop.destVRegStart
    info.destVRegEnd   := mUop.uop.destVRegEnd

    info.rs1Val := mUop.scalar_opnd_1
    info.rs2Val := mUop.scalar_opnd_2
    info.vs2    := mUop.uopRegInfo.vs2
    info.old_vd := mUop.uopRegInfo.old_vd
    info.mask   := mUop.uopRegInfo.mask

    info.muopEnd   := mUopMergeAttr.muopEnd
    info.rfWriteEn := mUopMergeAttr.rfWriteEn
    info.ldest     := mUopMergeAttr.ldest
    info.regCount  := mUopMergeAttr.regCount
    info.regDstIdx := mUopMergeAttr.regDstIdx

    info
  }
}

class LSULdstCtrl extends Bundle {
  val isLoad   = Bool()
  val isStore  = Bool()
  val nfield   = UInt(4.W) // 1~8
  val ldstType = UInt(4.W)
  val unitSMop = UInt(5.W)

  val ma = Bool()
  val ta = Bool()

  val vm        = Bool()
  val memwb     = UInt(4.W)
  val eewb      = UInt(4.W)
  val log2Memwb = UInt(2.W)
  val log2Eewb  = UInt(2.W)

  val mlen       = UInt(vlenbWidth.W)
  val elen       = UInt(vlenbWidth.W)
  val minLen     = UInt(vlenbWidth.W)
  val log2Mlen   = UInt(log2Ceil(vlenbWidth).W)
  val log2Elen   = UInt(log2Ceil(vlenbWidth).W)
  val log2MinLen = UInt(log2Ceil(vlenbWidth).W)
}

object LSULdstDecoder {
  def apply(mUop: Muop, mUopMergeAttr: MuopMergeAttr): LSULdstCtrl = {
    val (funct6, funct3) = (mUop.uop.ctrl.funct6, mUop.uop.ctrl.funct3)
    val (vsew, vm)       = (mUop.uop.info.vsew, mUop.uop.ctrl.vm)
    val unitStrdeMop     = mUop.uop.ctrl.vs2

    val nf   = funct6(5, 3)
    val mop  = funct6(1, 0)
    val ctrl = Wire(new LSULdstCtrl)

    ctrl.isLoad  := mUop.uop.ctrl.load
    ctrl.isStore := mUop.uop.ctrl.store

    ctrl.ma := mUop.uop.info.ma
    ctrl.ta := mUop.uop.info.ta

    ctrl.ldstType := MuxLookup(
      funct6(1, 0),
      Mop.unit_stride,
      Seq(
        "b00".U -> Mop.unit_stride,
        "b01".U -> Mop.index_unodered,
        "b10".U -> Mop.constant_stride,
        "b11".U -> Mop.index_ordered,
      ),
    )
    ctrl.unitSMop := Mux(mop === "b00".U, unitStrdeMop, UnitStrideMop.not_unit_strde)
    // eew and sew in bytes calculation
    val sewb = 1.U << vsew
    ctrl.eewb := 1.U << funct3(1, 0)

    ctrl.memwb  := Mux(ctrl.ldstType === Mop.index_ordered || ctrl.ldstType === Mop.index_unodered, sewb, ctrl.eewb)
    ctrl.nfield := Mux(ctrl.unitSMop === UnitStrideMop.whole_register, 1.U, nf +& 1.U)
    ctrl.vm     := vm
    // 1->0, 2->1, 4->2, 8->3
    ctrl.log2Memwb := Mux(
      ctrl.ldstType === Mop.index_ordered || ctrl.ldstType === Mop.index_unodered,
      vsew,
      funct3(1, 0),
    )
    ctrl.log2Eewb   := funct3(1, 0)
    ctrl.mlen       := vlenb.U >> ctrl.log2Memwb
    ctrl.elen       := vlenb.U >> ctrl.log2Eewb
    ctrl.log2Mlen   := Mux1H(ctrl.mlen, Seq(0.U, 1.U, 2.U, 3.U, 4.U))
    ctrl.log2Elen   := Mux1H(ctrl.elen, Seq(0.U, 1.U, 2.U, 3.U, 4.U))
    ctrl.minLen     := ctrl.elen min ctrl.mlen
    ctrl.log2MinLen := ctrl.log2Elen min ctrl.log2Mlen

    ctrl
  }
}

object AddrUtil {
  val addrOffsetHighIdx = log2Ceil(dataWidth / 8) - 1

  def isAddrMisalign(addr: UInt, size: UInt): Bool = {
    val mask = (1.U << size) - 1.U
    (addr & mask) =/= 0.U
  }

  def getAlignedAddr(addr: UInt): UInt =
    Cat(addr(addrWidth - 1, addrOffsetHighIdx + 1), 0.U((addrOffsetHighIdx + 1).W))

  def getAlignedOffset(addr: UInt): UInt =
    addr(addrOffsetHighIdx, 0)
}
