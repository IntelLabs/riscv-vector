package smartVector.lsutest

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VFuParamsKey
import darecreek.exu.vfu.VFuParameters
import xiangshan.XSCoreParamsKey
import xiangshan.XSCoreParameters
import smartVector._
import SmartParam._

case class CtrlBundle(
    instrn:        BitPat,
    isLoad:        Boolean = true,
    vm:            Boolean = true,
    ma:            Boolean = false,
    ta:            Boolean = false,
    vsew:          Int = 0,
    vlmul:         Int = 0,
    vl:            Int = 32,
    vstart:        Int = 0,
    uopIdx:        Int = 0,
    uopEnd:        Boolean = false,
    segIdx:        Int = 0,
    vs2:           Int = 0,
    destVRegStart: Boolean = false,
    destVRegEnd:   Boolean = false,
)

case class SrcBundleLd(
    scalar_opnd_1: String = "h1000",
    scalar_opnd_2: String = "h0",
    vs1:           String = "hc0bfbebdbcbbbab9_b8b7b6b5b4b3b2b1",
    vs2:           String = "h0000000000000000_0000000000000000",
    oldVd:         String = "h201f1e1d1c1b1a19_1817161514131211",
    mask:          String = "h0",
    //  mask: String = "hffff_ffff_ffff_ffff",
)

case class SrcBundleSt(
    scalar_opnd_1: String = "h1000",
    scalar_opnd_2: String = "h0",
    vs1:           String = "hc0bfbebdbcbbbab9_b8b7b6b5b4b3b2b1",
    vs2:           String = "h0000000000000000_0000000000000000",
    vs3:           String = "h201f1e1d1c1b1a19_1817161514131211",
    mask:          String = "h0",
    //  mask: String = "hffff_ffff_ffff_ffff",
)

trait BundleGenHelper {
  def genUop(c: CtrlBundle) =
    (new VUopTest).Lit(
      _.ctrl_funct6 -> {
        if (c.instrn(31, 29).equals(BitPat("b???"))) {
          BitPat.bitPatToUInt(c.instrn(28, 26))
        } else {
          BitPat.bitPatToUInt(c.instrn(31, 26))
        }
      },
      _.ctrl_funct3   -> BitPat.bitPatToUInt(c.instrn(14, 12)),
      _.ctrl_load     -> c.isLoad.B,
      _.ctrl_store    -> (!c.isLoad).B,
      _.ctrl_vm       -> c.vm.B,
      _.info_ma       -> c.ma.B,
      _.info_ta       -> c.ta.B,
      _.info_vsew     -> c.vsew.U,
      _.info_vlmul    -> c.vlmul.U,
      _.info_vl       -> c.vl.U,
      _.info_vstart   -> c.vstart.U,
      _.splitUopIdx   -> c.uopIdx.U,
      _.splitUopEnd   -> c.uopEnd.B,
      _.segIdx        -> c.segIdx.U,
      _.destVRegStart -> c.destVRegStart.B,
      _.destVRegEnd   -> c.destVRegEnd.B,
      _.ctrl_vs2 -> {
        if (c.instrn(24, 20).equals(BitPat("b?????"))) {
          0.U
        } else {
          BitPat.bitPatToUInt(c.instrn(24, 20))
        }
      },
    )

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) =
    (new MuopTest).Lit(
      _.uop           -> genUop(c),
      _.scalar_opnd_1 -> s.scalar_opnd_1.U,
      _.scalar_opnd_2 -> s.scalar_opnd_2.U,
      _.vs1           -> s.vs1.U,
      _.vs2           -> s.vs2.U,
      _.oldVd         -> s.oldVd.U,
      _.mask          -> s.mask.U,
    )

  def genStInput(c: CtrlBundle, s: SrcBundleSt) =
    (new MuopTest).Lit(
      _.uop           -> genUop(c),
      _.scalar_opnd_1 -> s.scalar_opnd_1.U,
      _.scalar_opnd_2 -> s.scalar_opnd_2.U,
      _.vs1           -> s.vs1.U,
      _.vs2           -> s.vs2.U,
      _.oldVd         -> s.vs3.U,
      _.mask          -> s.mask.U,
    )
}

class VUopTest extends Bundle {
  val ctrl_vs2      = UInt(5.W)
  val ctrl_funct6   = UInt(6.W)
  val ctrl_funct3   = UInt(3.W)
  val ctrl_load     = Bool()
  val ctrl_store    = Bool()
  val ctrl_vm       = Bool()
  val info_ma       = Bool() // vector mask agnostic, data unknown or undisturbed
  val info_ta       = Bool() // vector tail agnostic, data unknown or undisturbed
  val info_vsew     = UInt(3.W)
  val info_vlmul    = UInt(3.W)
  val info_vl       = UInt(bVL.W)
  val info_vstart   = UInt(bVstart.W)
  val splitUopIdx   = UInt(3.W)
  val splitUopEnd   = Bool()
  val segIdx        = UInt(3.W)
  val destVRegStart = Bool()
  val destVRegEnd   = Bool()
}

class MuopTest extends Bundle {
  val uop           = new VUopTest
  val oldVd         = UInt(VLEN.W)
  val scalar_opnd_1 = UInt(XLEN.W)
  val scalar_opnd_2 = UInt(XLEN.W)
  val vs1           = UInt(VLEN.W)
  val vs2           = UInt(VLEN.W)
  val mask          = UInt(VLEN.W)
}

// object DataTable {
//   val dataTable = Seq(
//       // addr, data, exception
//       (0x0fd0.U, BigInt("4040404040404404", 16).U, false.B),
//       (0x0fd8.U, BigInt("3030303030303030", 16).U, false.B),
//       (0x0fe0.U, BigInt("2020202020202020", 16).U, false.B),
//       (0x0fe8.U, BigInt("1010101010101010", 16).U, false.B),
//       (0x0ff0.U, BigInt("5678901234503489", 16).U, false.B),
//       (0x0ff8.U, BigInt("eeeeeeeeeeeeeeee", 16).U, false.B),
//       (0x1000.U, BigInt("0123456789abcdef", 16).U, false.B),
//       (0x1008.U, BigInt("ffffffffffffffff", 16).U, false.B),
//       (0x1010.U, BigInt("0f0f0f0f0f0f0f0f", 16).U, false.B),
//       (0x1018.U, BigInt("fedcba9876543210", 16).U, false.B),
//       (0x1020.U, BigInt("1234567890123456", 16).U, false.B),
//       (0x1028.U, BigInt("0101010101010101", 16).U, false.B),
//       (0x1030.U, BigInt("2345678901234567", 16).U, false.B),
//       (0x1038.U, BigInt("1111111111111111", 16).U, false.B),
//       (0x1040.U, BigInt("2222222222222222", 16).U, false.B),
//       (0x1048.U, BigInt("3333333333333333", 16).U, false.B),
//       (0x1050.U, BigInt("4444444444444444", 16).U, false.B),
//       (0x1058.U, BigInt("5555555555555555", 16).U, false.B),
//       (0x1060.U, BigInt("6666666666666666", 16).U, true.B),
//       (0x1068.U, BigInt("0807060504030201", 16).U, false.B),
//       (0x1070.U, BigInt("1615141312111009", 16).U, false.B),
//       (0x1078.U, BigInt("081814100c1c0004", 16).U, false.B),
//       (0x1080.U, BigInt("00080000000c0004", 16).U, false.B),
//       (0x1088.U, BigInt("000000080004001c", 16).U, false.B),
//       (0x1090.U, BigInt("00080004000c0018", 16).U, false.B),
//       (0x1098.U, BigInt("00380004000c0020", 16).U, false.B),
//       (0x1100.U, BigInt("00080001000c0004", 16).U, false.B),
//       (0x1108.U, BigInt("000000080004001c", 16).U, false.B),
//       (0x1110.U, BigInt("00080060000c0004", 16).U, false.B),
//       (0x1118.U, BigInt("000000080004001c", 16).U, false.B),
//   )
// }

object DataTable {
  val dataTable = Seq(
    // addr, data, exception
    (0x0fd0.U, BigInt("30303030303030304040404040404404", 16).U, false.B),
    (0x0fe0.U, BigInt("10101010101010102020202020202020", 16).U, false.B),
    (0x0ff0.U, BigInt("eeeeeeeeeeeeeeee5678901234503489", 16).U, false.B),
    (0x1000.U, BigInt("ffffffffffffffff0123456789abcdef", 16).U, false.B),
    (0x1010.U, BigInt("fedcba98765432100f0f0f0f0f0f0f0f", 16).U, false.B),
    (0x1020.U, BigInt("01010101010101011234567890123456", 16).U, false.B),
    (0x1030.U, BigInt("11111111111111112345678901234567", 16).U, false.B),
    (0x1040.U, BigInt("33333333333333332222222222222222", 16).U, false.B),
    (0x1050.U, BigInt("55555555555555554444444444444444", 16).U, false.B),
    (0x1060.U, BigInt("08070605040302016666666666666666", 16).U, true.B),
    (0x1070.U, BigInt("081814100c1c00041615141312111009", 16).U, false.B),
    (0x1080.U, BigInt("000000080004001c00080000000c0004", 16).U, false.B),
    (0x1090.U, BigInt("00380004000c002000080004000c0018", 16).U, false.B),
    (0x1100.U, BigInt("000000080004001c00080001000c0004", 16).U, false.B),
    (0x1110.U, BigInt("000000080004001c00080060000c0004", 16).U, false.B),
  )
}

class LSUFakeDCache extends Module {
  val io = IO(new Bundle {
    val dataExchange = Flipped(new RVUMemory)
    val memInfo      = Output(Vec(DataTable.dataTable.length, UInt(dataWidth.W)))
  })

  val dataVec = RegInit(VecInit(DataTable.dataTable.map(entry =>
    VecInit(Seq.tabulate(dataBytes)(i => entry._2(i * 8 + 7, i * 8)))
  )))

  for (i <- 0 until DataTable.dataTable.length) {
    val flattenedData = Cat(dataVec(i).reverse)
    io.memInfo(i) := flattenedData
  }

  val hasXcpt = WireInit(false.B)
  val hasMiss = WireInit(false.B)

  val s1_valid = RegNext(io.dataExchange.req.valid)
  val s1_req   = RegNext(io.dataExchange.req.bits)
  val s2_valid = RegNext(s1_valid)
  val s2_req   = RegNext(s1_req)

  when(hasXcpt || hasMiss) {
    s1_valid := false.B
    s2_valid := false.B
  }

  val noise = RegInit("b011001001".U(32.W))
  noise := noise >> 1.U
  val miss = noise(0)

  io.dataExchange.resp.bits.idx   := s2_req.idx
  io.dataExchange.resp.bits.flag  := s2_req.flag
  io.dataExchange.resp.bits.srcId := s2_req.srcId
  io.dataExchange.req.ready       := true.B
  io.dataExchange.xcpt            := 0.U.asTypeOf(new HellaCacheExceptions())
  io.dataExchange.resp.bits.mask  := 0.U

  when(s2_valid) {
    val isXcpt = WireInit(false.B)
    val data   = WireInit(0.U(dataWidth.W))

    for (i <- 0 until DataTable.dataTable.length) {
      when(DataTable.dataTable(i)._1 === s2_req.addr) {
        isXcpt := DataTable.dataTable(i)._3
        data   := Cat(dataVec(i).reverse)
      }
    }

    for (i <- 0 until DataTable.dataTable.length) {
      when(DataTable.dataTable(i)._1 === s2_req.addr) {
        isXcpt := DataTable.dataTable(i)._3
        when(s2_req.cmd === VMemCmd.write && !miss && !isXcpt) {
          for (j <- 0 until dataBytes) {
            when(s2_req.mask(j)) {
              dataVec(i)(j) := s2_req.data(j * 8 + 7, j * 8)
            }
          }
        }
      }
    }

    io.dataExchange.resp.bits.nack     := miss
    io.dataExchange.resp.bits.data     := data
    io.dataExchange.resp.bits.has_data := s2_req.cmd === VMemCmd.read && ~isXcpt && ~miss
    io.dataExchange.resp.valid         := ~isXcpt && ~miss
    io.dataExchange.xcpt.ma.ld         := isXcpt && ~miss
    hasXcpt                            := isXcpt && ~miss
    hasMiss                            := miss
  }.otherwise {
    io.dataExchange.resp.valid         := false.B
    io.dataExchange.resp.bits.nack     := false.B
    io.dataExchange.resp.bits.has_data := false.B
    io.dataExchange.resp.bits.data     := 0.U
  }
}

class SmartVectorLsuTestWrapper(isLoad: Boolean) extends Module {
  val io = IO(new Bundle {
    val mUop     = Input(ValidIO(new MuopTest))
    val lsuOut   = ValidIO(new LsuOutput)
    val xcpt     = Output(new VLSUXcpt)
    val lsuReady = Output(Bool())
    val memInfo  = Output(Vec(DataTable.dataTable.length, UInt(dataWidth.W)))
  })

  val p = Parameters.empty.alterPartial {
    case SmartParamsKey  => SmartParameters(VLEN = 128)
    case VFuParamsKey    => VFuParameters(XLEN = 64, VLEN = 128)
    case XSCoreParamsKey => XSCoreParameters()
  }

  val vLsu = Module(new SVlsuWrapper()(p))

  io.lsuReady        := vLsu.io.lsuReady
  vLsu.io.mUop.valid := io.mUop.valid

  vLsu.io.mUop.bits.scalar_opnd_1     := io.mUop.bits.scalar_opnd_1
  vLsu.io.mUop.bits.scalar_opnd_2     := io.mUop.bits.scalar_opnd_2
  vLsu.io.mUop.bits.uopRegInfo.vs1    := io.mUop.bits.vs1
  vLsu.io.mUop.bits.uopRegInfo.vs2    := io.mUop.bits.vs2
  vLsu.io.mUop.bits.uopRegInfo.old_vd := io.mUop.bits.oldVd
  vLsu.io.mUop.bits.uopRegInfo.mask   := io.mUop.bits.mask
  vLsu.io.mUop.bits.uopRegInfo.vxsat  := false.B

  vLsu.io.mUop.bits.uop.sysUop        := DontCare
  vLsu.io.mUop.bits.uop.uopIdx        := io.mUop.bits.uop.splitUopIdx
  vLsu.io.mUop.bits.uop.uopEnd        := io.mUop.bits.uop.splitUopEnd
  vLsu.io.mUop.bits.uop.segIndex      := io.mUop.bits.uop.segIdx
  vLsu.io.mUop.bits.uop.destVRegStart := io.mUop.bits.uop.destVRegStart
  vLsu.io.mUop.bits.uop.destVRegEnd   := io.mUop.bits.uop.destVRegEnd

  vLsu.io.mUop.bits.uop.ctrl.vs2         := io.mUop.bits.uop.ctrl_vs2
  vLsu.io.mUop.bits.uop.ctrl.funct6      := io.mUop.bits.uop.ctrl_funct6
  vLsu.io.mUop.bits.uop.ctrl.funct3      := io.mUop.bits.uop.ctrl_funct3
  vLsu.io.mUop.bits.uop.ctrl.load        := io.mUop.bits.uop.ctrl_load
  vLsu.io.mUop.bits.uop.ctrl.store       := io.mUop.bits.uop.ctrl_store
  vLsu.io.mUop.bits.uop.ctrl.vm          := io.mUop.bits.uop.ctrl_vm
  vLsu.io.mUop.bits.uop.ctrl.vs1_imm     := DontCare
  vLsu.io.mUop.bits.uop.ctrl.narrow      := DontCare
  vLsu.io.mUop.bits.uop.ctrl.narrow_to_1 := DontCare
  vLsu.io.mUop.bits.uop.ctrl.widen       := DontCare
  vLsu.io.mUop.bits.uop.ctrl.widen2      := DontCare
  vLsu.io.mUop.bits.uop.ctrl.alu         := false.B
  vLsu.io.mUop.bits.uop.ctrl.mul         := false.B
  vLsu.io.mUop.bits.uop.ctrl.fp          := false.B
  vLsu.io.mUop.bits.uop.ctrl.div         := false.B
  vLsu.io.mUop.bits.uop.ctrl.fixP        := false.B
  vLsu.io.mUop.bits.uop.ctrl.redu        := false.B
  vLsu.io.mUop.bits.uop.ctrl.mask        := false.B
  vLsu.io.mUop.bits.uop.ctrl.perm        := false.B

  vLsu.io.mUop.bits.uop.info.ma               := io.mUop.bits.uop.info_ma
  vLsu.io.mUop.bits.uop.info.ta               := io.mUop.bits.uop.info_ta
  vLsu.io.mUop.bits.uop.info.vsew             := io.mUop.bits.uop.info_vsew
  vLsu.io.mUop.bits.uop.info.vlmul            := io.mUop.bits.uop.info_vlmul
  vLsu.io.mUop.bits.uop.info.vl               := io.mUop.bits.uop.info_vl
  vLsu.io.mUop.bits.uop.info.vstart           := io.mUop.bits.uop.info_vstart
  vLsu.io.mUop.bits.uop.info.vxrm             := DontCare
  vLsu.io.mUop.bits.uop.info.frm              := DontCare
  vLsu.io.mUop.bits.uop.ctrl.ldest            := DontCare
  vLsu.io.mUop.bits.uop.ctrl.lsrc             := DontCare
  vLsu.io.mUop.bits.uop.ctrl.floatRed         := false.B
  vLsu.io.mUop.bits.uop.ctrl.vGatherEi16EEW8  := DontCare
  vLsu.io.mUop.bits.uop.ctrl.vGatherEi16EEW16 := DontCare
  vLsu.io.mUop.bits.uop.ctrl.vGatherEi16EEW32 := DontCare
  vLsu.io.mUop.bits.uop.ctrl.vGatherEi16EEW64 := DontCare

  vLsu.io.mUopMergeAttr.valid                := io.mUop.valid
  vLsu.io.mUopMergeAttr.bits.rfWriteEn       := isLoad.asBool
  vLsu.io.mUopMergeAttr.bits.ldest           := DontCare
  vLsu.io.mUopMergeAttr.bits.muopEnd         := io.mUop.bits.uop.splitUopEnd
  vLsu.io.mUopMergeAttr.bits.alu             := false.B
  vLsu.io.mUopMergeAttr.bits.mul             := false.B
  vLsu.io.mUopMergeAttr.bits.fp              := false.B
  vLsu.io.mUopMergeAttr.bits.div             := false.B
  vLsu.io.mUopMergeAttr.bits.fixP            := false.B
  vLsu.io.mUopMergeAttr.bits.redu            := false.B
  vLsu.io.mUopMergeAttr.bits.mask            := false.B
  vLsu.io.mUopMergeAttr.bits.perm            := false.B
  vLsu.io.mUopMergeAttr.bits.floatRegWriteEn := false.B

  vLsu.io.mUopMergeAttr.bits.scalarRegWriteEn := false.B
  vLsu.io.mUopMergeAttr.bits.regBackWidth     := 7.U
  vLsu.io.mUopMergeAttr.bits.regWriteMuopIdx  := 0.U
  vLsu.io.mUopMergeAttr.bits.permExpdLen      := 0.U
  vLsu.io.mUopMergeAttr.bits.regDstIdx        := 0.U
  vLsu.io.mUopMergeAttr.bits.regCount         := 1.U

  io.lsuOut.valid            := vLsu.io.lsuOut.valid
  io.lsuOut.bits.data        := vLsu.io.lsuOut.bits.data
  io.lsuOut.bits.rfWriteEn   := vLsu.io.lsuOut.bits.rfWriteEn
  io.lsuOut.bits.rfWriteMask := vLsu.io.lsuOut.bits.rfWriteMask
  io.lsuOut.bits.rfWriteIdx  := vLsu.io.lsuOut.bits.rfWriteIdx
  io.lsuOut.bits.muopEnd     := vLsu.io.lsuOut.bits.muopEnd
  io.lsuOut.bits.isSegLoad   := vLsu.io.lsuOut.bits.isSegLoad
  io.lsuOut.bits.regStartIdx := vLsu.io.lsuOut.bits.regStartIdx
  io.lsuOut.bits.regCount    := vLsu.io.lsuOut.bits.regCount
  io.lsuOut.bits.xcpt        := vLsu.io.lsuOut.bits.xcpt
  io.xcpt                    := vLsu.io.lsuOut.bits.xcpt

  val dcache = Module(new LSUFakeDCache)
  vLsu.io.dataExchange <> dcache.io.dataExchange
  io.memInfo := dcache.io.memInfo
}
