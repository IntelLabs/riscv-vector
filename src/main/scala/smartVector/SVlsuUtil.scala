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
    val exception_vld   = Bool()
    val update_vl       = Bool()
    val update_data     = UInt(bVL.W)
    val xcpt_cause      = new HellaCacheExceptions()
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop            = Input(ValidIO(new Muop()(p)))
    val mUopMergeAttr   = Input(ValidIO(new MuopMergeAttr))
    val lsuOut          = Output(ValidIO(new LsuOutput))
    val xcpt            = Output(new VLSUXcpt)
    val dataExchange    = new RVUMemory()
    val lsuReady        = Output(Bool())
}

object VRegSegmentStatus {
  val invalid :: srcData :: agnostic :: needLdst :: notReady :: ready :: xcpt :: Nil = Enum(7)
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

object LdstUopXcptCause {
    val misalign = 1.U
    val mem_xcpt = 2.U
}

class LdstUop extends Bundle {
    val valid = Bool()
    val addr  = UInt(addrWidth.W)
    val groupIdx = UInt(2.W)
    val splitIdx = UInt(vlenbWidth.W)
    val memCmd = Bool()
    val pos   = UInt(bVL.W) // position in vl
    val xcpt  = UInt(2.W)
}

class VRegSegmentInfo extends Bundle {
    // VRegSegmentStatus
    val status  = UInt(3.W)
    // corresponding ldstuop idx of current vreg segement
    val idx     = UInt(ldstUopQueueWidth.W)
    // offset of writeback valid data for current vreg segement
    val offset  = UInt(log2Ceil(8).W)
    // data of current vreg segement
    val data    = UInt(8.W)
}

class VRegInfo extends Bundle {
    val busy        = Bool()
    val clean    = Bool()
    val vregSeg     = Vec(vlenb, new VRegSegmentInfo)
}

class mUopInfo extends Bundle {
    val uopIdx      = UInt(3.W)
    val uopEnd      = Bool()
    val segIdx      = UInt(log2Ceil(8).W)

    val rs1Val      = UInt(XLEN.W)
    val rs2Val      = UInt(XLEN.W)
    val vs2         = UInt(VLEN.W)
    val mask        = UInt(VLEN.W)

    // merge attr
    val muopEnd     = Bool()
    val rfWriteEn   = Bool()
    val ldest       = UInt(5.W)

}

object mUopInfoSelecter {
    def apply(mUop: Muop, mUopMergeAttr: MuopMergeAttr): mUopInfo = {
        val info        = Wire(new mUopInfo)

        info.uopIdx     := mUop.uop.uopIdx
        info.uopEnd     := mUop.uop.uopEnd
        info.segIdx     := mUop.uop.segIndex

        info.rs1Val     := mUop.scalar_opnd_1
        info.rs2Val     := mUop.scalar_opnd_2
        info.vs2        := mUop.uopRegInfo.vs2
        info.mask       := mUop.uopRegInfo.mask

        info.muopEnd    := mUopMergeAttr.muopEnd
        info.rfWriteEn  := mUopMergeAttr.rfWriteEn
        info.ldest      := mUopMergeAttr.ldest

        info
    }
}

class LSULdstCtrl extends Bundle {
    val isLoad          = Bool()
    val isStore         = Bool()
    val nfield          = UInt(4.W) // 1~8
    val ldstType        = UInt(4.W)
    val unitSMop        = UInt(5.W)

    val ma              = Bool()
    val ta              = Bool()

    val vm              = Bool()
    val memwb           = UInt(4.W)
    val eewb            = UInt(4.W)
    val log2Memwb       = UInt(2.W)
    val log2Eewb        = UInt(2.W)

    val mlen            = UInt(vlenbWidth.W)
    val elen            = UInt(vlenbWidth.W)
    val minLen          = UInt(vlenbWidth.W)
    val log2Mlen        = UInt(log2Ceil(vlenbWidth).W)
    val log2Elen        = UInt(log2Ceil(vlenbWidth).W)
    val log2MinLen      = UInt(log2Ceil(vlenbWidth).W)
}

object LSULdstDecoder {
    def apply(mUop: Muop, mUopMergeAttr: MuopMergeAttr): LSULdstCtrl = {
        val (funct6, funct3) = (mUop.uop.ctrl.funct6, mUop.uop.ctrl.funct3)
        val (vsew, vm)       = (mUop.uop.info.vsew, mUop.uop.ctrl.vm)
        val unitStrdeMop     =  mUop.uop.ctrl.vs2

        val nf   = funct6(5, 3)
        val mop  = funct6(1, 0)
        val ctrl = Wire(new LSULdstCtrl)

        ctrl.isLoad   := mUop.uop.ctrl.load
        ctrl.isStore  := mUop.uop.ctrl.store

        ctrl.ma       := mUop.uop.info.ma
        ctrl.ta       := mUop.uop.info.ta

        ctrl.ldstType := MuxLookup(funct6(1, 0), Mop.unit_stride, Seq(
            "b00".U -> Mop.unit_stride,     "b01".U -> Mop.index_unodered,
            "b10".U -> Mop.constant_stride, "b11".U -> Mop.index_ordered
        ))
        ctrl.unitSMop := Mux((mop === "b00".U), unitStrdeMop, UnitStrideMop.not_unit_strde)
        // eew and sew in bytes calculation
        val sewb   = 1.U << vsew
        ctrl.eewb := 1.U << funct3(1, 0)

        ctrl.memwb      := Mux(ctrl.ldstType === Mop.index_ordered || ctrl.ldstType === Mop.index_unodered, sewb, ctrl.eewb)
        ctrl.nfield     := Mux(ctrl.unitSMop === UnitStrideMop.whole_register, 1.U, nf +& 1.U)
        ctrl.vm         := vm
        // 1->0, 2->1, 4->2, 8->3
        ctrl.log2Memwb  := Mux(ctrl.ldstType === Mop.index_ordered || ctrl.ldstType === Mop.index_unodered, vsew, funct3(1, 0))
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


class GroupInfo extends Bundle {
    val allocIdx = UInt(GroupNum.W)
    val ldstCtrl = new LSULdstCtrl
    val muopInfo = new mUopInfo
}

class GroupInfoQueue(size: Int) extends Module {
    val io = IO(
        new Bundle {
            val enq = Flipped(Decoupled(new GroupInfo))
            val deq = Decoupled(new GroupInfo)
            val front = ValidIO(new GroupInfo)
            val tail = ValidIO(new GroupInfo)
            val clear = Input(Bool())
        }
    )

    val queue       = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new GroupInfo))))
    val enqPtr      = RegInit(0.U(log2Ceil(size).W))
    val lastEnqPtr  = RegInit(0.U(log2Ceil(size).W))
    val deqPtr      = RegInit(0.U(log2Ceil(size).W))
    val count       = RegInit(0.U(log2Ceil(size).W))

    io.enq.ready := count < size.U
    io.deq.valid := count > 0.U
    io.deq.bits := queue(deqPtr)

    io.front.valid := count > 0.U
    io.front.bits := queue(deqPtr)

    io.tail.valid := count > 0.U
    io.tail.bits := queue(lastEnqPtr)

    when (io.clear) {
        (0 until size).foreach(i => queue(i).muopInfo := 0.U.asTypeOf(new mUopInfo))

        enqPtr  := 0.U
        lastEnqPtr := 0.U
        deqPtr  := 0.U
        count   := 0.U
    } .otherwise {
        when(io.enq.fire()) {
            queue(enqPtr) := io.enq.bits
            enqPtr := enqPtr + 1.U
            count := count + 1.U
            lastEnqPtr := enqPtr
        }

        when(io.deq.fire()) {
            deqPtr := deqPtr + 1.U
            count := count - 1.U
        }
    }
}

