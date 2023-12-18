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
  val invalid :: srcData :: needLdst :: notReady :: ready :: xcpt :: Nil = Enum(6)
}

object Mop {
    val unit_stride     = "b00".U
    val index_unodered  = "b01".U
    val constant_stride = "b10".U
    val index_ordered   = "b11".U
}

object UnitStrideMop {
    val unit_stride      = "b00000".U
    val whole_register   = "b01000".U
    val mask             = "b01011".U
    val fault_only_first = "b10000".U
}

object VMemCmd {
    val read  = false.B
    val write = true.B
}

class LdstUop extends Bundle {
    val valid   = Bool()
    val addr    = Output(UInt(64.W))
    val pos     = Output(UInt(bVL.W)) // position in vl
}

class VRegSegmentInfo extends Bundle {
    // VRegSegmentStatus
    val status  = UInt(3.W)
    // corresponding ldstuop idx of current vreg segement
    val idx     = UInt(ldUopQueueWidth.W)
    // offset of writeback valid data for current vreg segement
    val offset  = UInt(log2Ceil(8).W)
    // data of current vreg segement
    val data    = UInt(8.W)
}

class SVlsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    // split fsm states
    val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)
    val uopState        = RegInit(uop_idle)
    val nextUopState    = WireInit(uop_idle)
    val completeLd      = WireInit(false.B)

    // uop & control related
    val mUopReg         = RegInit(0.U.asTypeOf(new Muop()(p)))
    val mUopMergeReg    = RegInit(0.U.asTypeOf(new MuopMergeAttr))
    val unitSMopReg     = RegInit(0.U(5.W))
    val memwbReg        = RegInit(8.U)
    val eewbReg         = RegInit(8.U)
    val memwAlignReg    = RegInit(8.U)
    val elenReg         = RegInit(0.U(vlenbWidth.W))
    val mlenReg         = RegInit(0.U(vlenbWidth.W))
    val ldstTypeReg     = RegInit(0.U(2.W))
    val vmReg           = RegInit(true.B)
    val noUseMuop       = RegInit(false.B)

    // vreg seg info
    val vregInfo        = RegInit(VecInit(Seq.fill(vlenb)(0.U.asTypeOf(new VRegSegmentInfo))))

    // Split info
    val splitCount      = RegInit(0.U(vlenbWidth.W))
    val curSplitIdx     = RegInit(0.U(vlenbWidth.W))
    val splitStart      = RegInit(0.U(vlenbWidth.W))

    // ldQueue
    val ldEnqPtr        = RegInit(0.U(ldUopQueueWidth.W))
    val issueLdPtr      = RegInit(0.U(ldUopQueueWidth.W))
    val ldUopQueue      = RegInit(VecInit(Seq.fill(ldUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
    val hellaXcptReg    = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

    // val hasXcptHappened
    // assertion
    // exception only once
    val mem_xcpt        = io.dataExchange.resp.valid && (
                          io.dataExchange.xcpt.pf.st || io.dataExchange.xcpt.pf.ld ||
                          io.dataExchange.xcpt.ae.st || io.dataExchange.xcpt.ae.ld ||
                          io.dataExchange.xcpt.ma.st || io.dataExchange.xcpt.ma.ld)

    /****************************SPLIT STAGE*********************************/
    /*
                                                     splitId+1
                    +--------+                       +--------+
                    |        |                       |        |
                    |   +----+---+  mUop.Valid  +----+----+   |
                    |-> |uop_idle|--------------|uop_split| <-|
                        +---+----+              +----+----+
                            |                        |
                  completeLd|                        |splitIdx = splitCount-1
                            |   +----------------+   |        || xcpt
                            |-> |uop_split_finish| <-|
                                +----------------+

    */        

    io.lsuReady := Mux(uopState === uop_idle, true.B, false.B)
    // SPLIT FSM -- decide next state
    when(uopState === uop_idle) {
        when(io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst) {
            nextUopState := uop_split
        }.otherwise {
            nextUopState := uop_idle
        }
    }.elsewhen(uopState === uop_split) {
        when(splitCount === 0.U) {
            nextUopState := uop_idle
        }.elsewhen((splitCount - 1.U === curSplitIdx) || mem_xcpt) {
            nextUopState := uop_split_finish
        }.otherwise {
            nextUopState := uop_split
        }
    }.elsewhen(uopState === uop_split_finish) {
        when(completeLd) {
            nextUopState := uop_idle
        }.otherwise {
            nextUopState := uop_split_finish
        }
    }.otherwise {
        nextUopState := uop_idle
    }
    // SPLIT FSM -- transition
    uopState := nextUopState

    /*****************************SPLIT -- IDLE stage****************************************/
    val (funct6, funct3) = (io.mUop.bits.uop.ctrl.funct6, io.mUop.bits.uop.ctrl.funct3)
    val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
    val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)
    val (vsew, vm)       = (io.mUop.bits.uop.info.vsew, io.mUop.bits.uop.ctrl.vm)
    val unitStrideMop    = io.mUop.bits.uop.ctrl.vs2
    
    // eew and sew in bytes calculation
    val eewb = MuxLookup(Cat(funct6(2), funct3), 1.U, Seq(
        "b0000".U -> 1.U, "b0101".U -> 2.U, "b0110".U -> 4.U, "b0111".U -> 8.U
    ))
    val sewb = MuxLookup(vsew, 1.U, Seq(
        "b000".U -> 1.U, "b001".U -> 2.U, "b010".U -> 4.U, "b011".U -> 8.U
    ))

    // ldst type determination
    val ldstType = MuxLookup(funct6(1, 0), Mop.unit_stride, Seq(
        "b00".U -> Mop.unit_stride,     "b01".U -> Mop.index_unodered,
        "b10".U -> Mop.constant_stride, "b11".U -> Mop.index_ordered
    ))

    // unit-stride & strided use eew as memwb, indexed use sew
    val memwb       = Mux(ldstType === Mop.index_ordered || ldstType === Mop.index_unodered, sewb, eewb)
    val mlen        = vlenb.U / memwb
    val elen        = vlenb.U / eewb
    val minLen      = elen min mlen

    // decide micro vl
    val actualVl    = Mux(unitStrideMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
    val doneLen     = minLen * uopIdx
    val leftLen     = Mux(actualVl > doneLen, actualVl - doneLen, 0.U)
    val microVl     = minLen min leftLen
    val microVStart = Mux(vstart < doneLen, 0.U, minLen min (vstart - doneLen))

    val vregClean   = vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)
    val memVl       = leftLen min mlen
    val memVstart   = Mux(vstart < doneLen, 0.U, mlen min (vstart - doneLen))

    when(uopState === uop_idle) {
        when(io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst) {
            mUopReg      := io.mUop.bits
            mUopMergeReg := io.mUopMergeAttr.bits

            // 1->0, 2->1, 4->2, 8->3
            memwAlignReg := MuxCase(0.U, Array(
                (memwb === 1.U) -> 0.U,
                (memwb === 2.U) -> 1.U,
                (memwb === 4.U) -> 2.U,
                (memwb === 8.U) -> 3.U,
            ))

            unitSMopReg := unitStrideMop
            vmReg       := vm
            eewbReg     := eewb
            ldstTypeReg := ldstType
            memwbReg    := memwb
            elenReg     := elen
            mlenReg     := mlen

            // Set split info
            ldEnqPtr   := 0.U
            issueLdPtr := 0.U
            curSplitIdx  := 0.U
            splitCount   := microVl - microVStart     
            noUseMuop    := (microVl === microVStart) 
            splitStart   := microVStart

            // set vreg
            when(vregClean) {
                (0 until vlenb).foreach { i => vregInfo(i).data := io.mUop.bits.uopRegInfo.old_vd(8 * i + 7, 8 * i) }

                (0 until vlenb).foreach { i =>
                    (0 until vlenb).foreach { j =>
                        when(i.U < mlen && j.U < memwb) {
                            vregInfo(i.U * memwb + j.U).status :=
                                Mux(i.U < memVl && i.U >= memVstart, VRegSegmentStatus.needLdst, VRegSegmentStatus.srcData)
                        }
                    }
                }   
            }
        }
    }

    /*-----------------------------------------calc addr start-------------------------------------------------*/
    /*                                                                                                         */
    val curVl       = mUopReg.uop.uopIdx * (elenReg min mlenReg) + splitStart + curSplitIdx
    val calcAddr    = WireInit(0.U(64.W))
    val addr        = WireInit(0.U(64.W))
    val addrMask    = WireInit(0.U(64.W))
    val alignedAddr = WireInit(0.U(64.W))
    val offset      = WireInit(0.U(log2Ceil(8).W))
    val baseSegIdx  = (curVl % mlenReg) * memwbReg

    val isNotMasked = mUopReg.uopRegInfo.mask(curVl)
    val baseAddr    = mUopReg.scalar_opnd_1

    // indexed addr
    val idxVal      = WireInit(0.U(XLEN.W))
    val idxMask     = WireInit(0.U(XLEN.W))
    val eew         = eewbReg << 3.U
    val beginIdx    = (curVl % elenReg) * (eew)
    idxMask := (("h1".asUInt(64.W) << eew) - 1.U)
    idxVal := (mUopReg.uopRegInfo.vs2 >> beginIdx) & idxMask

    when(ldstTypeReg === Mop.unit_stride) {
        calcAddr := baseAddr + curVl * memwbReg
    }.elsewhen(ldstTypeReg === Mop.constant_stride) {
        when(mUopReg.scalar_opnd_2 === 0.U) {
            calcAddr := baseAddr
        }.otherwise {
            calcAddr := (Cat(false.B, baseAddr).asSInt + curVl * (mUopReg.scalar_opnd_2).asSInt).asUInt
        }
    }.elsewhen(ldstTypeReg === Mop.index_ordered || ldstTypeReg === Mop.index_unodered) {        
        calcAddr := (Cat(false.B, baseAddr).asSInt + idxVal.asSInt).asUInt
    }.otherwise {
        // do something
    }

    addrMask    := ~(("h1".asUInt(64.W) << memwAlignReg) - 1.U)
    addr        := calcAddr & addrMask  // align calcAddr to memwb
    alignedAddr := (addr >> 3.U) << 3.U // align addr to 64 bits
    offset      := addr - alignedAddr 
    /*                                                                                                         */
    /*-----------------------------------------calc addr end---------------------------------------------------*/

    when(uopState === uop_split && !mem_xcpt) {
        noUseMuop := false.B
        when(curSplitIdx < splitCount) {
            when(vmReg || isNotMasked) {
                ldUopQueue(ldEnqPtr).valid  := true.B
                ldUopQueue(ldEnqPtr).addr   := alignedAddr
                ldUopQueue(ldEnqPtr).pos    := curVl

                ldEnqPtr  := ldEnqPtr  + 1.U
            }

            for(i <- 0 until vlenb) {
                when(i.U < memwbReg) {
                    val segIdx = baseSegIdx + i.U
                    when(vmReg || isNotMasked) {
                        vregInfo(segIdx).status := VRegSegmentStatus.notReady
                        vregInfo(segIdx).idx    := ldEnqPtr
                        vregInfo(segIdx).offset := offset + i.U
                    }.otherwise {
                        vregInfo(segIdx).status := VRegSegmentStatus.srcData
                    }
                }
            }
            curSplitIdx := curSplitIdx + 1.U
        }
    }
    /*-----------------SPLIT STAGE END-----------------------*/


    /*********************************ISSUE START*********************************/
    // update issueLdPtr
    when(io.dataExchange.resp.valid && io.dataExchange.resp.bits.nack && !mem_xcpt) {
        issueLdPtr := io.dataExchange.resp.bits.idx
    }.elsewhen(ldUopQueue(issueLdPtr).valid && io.dataExchange.req.ready) {
        issueLdPtr := issueLdPtr + 1.U
    }

    when(ldUopQueue(issueLdPtr).valid && io.dataExchange.req.ready) {
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := ldUopQueue(issueLdPtr).addr
        io.dataExchange.req.bits.cmd    := VMemCmd.read
        io.dataExchange.req.bits.idx    := issueLdPtr
        io.dataExchange.req.bits.data   := DontCare
        io.dataExchange.req.bits.mask   := DontCare
    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }

    /*---------------------------------ISSUE END---------------------------------*/


    /*********************************RESP START*********************************/
    val (respLdPtr, respData) = (io.dataExchange.resp.bits.idx, io.dataExchange.resp.bits.data)

    when(io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data && !mem_xcpt) { // !nack
        ldUopQueue(respLdPtr).valid := false.B

        (0 until vlenb).foreach { i =>
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respLdPtr) {
                val offsetOH = UIntToOH(vregInfo(i).offset, 8)
                vregInfo(i).data := Mux1H(
                    offsetOH, 
                    Seq (respData( 7,  0), respData(15,  8), respData(23, 16), respData(31, 24),
                         respData(39, 32), respData(47, 40), respData(55, 48), respData(63, 56))
                )

                vregInfo(i).status := VRegSegmentStatus.ready
            }
        }

    }.elsewhen(mem_xcpt) { // exception handling
        // 1. clear ldUopQueue
        ldUopQueue.foreach(uop => uop.valid := false.B)

        // 2. update xcpt info
        xcptVlReg       := ldUopQueue(respLdPtr).pos
        hellaXcptReg    := io.dataExchange.xcpt

        // 3. update vreg
        for(i <- 0 until vlenb) {
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respLdPtr) {
                vregInfo(i).status := VRegSegmentStatus.xcpt
            }
        }
    }

    completeLd := ldUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened
    
    /*---------------------------------RESP END---------------------------------*/

    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_xcpt  = vregInfo.map(info => info.status === VRegSegmentStatus.xcpt).reduce(_ || _)
    val vreg_wb_ready =
        vregInfo.forall(info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.srcData) ||
        (vregInfo.forall(info => info.status === VRegSegmentStatus.invalid) && noUseMuop)

    when(vreg_wb_ready || vreg_wb_xcpt) {
        io.lsuOut.valid             := true.B
        io.lsuOut.bits.data         := Cat(vregInfo.reverseMap(entry => entry.data)) // Concatenate data from all vregInfo elements)
        io.lsuOut.bits.muopEnd      := mUopMergeReg.muopEnd
        io.lsuOut.bits.rfWriteEn    := mUopMergeReg.rfWriteEn
        io.lsuOut.bits.rfWriteIdx   := mUopMergeReg.ldest

        // Reset vreg info
        for (i <- 0 until vlenb) {
            vregInfo(i).status  := VRegSegmentStatus.invalid
            vregInfo(i).idx     := DontCare
            vregInfo(i).offset  := DontCare
            vregInfo(i).data    := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits  := DontCare
    }

    // exception output
    when(vreg_wb_xcpt) {
        when(unitSMopReg === UnitStrideMop.fault_only_first && xcptVlReg > 0.U) {
            io.xcpt.exception_vld   := false.B
            io.xcpt.xcpt_cause      := 0.U.asTypeOf(new HellaCacheExceptions)
        }.otherwise {
            io.xcpt.exception_vld   := true.B
            io.xcpt.xcpt_cause      := hellaXcptReg
        }
        io.xcpt.update_vl           := true.B
        io.xcpt.update_data         := xcptVlReg
    }.otherwise {
        io.xcpt.exception_vld       := false.B
        io.xcpt.update_vl           := false.B
        io.xcpt.update_data         := DontCare
        io.xcpt.xcpt_cause          := 0.U.asTypeOf(new HellaCacheExceptions)
    }

}
