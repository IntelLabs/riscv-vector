package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._

class VLSUOutput extends Bundle {
    val vd = UInt(VLEN.W)
    val uopQueueIdx = UInt(4.W) // magic number
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop = Flipped(ValidIO(new Muop()(p)))
    val oldVd = Input(UInt(VLEN.W))
    val lsuOut = ValidIO(new VLSUOutput)
    val dataExchange = new RVUMemory()
    val xcpt = Output(new HellaCacheExceptions)
    val lsuReady = Output(Bool())
}

object VRegSegmentStatus {
  val invalid :: needData :: notReady :: ready :: Nil = Enum(4)
}

object Mop {
    val unit_stride     = "b00".U
    val index_unodered  = "b01".U
    val constant_stride = "b10".U
    val index_ordered   = "b11".U
}

object VMemCmd {
    val read = false.B
    val write = true.B
}

class LdstUop extends Bundle {
    val valid = Bool()
    val addr = Output(UInt(64.W))
    val queueIdx = Output(UInt(4.W))
    val isLast = Output(Bool())
}

class VRegSegmentInfo extends Bundle {
    // VRegSegmentStatus
    val status = UInt(2.W)
    // corresponding ldstuop idx of current vreg segement
    val idx = UInt(8.W)
    // offset of writeback valid data for current vreg segement
    val offset = UInt(log2Ceil(64).W)
    // data of current vreg segement
    val data = UInt(8.W)
}

class SVlsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    var segNum = VLEN / 8
    // split fsm states
    val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)
    val uopState        = RegInit(uop_idle)
    val nextUopState    = WireInit(uop_idle)
    val completeLd      = RegInit(false.B)

    // uop & control related
    val mUopReg         = RegInit(0.U.asTypeOf(new Muop()(p)))
    val eewbReg         = RegInit(8.U)
    val uopVlReg        = RegInit(0.U(bVL.W))
    val ldstTypeReg     = RegInit(0.U(2.W))

    // ldQueue
    var ldUopQueueSize  = 16
    val ldUopIdxBits    = log2Up(ldUopQueueSize)
    val ldstEnqPtr      = RegInit(0.U(ldUopIdxBits.W))
    val ldstDeqPtr      = RegInit(0.U(ldUopIdxBits.W))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(ldUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // vreg seg info
    val vregSegCount    = VLEN / 8
    val vregInfo        = RegInit(VecInit(Seq.fill(vregSegCount)(0.U.asTypeOf(new VRegSegmentInfo))))

    // Split info
    val splitCount      = RegInit(0.U(5.W))
    val curSplitIdx     = RegInit(0.U(5.W))
    val splitOffset     = RegInit(0.U(5.W))
    
    /****************************SPLIT STAGE*********************************/

    //                                                        splitIdx + 1
    // +------------+                                       +-------------+
    // |            |                                       |             |
    // |      +-----+-----+                          +------+------+      |
    // |      |           |       mUop.Valid         |             |      |
    // +----> | uop_idle  +-------------------------->  uop_split  | <----+
    //        |           |                          |             |
    //        +-----+-----+                          +------+------+
    //              ^                                       |
    //              |                                       |
    //   completeLd |                                       | splitIdx = splitCount
    //              |        +--------------------+         |
    //              |        |                    |         |
    //              +--------+  uop_split_finish  +<--------+
    //                       |                    |
    //                       +--------------------+
    //

    // SPLIT FSM -- decide next state
    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            nextUopState := uop_split
        }.otherwise {
            nextUopState := uop_idle
        }
    }.elsewhen(uopState === uop_split) {
        when(splitCount - 1.U === curSplitIdx) {
            nextUopState := uop_split_finish
        }.otherwise {
            nextUopState := uop_split
        }
    }.elsewhen(uopState === uop_split_finish) {
        when(completeLd) {
            nextUopState := uop_idle
            completeLd := false.B
        }.otherwise {
            nextUopState := uop_split_finish
        }
    }.otherwise {
        nextUopState := uop_idle
    }
    // SPLIT FSM -- transition
    uopState := nextUopState

    when(nextUopState === uop_idle) {
        io.lsuReady := true.B // LSU free --> accept new uop request
    }.otherwise {
        io.lsuReady := false.B
    }

    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            mUopReg := io.mUop.bits

            val (funct6, funct3) = (io.mUop.bits.uop.ctrl.funct6, io.mUop.bits.uop.ctrl.funct3)
            val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
            val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)
            // setup info
            // mw + width[2:0] 
            val mwCatWidth = Cat(funct6(2), funct3)
            val eewb = MuxCase(1.U, Array(
                (mwCatWidth === "b0001".U) -> 1.U,
                (mwCatWidth === "b0101".U) -> 2.U,
                (mwCatWidth === "b0110".U) -> 4.U,
                (mwCatWidth === "b0111".U) -> 8.U,
            ))
            eewbReg := eewb

            // ldst type
            ldstTypeReg := MuxCase(0.U, Array(
                (funct6(1, 0) === "b00".U) -> Mop.unit_stride,
                (funct6(1, 0) === "b01".U) -> Mop.index_unodered,
                (funct6(1, 0) === "b10".U) -> Mop.constant_stride,
                (funct6(1, 0) === "b11".U) -> Mop.index_ordered,
            ))
            
            // decide micro vl
            val elen = (VLEN.U >> 3.U) / eewb
            val uopVl = Mux(
                uopIdx === 0.U && uopEnd, vl - vstart, 
                Mux(
                    uopIdx === 0.U, elen - vstart,
                    Mux(uopEnd, vl - (elen - vstart) - elen * (uopIdx - 1.U), elen)
                )
            )
            uopVlReg := uopVl

            // Set split info
            ldstEnqPtr  := 0.U
            curSplitIdx := 0.U
            splitCount  := uopVl
            splitOffset := Mux(uopIdx === 0.U, vstart, 0.U)

            for(i <- 0 until segNum) {
                vregInfo(i).data := Reverse(io.oldVd(8 * i + 7, 8 * i))

                when(i.U < uopVl) {
                    for(j <- 0 until segNum) {
                        when(j.U < eewb) {
                            vregInfo((splitOffset + i.U) * eewb + j.U).status := VRegSegmentStatus.needData
                        }
                    }
                }
            }
        }
    }.elsewhen(uopState === uop_split) {
        // unit stride
        when(curSplitIdx < splitCount) {
            // align addr to eew
            val addr = ((mUopReg.scalar_opnd_1 >> eewbReg) << eewbReg)  + curSplitIdx * eewbReg
            // align addr to 64 bits
            val alignedAddr = (addr >> 3.U) << 3.U
            val offset = addr - alignedAddr

            ldstUopQueue(ldstEnqPtr).valid  := true.B
            ldstUopQueue(ldstEnqPtr).addr   := alignedAddr
            ldstUopQueue(ldstEnqPtr).isLast := (curSplitIdx === splitCount - 1.U)

            for(i <- 0 until segNum) {
                when(i.U < eewbReg) {
                    vregInfo((splitOffset + curSplitIdx) * eewbReg + i.U).status := VRegSegmentStatus.notReady
                    vregInfo((splitOffset + curSplitIdx) * eewbReg + i.U).idx := ldstEnqPtr
                    vregInfo((splitOffset + curSplitIdx) * eewbReg + i.U).offset := offset + i.U
                }
            }
            curSplitIdx := curSplitIdx + 1.U
            ldstEnqPtr  := ldstEnqPtr  + 1.U
        }
    }
    /*-----------------SPLIT STAGE END-----------------------*/


    /******************ISSUE STAGE START**********************/
    // issue stage
    // issue lduop to hellacache
    val issueLdstPtr = RegInit(0.U(ldUopIdxBits.W))

    val ld_issue :: ld_wait :: Nil = Enum(2)
    val dataExchangeState     = RegInit(ld_issue)
    val nextDataExchangeState = WireInit(ld_issue)

    // ISSUE FSM -- decide next state
    when(dataExchangeState === ld_issue) {
        when(ldstUopQueue(issueLdstPtr).valid && io.dataExchange.req.ready) {
            nextDataExchangeState := ld_wait
        }.otherwise {
            nextDataExchangeState := ld_issue
        }
    }.elsewhen(dataExchangeState === ld_wait) {
        when(io.dataExchange.resp.valid) {
            nextDataExchangeState := ld_issue
        }.otherwise {
            nextDataExchangeState := ld_wait
        }
    }.otherwise {
        nextDataExchangeState := ld_issue
    }

    // SPLIT FSM -- transition
    dataExchangeState := nextDataExchangeState

    when(dataExchangeState === ld_issue && ldstUopQueue(issueLdstPtr).valid && io.dataExchange.req.ready) {
        io.dataExchange.req.valid       := true.B
        io.dataExchange.req.bits.addr   := ldstUopQueue(issueLdstPtr).addr
        io.dataExchange.req.bits.cmd    := VMemCmd.read
        io.dataExchange.req.bits.signed := false.B
        io.dataExchange.req.bits.idx    := issueLdstPtr
        io.dataExchange.req.bits.data   := DontCare
        io.dataExchange.req.bits.mask   := DontCare

    }.otherwise {
        io.dataExchange.req.valid       := false.B
        io.dataExchange.req.bits        := DontCare
    }
     

    /************************cache hit***********************/
    // cache hit after 2 cycle
    // data writeback from cache

    /*todo
        * 1. write data into dest reg
        * 2. deq lduop
    */
    when(dataExchangeState === ld_wait && io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data) {
        val loadData = io.dataExchange.resp.bits.data
        for(i <- 0 until segNum) {
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === issueLdstPtr) {
                for(j <- 0 until 8) {
                    when(vregInfo(i).offset === j.U) {
                        vregInfo(i).data := Reverse(loadData(j * 8 + 7, j * 8))
                    }
                }
                vregInfo(i).status := VRegSegmentStatus.ready
            }
        }
        ldstUopQueue(issueLdstPtr).valid := false.B
        issueLdstPtr := Mux(ldstUopQueue(issueLdstPtr).isLast, 0.U, issueLdstPtr + 1.U)
        completeLd := Mux(ldstUopQueue(issueLdstPtr).isLast, true.B, false.B)
    }
    
    /**************************exception handling**********************************/
    // exception handling
    // when(dataExchangeState === ld_wait && io.dataExchange.resp.valid && io.dataExchange.xcpt) {
    //     io.xcpt.valid := true.B
    //     io.xcpt.xcptElemIdx := 0.U
    // }.otherwise {
    //     io.xcpt.valid := false.B
    //     io.xcpt.xcptElemIdx := DontCare
    // }
    io.xcpt <> io.dataExchange.xcpt

    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_ready = Wire(Bool())
    vreg_wb_ready := vregInfo.forall(
        info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.invalid
    ) && !vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)

    when(vreg_wb_ready) {
        io.lsuOut.valid := true.B
        io.lsuOut.bits.vd := Cat(vregInfo.reverseMap(entry => Reverse(entry.data))) // Concatenate data from all vregInfo elements)
        io.lsuOut.bits.uopQueueIdx := mUopReg.uop.uopIdx

        // Reset vreg info
        for (i <- 0 until segNum) {
            vregInfo(i).status  := VRegSegmentStatus.invalid
            vregInfo(i).idx     := DontCare
            vregInfo(i).offset  := DontCare
            vregInfo(i).data    := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits  := DontCare
    }

}
