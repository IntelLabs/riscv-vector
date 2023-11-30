package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._

/*
    what I need:
        * XLEN --> to obtain the highest bit in scalar_operand
*/
class VLSUOutput extends Bundle {
    val vd              = UInt(VLEN.W)
    val uopQueueIdx     = UInt(4.W) // magic number
}

class VLSUXcpt extends Bundle {
    val exception_vld   = Bool()
    val update_vl       = Bool()
    val update_data     = UInt(bVL.W)
    val xcpt_cause       = new HellaCacheExceptions()
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop            = Flipped(ValidIO(new Muop()(p)))
    val oldVd           = Input(UInt(VLEN.W))
    val lsuOut          = ValidIO(new VLSUOutput)
    val dataExchange    = new RVUMemory()
    val xcpt            = Output(new VLSUXcpt)
    val lsuReady        = Output(Bool())
}

object VRegSegmentStatus {
  val invalid :: needData :: notReady :: ready :: xcpt :: Nil = Enum(5)
}

object Mop {
    val unit_stride     = "b00".U
    val index_unodered  = "b01".U
    val constant_stride = "b10".U
    val index_ordered   = "b11".U
}

object VMemCmd {
    val read  = false.B
    val write = true.B
}

class LdstUop extends Bundle {
    val valid       = Bool()
    val addr        = Output(UInt(64.W))
    val queueIdx    = Output(UInt(4.W))
    val isLast      = Output(Bool())
}

class VRegSegmentInfo extends Bundle {
    // VRegSegmentStatus
    val status  = UInt(3.W)
    // corresponding ldstuop idx of current vreg segement
    val idx     = UInt(8.W)
    // offset of writeback valid data for current vreg segement
    val offset = UInt(log2Ceil(64).W)
    // data of current vreg segement
    val data    = UInt(8.W)
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
    val memElemBytesReg = RegInit(8.U)
    val eewbReg         = RegInit(64.U)
    val sewbReg         = RegInit(64.U) // for indexed load
    val uopVlReg        = RegInit(0.U(bVL.W))
    val elenReg         = RegInit(0.U(log2Ceil(VLEN / 8 + 1).W))
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

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
    val hellaXcptReg    = RegInit(0.U.asTypeOf(new HellaCacheExceptions))
    val mem_xcpt =  io.dataExchange.xcpt.pf.st ||
                    io.dataExchange.xcpt.pf.ld ||
                    io.dataExchange.xcpt.ae.st ||
                    io.dataExchange.xcpt.ae.ld ||
                    io.dataExchange.xcpt.ma.st ||
                    io.dataExchange.xcpt.ma.ld
    
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
        when((splitCount - 1.U === curSplitIdx) || mem_xcpt) {
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
            val ldstType = MuxCase(0.U, Array(
                (funct6(1, 0) === "b00".U) -> Mop.unit_stride,
                (funct6(1, 0) === "b01".U) -> Mop.index_unodered,
                (funct6(1, 0) === "b10".U) -> Mop.constant_stride,
                (funct6(1, 0) === "b11".U) -> Mop.index_ordered,
            ))
            ldstTypeReg := ldstType

            val memElemBytes = Mux(
                ldstType === Mop.index_ordered || ldstType === Mop.index_unodered,
                io.mUop.bits.uop.info.vsew,
                eewb
            )
            memElemBytesReg := memElemBytes
            sewbReg := io.mUop.bits.uop.info.vsew

            // decide micro vl
            val elen = (VLEN.U >> 3.U) / memElemBytes
            // val uopVl = Mux(
            //     uopIdx === 0.U && uopEnd, vl - vstart, 
            //     Mux(
            //         uopIdx === 0.U, elen - vstart,
            //         Mux(uopEnd, vl - (elen - vstart) - elen * (uopIdx - 1.U), elen)
            //     )
            // )


            // vstart > vl ??
            val uopVl = Mux(
                uopEnd, vl - elen * uopIdx, elen
            )

            val uopVstart = Mux(
                uopIdx === 0.U, Mux(vstart >= elen, elen, vstart),
                Mux(
                    vstart > elen * uopIdx, vstart - elen * uopIdx, 0.U
                )
            )
            elenReg  := elen
            uopVlReg := uopVl

            // Set split info
            ldstEnqPtr  := 0.U
            curSplitIdx := 0.U
            splitCount  := uopVl - uopVstart
            splitOffset := uopVstart

            for(i <- 0 until segNum) {
                vregInfo(i).data := Reverse(io.oldVd(8 * i + 7, 8 * i))

                when(i.U < uopVl && i.U >= uopVstart) {
                    for(j <- 0 until segNum) {
                        when(j.U < eewb) {
                            vregInfo(i.U * eewb + j.U).status := VRegSegmentStatus.needData
                        }
                    }
                }
            }
        }
    }.elsewhen(uopState === uop_split && !mem_xcpt) {
        // unit stride
        when(curSplitIdx < splitCount) {
            // align addr to memElemBytes
            val align2eewbAddr = (mUopReg.scalar_opnd_1 >> memElemBytesReg) << memElemBytesReg
            val addr = WireInit(0.U.asTypeOf(align2eewbAddr))

            when(ldstTypeReg === Mop.unit_stride) {
                val baseAddr = align2eewbAddr + (mUopReg.uop.uopIdx * elenReg + splitOffset) * memElemBytesReg
                addr := baseAddr  + curSplitIdx * memElemBytesReg

            }.elsewhen(ldstTypeReg === Mop.constant_stride) {
                when(mUopReg.scalar_opnd_2 === 0.U) {
                    // do someting
                }.otherwise {
                    var XLEN = 64
                    val strideNeg = mUopReg.scalar_opnd_2(XLEN - 1)
                    val strideAbs = Mux(strideNeg, -mUopReg.scalar_opnd_2, mUopReg.scalar_opnd_2) * memElemBytesReg
                    val elen = (VLEN.U >> 3.U) / memElemBytesReg
                    val baseAddr = Mux(
                        strideNeg,
                        align2eewbAddr - (mUopReg.uop.uopIdx * elenReg + splitOffset) * strideAbs,
                        align2eewbAddr + (mUopReg.uop.uopIdx * elenReg + splitOffset) * strideAbs
                    )
                    
                    addr := Mux(strideNeg,
                            baseAddr - curSplitIdx * strideAbs, 
                            baseAddr + curSplitIdx * strideAbs)
                }
            }.elsewhen(ldstTypeReg === Mop.index_ordered || ldstTypeReg === Mop.index_unodered) {
                // indexed addr
            }.otherwise {
                // do something 
            }

            // align addr to 64 bits
            val alignedAddr = (addr >> 3.U) << 3.U
            val offset = addr - alignedAddr

            ldstUopQueue(ldstEnqPtr).valid  := true.B
            ldstUopQueue(ldstEnqPtr).addr   := alignedAddr
            ldstUopQueue(ldstEnqPtr).isLast := (curSplitIdx === splitCount - 1.U)

            val baseSegIdx = (splitOffset + curSplitIdx) * memElemBytesReg;
            for(i <- 0 until segNum) {
                when(i.U < memElemBytesReg) {
                    val segIdx = baseSegIdx + i.U
                    vregInfo(segIdx).status := VRegSegmentStatus.notReady
                    vregInfo(segIdx).idx    := ldstEnqPtr
                    vregInfo(segIdx).offset := offset + i.U
                }
            }
            curSplitIdx := curSplitIdx + 1.U
            ldstEnqPtr  := ldstEnqPtr  + 1.U
        }
    }.elsewhen(uopState === uop_split && mem_xcpt) {
        ldstUopQueue.foreach(
            uop => uop.valid := false.B
        )
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

    when(dataExchangeState === ld_wait && io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data && !mem_xcpt) {
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
    }.elsewhen(dataExchangeState === ld_wait && io.dataExchange.resp.valid && mem_xcpt) {
        /**************************exception handling**********************************/
        val xcptQueueIdx = io.dataExchange.resp.bits.idx
        val completedIdx = PriorityEncoder(vregInfo.map(info => info.status === VRegSegmentStatus.notReady && info.idx === xcptQueueIdx)) / memElemBytesReg
        xcptVlReg       := mUopReg.uop.uopIdx * elenReg + completedIdx

        for(i <- 0 until segNum) {
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === issueLdstPtr) {
                vregInfo(i).status := VRegSegmentStatus.xcpt
            }
        }

        // for out-of-order use
        // val dataReady = true.B
        // for(i <- 0 until segNum) {
        //     when(i.U < issueLdstPtr) {
        //         when(vregInfo(i).status === VRegSegmentStatus.notReady) {
        //             dataReady := dataReady & false.B
        //         }
        //     }            
        // }

        hellaXcptReg := io.dataExchange.xcpt
        issueLdstPtr := 0.U
        
        completeLd := true.B
    }
    

    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_ready = Wire(Bool())
    vreg_wb_ready := vregInfo.forall(
        info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.invalid
    ) && !vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)
    val vreg_wb_xcpt = vregInfo.map(info => info.status === VRegSegmentStatus.xcpt).reduce(_ || _)

    when(vreg_wb_ready || vreg_wb_xcpt) {
        io.lsuOut.valid             := true.B
        // Concatenate data from all vregInfo elements)
        io.lsuOut.bits.vd           := Cat(vregInfo.reverseMap(entry => Reverse(entry.data)))
        io.lsuOut.bits.uopQueueIdx  := mUopReg.uop.uopIdx

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

    when(vreg_wb_xcpt) {
        io.xcpt.exception_vld   := true.B
        io.xcpt.update_vl       := true.B
        io.xcpt.update_data     := xcptVlReg
        io.xcpt.xcpt_cause      := hellaXcptReg
    }.otherwise {
        io.xcpt.exception_vld   := false.B
        io.xcpt.update_vl       := false.B
        io.xcpt.update_data     := DontCare
        io.xcpt.xcpt_cause      := 0.U.asTypeOf(new HellaCacheExceptions)
    }

}
