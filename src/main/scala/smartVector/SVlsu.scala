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
    val vd              = UInt(VLEN.W)
    val uopQueueIdx     = UInt(3.W) // magic number
}

class VLSUXcpt extends Bundle {
    val exception_vld   = Bool()
    val update_vl       = Bool()
    val update_data     = UInt(bVL.W)
    val xcpt_cause      = new HellaCacheExceptions()
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop            = Flipped(ValidIO(new Muop()(p)))
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
    val valid       = Bool()
    val addr        = Output(UInt(64.W))
    val firstvl     = Output(UInt(bVL.W))
    val isLast      = Output(Bool())
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
    val unitSMopReg     = RegInit(0.U(5.W))
    val memwbReg        = RegInit(8.U)
    val eewbReg         = RegInit(8.U)
    val memwAlignReg    = RegInit(8.U)
    val elenReg         = RegInit(0.U(vlenbWidth.W))
    val mlenReg         = RegInit(0.U(vlenbWidth.W))
    val ldstTypeReg     = RegInit(0.U(2.W))
    val vmReg           = RegInit(true.B)

    // vreg seg info
    val vregInfo        = RegInit(VecInit(Seq.fill(vlenb)(0.U.asTypeOf(new VRegSegmentInfo))))

    // Split info
    val splitCount      = RegInit(0.U(vlenbWidth.W))
    val curSplitIdx     = RegInit(0.U(vlenbWidth.W))
    val splitStart      = RegInit(0.U(vlenbWidth.W))

    // ldQueue
    val ldstEnqPtr      = RegInit(0.U(ldUopQueueWidth.W))
    val issueLdstPtr    = RegInit(0.U(ldUopQueueWidth.W))
    val ldstUopQueue    = RegInit(VecInit(Seq.fill(ldUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    // xcpt info
    val xcptVlReg       = RegInit(0.U(bVL.W))
    val hellaXcptReg    = RegInit(0.U.asTypeOf(new HellaCacheExceptions))

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
        when(io.mUop.valid) {
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
    
    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            mUopReg := io.mUop.bits

            val (funct6, funct3) = (io.mUop.bits.uop.ctrl.funct6, io.mUop.bits.uop.ctrl.funct3)
            val (vstart, vl)     = (io.mUop.bits.uop.info.vstart, io.mUop.bits.uop.info.vl)
            val (uopIdx, uopEnd) = (io.mUop.bits.uop.uopIdx, io.mUop.bits.uop.uopEnd)
            val (vsew, vm)       = (io.mUop.bits.uop.info.vsew, io.mUop.bits.uop.ctrl.vm)
            val unitStrideMop    = io.mUop.bits.uop.ctrl.lsrc(1)
            
            // eew in bytes 
            val mwCatWidth = Cat(funct6(2), funct3)
            val eewb = MuxCase(1.U, Array(
                (mwCatWidth === "b0000".U) -> 1.U,
                (mwCatWidth === "b0101".U) -> 2.U,
                (mwCatWidth === "b0110".U) -> 4.U,
                (mwCatWidth === "b0111".U) -> 8.U,
            ))
            
            // sew in bytes
            val sewb = MuxCase(1.U, Array(
                (vsew === "b000".U) -> 1.U,
                (vsew === "b001".U) -> 2.U,
                (vsew === "b010".U) -> 4.U,
                (vsew === "b011".U) -> 8.U,
            ))

            // ldst type
            val ldstType = MuxCase(0.U, Array(
                (funct6(1, 0) === "b00".U) -> Mop.unit_stride,
                (funct6(1, 0) === "b01".U) -> Mop.index_unodered,
                (funct6(1, 0) === "b10".U) -> Mop.constant_stride,
                (funct6(1, 0) === "b11".U) -> Mop.index_ordered,
            ))

            // unit-stride & strided use eew as memwb
            // indexed use sew
            val memwb = Mux(ldstType === Mop.index_ordered || ldstType === Mop.index_unodered, sewb, eewb)

            memwAlignReg := MuxCase(1.U, Array(
                (memwb === 1.U) -> 0.U,
                (memwb === 2.U) -> 1.U,
                (memwb === 4.U) -> 2.U,
                (memwb === 8.U) -> 3.U,
            ))

            val mlen = (VLEN.U >> 3.U) / memwb
            val elen = (VLEN.U >> 3.U) / eewb

            unitSMopReg := unitStrideMop
            vmReg       := vm
            eewbReg     := eewb
            ldstTypeReg := ldstType
            memwbReg    := memwb
            elenReg     := elen
            mlenReg     := mlen

            val minLen = Mux(elen < mlen, elen, mlen)
            // decide micro vl
            // vstart > vl ??
            val actualVl = Mux(unitStrideMop === UnitStrideMop.mask, (vl + 7.U) >> 3.U, vl) // ceil(vl/8)
            val leftLen  = actualVl - minLen * uopIdx
            val microVl  = Mux(uopEnd, leftLen, minLen)

            val microVStart = Mux(vstart < minLen * uopIdx, 0.U, 
                Mux(vstart - minLen * uopIdx > minLen, minLen, vstart - minLen * uopIdx)
            )

            // Set split info
            ldstEnqPtr   := 0.U
            issueLdstPtr := 0.U
            curSplitIdx  := 0.U
            splitCount   := microVl - microVStart      
            splitStart   := microVStart


            // set vreg
            val vregClean = vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)

            when(vregClean) {
                val memVl = Mux(leftLen < mlen, leftLen, mlen)
                val memVstart = Mux(vstart < minLen * uopIdx, 
                    0.U, 
                    Mux(vstart - minLen * uopIdx > mlen, mlen, vstart - minLen * uopIdx)
                )

                for(i <- 0 until vlenb) {
                    vregInfo(i).data := Reverse(io.mUop.bits.uopRegInfo.old_vd(8 * i + 7, 8 * i))

                    when(i.U < memVl && i.U >= memVstart) {
                        for(j <- 0 until vlenb) {
                            when(j.U < memwb) {
                                vregInfo(i.U * memwb + j.U).status := VRegSegmentStatus.needData
                            }
                        }
                    }
                }
            }
        }
    }.elsewhen(uopState === uop_split && !mem_xcpt) {
        when(curSplitIdx < splitCount) {
            val curVl       = mUopReg.uop.uopIdx * Mux(elenReg < mlenReg, elenReg, mlenReg) + splitStart + curSplitIdx
            val addr        = WireInit(0.U(64.W))
            val offset      = WireInit(0.U(log2Ceil(8).W))
            val baseSegIdx  = (curVl % mlenReg) * memwbReg

            val maskVal = mUopReg.uopRegInfo.mask(curVl)

            when(vmReg || maskVal) {
                /*-----------------------------------------calc addr start-------------------------------------------------*/
                /*                                                                                                         */
                val baseAddr = mUopReg.scalar_opnd_1
                when(ldstTypeReg === Mop.unit_stride) {
                    // align addr to memwb
                    addr := ((baseAddr + curVl * memwbReg) >> memwAlignReg) << memwAlignReg
                }.elsewhen(ldstTypeReg === Mop.constant_stride) {
                    when(mUopReg.scalar_opnd_2 === 0.U) {
                        addr := (baseAddr >> memwAlignReg) << memwAlignReg
                    }.otherwise {
                        val strideNeg = mUopReg.scalar_opnd_2(XLEN - 1)
                        val strideAbs = Mux(strideNeg, -mUopReg.scalar_opnd_2, mUopReg.scalar_opnd_2)

                        addr := (Mux(strideNeg, baseAddr - curVl * strideAbs, baseAddr + curVl * strideAbs) >> memwAlignReg) << memwAlignReg
                    }
                }.elsewhen(ldstTypeReg === Mop.index_ordered || ldstTypeReg === Mop.index_unodered) {
                    // indexed addr
                    val idxVal = WireInit(0.U(XLEN.W))
                    val remain = WireInit(0.U(XLEN.W))
                    val rShiftVal = WireInit(0.U(VLEN.W))
                    val eew = eewbReg << 3.U

                    val beginIdx = (curVl % elenReg) * (eew)

                    rShiftVal := (mUopReg.uopRegInfo.vs2 >> beginIdx)
                    remain := ((1.U << eew) - 1.U)
                    idxVal := (mUopReg.uopRegInfo.vs2 >> beginIdx) & remain
                    val idxNeg = idxVal(XLEN - 1)
                    val idxAbs = Mux(idxNeg, -idxVal, idxVal)
                    
                    addr := (Mux(idxNeg, baseAddr - idxAbs, baseAddr + idxAbs) >> memwAlignReg) << memwAlignReg
                }.otherwise {
                    // do something
                }

                // align addr to 64 bits
                val alignedAddr = (addr >> 3.U) << 3.U
                offset := addr - alignedAddr
                /*                                                                                                         */
                /*-----------------------------------------calc addr end---------------------------------------------------*/

                ldstUopQueue(ldstEnqPtr).valid   := true.B
                ldstUopQueue(ldstEnqPtr).addr    := alignedAddr
                ldstUopQueue(ldstEnqPtr).isLast  := (curSplitIdx === splitCount - 1.U)
                ldstUopQueue(ldstEnqPtr).firstvl := curVl

                ldstEnqPtr  := ldstEnqPtr  + 1.U
            }

            for(i <- 0 until vlenb) {
                when(i.U < memwbReg) {
                    val segIdx = baseSegIdx + i.U
                    when(vmReg || maskVal) {
                        vregInfo(segIdx).status := VRegSegmentStatus.notReady
                        vregInfo(segIdx).idx    := ldstEnqPtr
                        vregInfo(segIdx).offset := offset + i.U
                    }.otherwise {
                        vregInfo(segIdx).status := VRegSegmentStatus.ready
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
        issueLdstPtr := io.dataExchange.resp.bits.idx
    }.elsewhen(ldstUopQueue(issueLdstPtr).valid && io.dataExchange.req.ready) {
        issueLdstPtr := issueLdstPtr + 1.U
    }

    when(ldstUopQueue(issueLdstPtr).valid && io.dataExchange.req.ready) {
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

    when(mem_xcpt) {
        ldstUopQueue.foreach(
            uop => uop.valid := false.B
        )
    }
    /*---------------------------------ISSUE END---------------------------------*/


    /*********************************RESP START*********************************/
    when(io.dataExchange.resp.valid && io.dataExchange.resp.bits.has_data && !mem_xcpt) {
        val loadData  = io.dataExchange.resp.bits.data
        val respLdPtr = io.dataExchange.resp.bits.idx

        ldstUopQueue(respLdPtr).valid := false.B
        
        for(i <- 0 until vlenb) {
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === respLdPtr) {
                for(j <- 0 until 8) {
                    when(vregInfo(i).offset === j.U) {
                        vregInfo(i).data := Reverse(loadData(j * 8 + 7, j * 8))
                    }
                }
                vregInfo(i).status := VRegSegmentStatus.ready
            }
        }
    }.elsewhen(mem_xcpt) { // exception handling
        val xcptQueueIdx = io.dataExchange.resp.bits.idx
        xcptVlReg       := ldstUopQueue(xcptQueueIdx).firstvl
        hellaXcptReg    := io.dataExchange.xcpt

        for(i <- 0 until vlenb) {
            when(vregInfo(i).status === VRegSegmentStatus.notReady && vregInfo(i).idx === xcptQueueIdx) {
                vregInfo(i).status := VRegSegmentStatus.xcpt
            }
        }
    }

    completeLd := ldstUopQueue.forall(uop => uop.valid === false.B) // ld completed or xcpt happened
    
    /*---------------------------------RESP END---------------------------------*/

    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_xcpt  = vregInfo.map(info => info.status === VRegSegmentStatus.xcpt).reduce(_ || _)
    val vreg_wb_ready = vregInfo.forall(info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.invalid) && 
        !vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)

    when(vreg_wb_ready || vreg_wb_xcpt) {
        io.lsuOut.valid             := true.B
        io.lsuOut.bits.vd           := Cat(vregInfo.reverseMap(entry => Reverse(entry.data))) // Concatenate data from all vregInfo elements)
        io.lsuOut.bits.uopQueueIdx  := mUopReg.uop.uopIdx

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
