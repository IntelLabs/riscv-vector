package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VUop
import xiangshan.MicroOp
import SmartParam._

class VLSUOutput extends Bundle {
    val vd = UInt(VLEN.W)
    val uopQueueIdx = UInt(4.W) // magic number
    val vxsat = Bool()
}

class VLSUXcpt extends Bundle {
    val valid = Bool()
    val xcptElemIdx = UInt(4.W)
}

class LdstIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val mUop = Flipped(ValidIO(new UopQueueOutput()(p)))
    val oldVd = Input(UInt(VLEN.W))
    val lsuOut = ValidIO(new VLSUOutput)
    val dataExchange = new RVUMemory()
    val xcpt = Output(new VLSUXcpt)
    val lsuReady = Output(Bool())
}

class LdstUop extends Bundle {
    val valid = Bool()
    val addr = Output(UInt(64.W))
    val queueIdx = Output(UInt(4.W))
    val isLast = Output(Bool())
}

object VRegSegmentStatus {
  val invalid :: needData :: notReady :: ready :: Nil = Enum(4)
}

class VRegSegmentInfo extends Bundle {
    // 0: not ready, 1: ready
    val status = UInt(2.W)
    // corresponding ldstuop idx of current vreg segement
    val idx = UInt(8.W)
    // offset of writeback valid data for current vreg segement
    val offset = UInt(6.W)
    // data of current vreg segement
    val data = UInt(8.W)
}

class SVlsu(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    
    val uop_idle :: uop_split :: uop_split_finish :: Nil = Enum(3)
    val uopState = RegInit(uop_idle)
    val nextUopState = WireInit(uop_idle)
    val completeLd = RegInit(false.B)

    when(nextUopState === uop_idle) {
        // LSU free --> accept new uop request
        io.lsuReady := true.B
    }.otherwise {
        io.lsuReady := false.B
    }

    val mUopReg = RegInit(0.U.asTypeOf(new UopQueueOutput()(p)))

    // define v reg info
    var ldUopQueueSize = 16
    val ldUopIdxBits = log2Up(ldUopQueueSize)
    val vRegIdx = Wire(UInt(5.W))

    val vregSegCount = VLEN / 8
    val vregInfo = RegInit(VecInit(Seq.fill(vregSegCount)(0.U.asTypeOf(new VRegSegmentInfo))))

    /**********************SPLIT STAGE**************************/
    // stage 0 IDLE --> stage 1 SPLIT (start to split) --> stage 2 SPLIT_FINISH (split finish) --> stage 3 COMPLETE (writeback to uopQueue)
    
    val splitCount = RegInit(0.U(5.W))
    val curSplitIdx = RegInit(0.U(5.W))
    // SPLIT FSM
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
    
    uopState := nextUopState

    vRegIdx := io.mUop.bits.uopAttribute.ldest

    val ldstEnqPtr = RegInit(0.U(ldUopIdxBits.W))
    val ldstDeqPtr = RegInit(0.U(ldUopIdxBits.W))

    val ldstUopQueue = RegInit(VecInit(Seq.fill(ldUopQueueSize)(0.U.asTypeOf(new LdstUop))))

    def findEntryWithAddr(ldstUopQueue: Vec[LdstUop], alignedAddr: UInt): (Bool, UInt) = {
        val matches = ldstUopQueue.zipWithIndex.map { case (entry, i) =>
            entry.valid && entry.addr === alignedAddr
        }

        val validMatch = matches.reduce(_ || _)
        val indexMatch = Mux1H(matches, (0 until ldstUopQueue.length).map(_.U))

        (validMatch, indexMatch)
    }


    // vstart -->  
    // vend --> 
    // vl = VLEN/eew
    val vl = 8.U
    val eew = 8.U

    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            mUopReg := io.mUop.bits
            // Set split info
            curSplitIdx := 0.U
            splitCount := vl
            ldstEnqPtr := 0.U
            val eewInBytes = eew >> 3.U

            for(i <- 0 until 16) {
                vregInfo(i).data := io.oldVd(8 * i + 7, 8 * i)

                when(i.U < vl) {
                    for(j <- 0 until 8) {
                        when(j.U < eewInBytes) {
                            vregInfo(i.U * eewInBytes + j.U).status := VRegSegmentStatus.needData
                        }
                    }
                }
            }
        }
    }.elsewhen(uopState === uop_split) {
        // unit stride
        when(curSplitIdx < splitCount) {
            val eewInBytes = eew >> 3.U // align to eew
            val addr = ((mUopReg.scalar_opnd_1 >> eewInBytes) << eewInBytes)  + curSplitIdx * eewInBytes
            val alignedAddr = (addr >> 3.U) << 3.U
            val offset = addr - alignedAddr

            ldstUopQueue(ldstEnqPtr).valid := true.B
            ldstUopQueue(ldstEnqPtr).addr := alignedAddr
            ldstUopQueue(ldstEnqPtr).isLast := (curSplitIdx === splitCount - 1.U)

            for(i <- 0 until 16) {
                when(i.U < eewInBytes) {
                    vregInfo(curSplitIdx * eewInBytes + i.U).status := VRegSegmentStatus.notReady
                    vregInfo(curSplitIdx * eewInBytes + i.U).idx := ldstEnqPtr
                    vregInfo(curSplitIdx * eewInBytes + i.U).offset := offset + i.U
                }
            }
            curSplitIdx := curSplitIdx + 1.U
            ldstEnqPtr := ldstEnqPtr + 1.U
        }
    }
    
    /*-----------------SPLIT STAGE END-----------------------*/


    /******************ISSUE STAGE START**********************/
    // issue stage
    // issue lduop to hellacache

    val issueLdstUop = WireInit(0.U.asTypeOf(new LdstUop))
    val issueLdstPtr = RegInit(0.U(ldUopIdxBits.W))

    // idle issue  
    val ld_issue :: ld_wait :: Nil = Enum(2)
    
    //prepare hellacache req
    val dataExchangeState = RegInit(ld_issue)
    val nextDataExchangeState = WireInit(ld_issue)

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

    dataExchangeState := nextDataExchangeState

    when(dataExchangeState === ld_issue && ldstUopQueue(issueLdstPtr).valid && io.dataExchange.req.ready) {
        issueLdstUop := ldstUopQueue(issueLdstPtr)
        io.dataExchange.req.valid := true.B
        io.dataExchange.req.bits.addr := issueLdstUop.addr
        io.dataExchange.req.bits.cmd := M_XRD
        io.dataExchange.req.bits.size := 8.U
        io.dataExchange.req.bits.signed := false.B
        io.dataExchange.req.bits.phys := false.B
        io.dataExchange.req.bits.idx := issueLdstPtr
        io.dataExchange.req.bits.data := DontCare
        io.dataExchange.req.bits.mask := DontCare

    }.otherwise {
        io.dataExchange.req.valid := false.B
        io.dataExchange.req.bits := DontCare
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
        for(i <- 0 until 16) {
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
    when(dataExchangeState === ld_wait && io.dataExchange.resp.valid && io.dataExchange.xcpt) {
        io.xcpt.valid := true.B
        io.xcpt.xcptElemIdx := 0.U
    }.otherwise {
        io.xcpt.valid := false.B
        io.xcpt.xcptElemIdx := DontCare
    }

    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_ready = Wire(Bool())
    vreg_wb_ready := vregInfo.forall(
        info => info.status === VRegSegmentStatus.ready || info.status === VRegSegmentStatus.invalid
    ) && !vregInfo.forall(info => info.status === VRegSegmentStatus.invalid)

    when(vreg_wb_ready) {
        io.lsuOut.valid := true.B
        io.lsuOut.bits.vd := Cat(vregInfo.reverseMap(entry => Reverse(entry.data))) // Concatenate data from all vregInfo elements)
        io.lsuOut.bits.uopQueueIdx := mUopReg.uop.uopIdx
        io.lsuOut.bits.vxsat := false.B

        // Reset vreg info
        for (i <- 0 until 16) {
            vregInfo(i).status := VRegSegmentStatus.invalid
            vregInfo(i).idx := DontCare
            vregInfo(i).offset := DontCare
            vregInfo(i).data := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits := DontCare
    }

}
