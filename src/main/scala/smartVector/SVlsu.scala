package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import darecreek.exu.vfu.VUop
import xiangshan.MicroOp
import SmartParam._

class VLSUOutput extends Bundle {
    val vd = UInt(VLEN.W)
    val uopQueueIdx = UInt(4.W) // magic number
    val vxsat = Bool()
}

class LdstIO(implicit tileParams: Parameters, p: Parameters) extends ParameterizedBundle()(p) {
    val mUop = Flipped(ValidIO(new UopQueueOutput()(p)))
    val lsuOut = ValidIO(new VLSUOutput)
    val hellacache = new freechips.rocketchip.rocket.HellaCacheIO()(tileParams)
    val lsuReady = Output(Bool())
}

class LdstUop extends Bundle {
    val valid = Bool()
    val addr = Output(UInt(64.W))
    val effecSize = Output(UInt(3.W))
    val offset = Output(UInt(6.W))
}

object VRegSegmentStatus {
  val invalid :: notReady :: ready :: Nil = Enum(3)
}

class VRegSegmentInfo extends Bundle {
    // 0: not ready, 1: ready
    val status = UInt(log2Ceil(3).W)
    // corresponding ldstuop idx of current vreg segement
    val idx = UInt(8.W)
    // offset of writeback valid data for current vreg segement
    val offset = UInt(6.W)
    // data of current vreg segement
    val data = UInt(8.W)
}

class SVlsu(implicit tileParams: Parameters, p: Parameters) extends Module {
    val io = IO(new LdstIO()(tileParams, p))

    val uop_idle :: uop_split :: uop_split_finish :: uop_complete :: Nil = Enum(4)
    val uopState = RegInit(uop_idle)
    val completeLd = RegInit(false.B)

    when(uopState === uop_idle) {
        // LSU free --> accept new uop request
        io.lsuReady := true.B
    }.otherwise {
        io.lsuReady := false.B
    }

    // define v reg info
    var ldUopSize = 16
    val ldUopIdxBits = log2Up(ldUopSize)
    val vRegIdx = Wire(UInt(5.W))
    // val vreg_info = VecInit(Seq.fill(16)(VRegSegmentInfo()))
    val vreg_info = Reg(Vec(16, new VRegSegmentInfo))

    // SPLIT FSM
    when(uopState === uop_idle) {
        when(io.mUop.valid) {
            uopState := uop_split
        }.otherwise {
            uopState := uop_idle
        }
    }.elsewhen(uopState === uop_split) {
        uopState := uop_split_finish
    }.elsewhen(uopState === uop_split_finish) {
        when(completeLd) {
            uopState := uop_complete
        }.otherwise {
            uopState := uop_split_finish
        }   
    }.otherwise {
        uopState := uop_idle
    }

    /**********************SPLIT STAGE**************************/
    // stage 0 IDLE --> stage 1 SPLIT (start to split) --> stage 2 SPLIT_FINISH (split finish) --> stage 3 COMPLETE (writeback to uopQueue)
    vRegIdx := io.mUop.bits.uopAttribute.ldest
    val s1_mUop = RegNext(io.mUop)

    val ldstEnqPtr = RegInit(0.U(ldUopIdxBits.W))
    val ldstUopQueue = Reg(Vec(ldUopSize, new LdstUop))

    when(uopState === uop_split) {
        // unit stride
        ldstUopQueue(ldstEnqPtr).valid := true.B
        ldstUopQueue(ldstEnqPtr).addr := s1_mUop.bits.scalar_opnd_1
        // split into lduop queue & write vreginfo
        for(i <- 0 until 16) {
            vreg_info(i).status := VRegSegmentStatus.notReady
            vreg_info(i).idx := ldstEnqPtr
            vreg_info(i).offset := i.U
        }
        ldstEnqPtr := ldstEnqPtr + 1.U
    }
    
    /*-----------------SPLIT STAGE END-----------------------*/


    /******************ISSUE STAGE START**********************/
    // issue stage
    // issue lduop to hellacache

    val issueLdstUop = WireInit(0.U.asTypeOf(new LdstUop))
    val issueLdstPtr = RegInit(0.U(ldUopIdxBits.W))

    val ld_idle :: ld_issue :: ld_wait :: ld_complete :: ld_replay :: Nil = Enum(5)
    
    //prepare hellacache req
    val hellacacheState = RegInit(ld_idle)

    when(hellacacheState === ld_idle) {
        // ldstuop queue has data & hellacache is not busy
        when(ldstUopQueue(issueLdstPtr).valid && !io.hellacache.ordered) {
            hellacacheState := ld_issue
        }.otherwise {
            hellacacheState := ld_idle
        }
    }.elsewhen(hellacacheState === ld_issue) {
        hellacacheState := ld_wait
    }.elsewhen(hellacacheState === ld_wait) {
        when(io.hellacache.resp.valid && io.hellacache.resp.bits.has_data) {
            hellacacheState := ld_complete
        } .elsewhen(io.hellacache.resp.valid && io.hellacache.resp.bits.replay) {
            hellacacheState := ld_replay
        }.otherwise {
            hellacacheState := ld_wait
        }
    }.elsewhen(hellacacheState === ld_complete) {
        hellacacheState := ld_idle
    }.elsewhen(hellacacheState === ld_replay) {
        hellacacheState := ld_issue
    }.otherwise {
        hellacacheState := ld_idle
    }

    when(hellacacheState === ld_issue) {
        issueLdstUop := ldstUopQueue(issueLdstPtr)
        io.hellacache.req.valid := true.B
        io.hellacache.req.bits.addr := issueLdstUop.addr
        // io.hellacache.req.bits.idx := 0.U
        io.hellacache.req.bits.tag := 0.U
        io.hellacache.req.bits.cmd := M_XRD
        io.hellacache.req.bits.size := log2Ceil(64).U
        io.hellacache.req.bits.signed := false.B
        io.hellacache.req.bits.dprv := PRV.S.U
        io.hellacache.req.bits.dv := false.B
        
        io.hellacache.req.bits.phys := false.B
        io.hellacache.req.bits.no_xcpt := false.B
        io.hellacache.req.bits.no_alloc := false.B

        io.hellacache.req.bits.data := 0.U
        io.hellacache.req.bits.mask := 0.U


        io.hellacache.s1_kill := false.B
        io.hellacache.s1_data.mask := 0.U
        io.hellacache.s1_data.data := 0.U
        io.hellacache.s2_kill := false.B

        io.hellacache.keep_clock_enabled := true.B

    }.otherwise {
        io.hellacache.req.valid := false.B
        io.hellacache.req.bits := DontCare
        io.hellacache.s2_kill := false.B
        io.hellacache.s1_kill := false.B
        io.hellacache.s1_data.mask := 0.U
        io.hellacache.s1_data.data := 0.U
        io.hellacache.keep_clock_enabled := true.B
    }

     

    /************************cache hit***********************/
    // cache hit after 2 cycle
    // data writeback from cache

    /*todo
        * 1. write data into dest reg
        * 2. deq lduop
    */
    when(hellacacheState === ld_wait && io.hellacache.resp.valid && io.hellacache.resp.bits.has_data) {
        val loadData = io.hellacache.resp.bits.data
        for(i <- 0 until 16) {
            when(vreg_info(i).idx === issueLdstPtr) {
                for(j <- 0 until 8) {
                    when(vreg_info(i).offset === j.U) {
                        vreg_info(i).data := loadData(j * 8 + 7, j * 8)
                    }
                }
                vreg_info(i).status := VRegSegmentStatus.ready
            }
        }
        issueLdstPtr := issueLdstPtr + 1.U
        completeLd := true.B
    }
    
    /**************************exception handling**********************************/


    /**************************cache miss replay***********************************/



    /************************** Ldest data writeback to uopQueue********************/
    val vreg_wb_ready = Wire(Bool())
    vreg_wb_ready := vreg_info.map(_.status === VRegSegmentStatus.ready).reduce(_ && _)

    when(vreg_wb_ready) {
        io.lsuOut.valid := true.B
        io.lsuOut.bits.vd := Cat(vreg_info.map(_.data)) // Concatenate data from all vreg_info elements
        io.lsuOut.bits.uopQueueIdx := 0.U // Adjust this value based on your design
        io.lsuOut.bits.vxsat := false.B

        // Reset vreg info
        for (i <- 0 until 16) {
            vreg_info(i).status := VRegSegmentStatus.invalid
            vreg_info(i).idx := DontCare
            vreg_info(i).offset := DontCare
            vreg_info(i).data := DontCare
        }
    }.otherwise {
        io.lsuOut.valid := false.B
        io.lsuOut.bits := DontCare // You may want to provide default values for other bits
    }

}
