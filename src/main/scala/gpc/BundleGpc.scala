package gpc.core

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.{SFenceReq}//, BTBUpdate, BHTUpdate, RASUpdate, BTBResp}
import freechips.rocketchip.tile._
import freechips.rocketchip.util._

object VectorParam {
  val VLEN = 256
  val bVL = log2Up(VLEN) + 1
  val bVstart = bVL - 1
  val VScoreboardSize = 32
  val bVScoreboardId = log2Up(VScoreboardSize) + 1 // 1-bit flag
  val VecMemAddrWidth = 64
}
object DebugParam {
  val debug = false
  val RobSize = 64
  val RobIdxWidth = log2Up(RobSize)
}
import DebugParam._
import VectorParam._

class SUOp(implicit p: Parameters) extends CoreBundle {
  val ctrl = new IntCtrlSigs
  val inst = UInt(32.W)
  val pipe0 = Bool()
  val pipe1 = Bool()
  val pc = UInt(vaddrBitsExtended.W)
  val rvc = Bool()
  val replay = Bool()
  val xcpt_noIntrp = Bool() // Exception that is not an interrupt
  val interrupt = Bool()
  val rs_ready = Vec(2, Bool())
  val wdata_ready = Bool()
  val rxs1 = Bool()
  val rxs2 = Bool()
  val wfd = Bool()
  
  def onlyPipe0 = pipe0 && !pipe1
  def onlyPipe1 = pipe1 && !pipe0

  val btb_resp = new BTBResp

  val sb_id = UInt(6.W) // For VPU
  val robIdx = debug.option(UInt(RobIdxWidth.W))

  def rs1 = inst(19, 15)
  def rs2 = inst(24, 20)
  def rs3 = inst(31, 27)
  def rd = inst(11, 7)

  val vec_config = Bool()
  val vec_config_case = UInt(3.W) // Cat(vsetvli, vsetivli, vsetvl)
  val vec_arith = Bool()
  val vec_load = Bool()
  val vec_store = Bool()
  val vec = Bool() // vec_arith || vec_load || vec_store
}

// //---- Frontend - Mainpipe IO ----
// class FrontendIOGpc(implicit p: Parameters) extends CoreBundle()(p) {
//     val might_request = Output(Bool())
//     val req = Valid(new FrontendReqGpc)
//     val sfence = Valid(new SFenceReq)
//     val resp = Flipped(Vec(2, Decoupled(new FrontendRespGpc)))
//     val btb_update = Valid(new BTBUpdate)
//     val bht_update = Valid(new BHTUpdate)
//     val ras_update = Valid(new RASUpdate)
//     val flush_icache = Output(Bool())
//     val perf = Input(new FrontendPerfEventsGpc())
//     val progress = Output(Bool())
// }

// class FrontendPerfEventsGpc extends Bundle {
//     val acquire = Bool()
//     val tlbMiss = Bool()
// }

// class FrontendRespGpc(implicit p: Parameters) extends CoreBundle()(p) {
//     val btb = new BTBResp
//     val pc = UInt(vaddrBitsExtended.W)  
//     val inst = UInt(32.W)
//     val raw_inst = UInt(32.W)
//     val rvc = Bool()
//     val xcpt = new FrontendExceptionsGpc
//     val replay = Bool()
//     val next_pc = UInt(vaddrBitsExtended.W)
// }

// class FrontendReqGpc(implicit p: Parameters) extends CoreBundle()(p) {
//     val pc = UInt(vaddrBitsExtended.W)
//     val speculative = Bool()
// }

// class FrontendExceptionsGpc extends Bundle {
//     val pf = new Bundle {
//     val inst = Bool()
//     }
//     val ae = new Bundle {
//     val inst = Bool()
//     }
// }

// Vector
class VCsr extends Bundle {
  val vstart = UInt(bVstart.W)
  val vl = UInt(bVL.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
  val vill = Bool()
  val vma = Bool()
  val vta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
}

class VIssue(implicit p: Parameters) extends CoreBundle {
  val valid = Bool()
  val vsb_id = UInt(bVScoreboardId.W)
  val inst = UInt(32.W)
  val rs1 = UInt(xLen.W)
  val rs2 = UInt(xLen.W)
  val vcsr = new VCsr
}

class VLdstFlag extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class VLdstXcpt extends Bundle {
  val ma = new VLdstFlag
  val pf = new VLdstFlag
  val gf = new VLdstFlag
  val ae = new VLdstFlag
}

class VIllegal extends Bundle {
  val valid = Output(Bool())
  val vsb_id = Output(UInt(bVScoreboardId.W))
  val not_illegal = Output(Bool())
}

// From perspective of VPU
class VComplete(implicit p: Parameters) extends CoreBundle {
  val valid = Output(Bool())
  val vsb_id = Output(UInt(bVScoreboardId.W))
  val wen_int = Output(Bool())
  val wen_fp = Output(Bool())
  val wdata = Output(UInt(xLen.W))
  val wdata_reg_idx = Output(UInt(5.W))
  val illegal_inst = Output(Bool())
  val update_vl = Output(Bool())
  val update_vl_data = Output(UInt(bVL.W))
  val mem_xcpt_valid = Output(Bool())
  val mem_xcpt_cause = Output(new VLdstXcpt)
  val xcpt_addr = Output(UInt(VecMemAddrWidth.W))
  val vxsat = Output(Bool())
  val fflags = Output(UInt(5.W))
}
class VWritebackReady extends Bundle {
  val wb_int_ready = Input(Bool())
  val wb_fp_ready = Input(Bool())
}