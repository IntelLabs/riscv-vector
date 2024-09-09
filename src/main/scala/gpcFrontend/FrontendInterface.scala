package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.{withClock,withReset}
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLWidthWidget}
import freechips.rocketchip.util._
import freechips.rocketchip.util.property


class FrontendBundle(val outer: Frontend) extends CoreBundle()(outer.p) {
    val cpu = Flipped(new FrontendIO())
    val ptw = new TLBPTWIO()
    val errors = new ICacheErrors
}


class FrontendIO(implicit p: Parameters) extends CoreBundle()(p) {
    val might_request = Output(Bool())
    val req = Valid(new FrontendReq)
    val sfence = Valid(new SFenceReq)
    val resp = Flipped(Vec(decodeWidth, Decoupled(new FrontendResp)))
    val btb_update = Valid(new BTBUpdate)
    val bht_update = Valid(new BHTUpdate)
    val ras_update = Valid(new RASUpdate)
    val flush_icache = Output(Bool())
    val perf = Input(new FrontendPerfEvents())
    val progress = Output(Bool())
}

class FrontendPerfEvents extends Bundle {
    val acquire = Bool()
    val tlbMiss = Bool()
}

class FrontendResp(implicit p: Parameters) extends CoreBundle()(p) {
    val btb = new BTBResp
    val pc = UInt(vaddrBitsExtended.W)
    val next_pc = UInt(vaddrBitsExtended.W)
    val inst = new ExpandedInstruction
    val raw_inst = UInt(32.W)
    val rvc = Bool()
    val xcpt = new FrontendExceptions
    val replay = Bool()
}

class FrontendReq(implicit p: Parameters) extends CoreBundle()(p) {
    val pc = UInt(vaddrBitsExtended.W)
    val speculative = Bool()
}

class FrontendExceptions extends Bundle {
    val pf = new Bundle {
    val inst = Bool()
    }
    val ae = new Bundle {
    val inst = Bool()
    }
}

