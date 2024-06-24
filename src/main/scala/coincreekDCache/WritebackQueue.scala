package coincreekDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class WritebackReq(params: TLBundleParameters) extends Bundle {
  val voluntary = Bool()
  val addr      = UInt(paddrWidth.W)
  val param     = UInt(5.W)
  val source    = UInt(params.sourceBits.W) // tilelink source
  val hasData   = Bool()
  val data      = UInt(dataWidth.W)
}

class MissCheck extends Bundle {
  val valid     = Input(Bool())
  val addr      = Input(UInt(paddrWidth.W))
  val blockMiss = Output(Bool())
}

class WriteBackQueue(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module {
  val io = IO(new Bundle {
    val req       = Flipped(Decoupled(new WritebackReq(edge.bundle)))
    val missCheck = new MissCheck()
    val release   = Decoupled(new TLBundleC(edge.bundle))
    val grant     = Flipped(Decoupled(new TLBundleD(edge.bundle)))
  })

  def getBeatData(count: UInt, data: UInt): UInt = {
    val beatData = Wire(UInt(beatBits.W))
    beatData := data >> (count << log2Up(beatBits))
    beatData(beatBits - 1, 0)
  }

  val s_invalid :: s_release_req :: s_release_resp :: Nil = Enum(3)

  val state     = RegInit(s_invalid)
  val nextState = WireInit(s_invalid)

  val req         = RegEnable(io.req.bits, io.req.fire)
  val count       = RegInit(0.U(log2Up(refillCycles).W))
  val curBeatData = getBeatData(count, req.data)
  // val allBeatDone = count === (refillCycles - 1).U

  val (_, _, allBeatDone, _) = edge.count(io.release)

  // * FSM transition Begin

  switch(state) {
    is(s_invalid) {
      when(io.req.fire) {
        state := s_release_req
      }
    }
    is(s_release_req) {
      when(allBeatDone) {
        when(req.hasData) {
          state := s_release_resp
        }.otherwise {
          state := s_invalid
        }
      }
    }
    is(s_release_resp) {
      when(io.grant.fire) {
        state := s_invalid
      }
    }
  }
  // * FSM transition End

  count := Mux(state === s_release_req, Mux(io.release.fire, count + 1.U, count), 0.U)

  // probe ack response
  val probeAck = edge.ProbeAck(
    fromSource = req.source,
    toAddress = req.addr,
    lgSize = log2Ceil(beatBytes).U,
    reportPermissions = req.param,
  )

  // probe ack with data
  val probeAckData = edge.ProbeAck(
    fromSource = req.source,
    toAddress = req.addr,
    lgSize = log2Ceil(beatBytes).U,
    reportPermissions = req.param,
    data = curBeatData,
  )

  // voluntary release
  val release = edge.Release(
    fromSource = req.source,
    toAddress = req.addr,
    lgSize = log2Ceil(beatBytes).U,
    shrinkPermissions = req.param,
  )._2

  // voluntary release with data
  val releaseData = edge.Release(
    fromSource = req.source,
    toAddress = req.addr,
    lgSize = log2Ceil(beatBytes).U,
    shrinkPermissions = req.param,
    data = curBeatData,
  )._2

  io.release.valid := (state === s_release_req) // TODO
  io.release.bits := Mux(
    req.voluntary,
    Mux(req.hasData, releaseData, release),
    Mux(req.hasData, probeAckData, probeAck),
  )

  io.req.ready           := true.B
  io.missCheck.blockMiss := false.B
  io.grant.ready         := true.B
}
