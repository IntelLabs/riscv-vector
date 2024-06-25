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
  val lineAddr  = UInt((paddrWidth - blockOffBits).W)
  val perm      = UInt(TLPermissions.cWidth.W)
  val source    = UInt(params.sourceBits.W) // tilelink source
  val hasData   = Bool()
  val data      = UInt(dataWidth.W)
}

class MissCheck extends Bundle {
  val valid     = Input(Bool())
  val lineAddr  = Input(UInt((paddrWidth - blockOffBits).W))
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
    beatData
  }

  val s_invalid :: s_release_req :: s_release_resp :: Nil = Enum(3)

  val state = RegInit(s_invalid)

  val req         = RegEnable(io.req.bits, io.req.fire)
  val remainBeats = RegInit(0.U(log2Up(refillCycles + 1).W))
  val curBeatData = getBeatData(refillCycles.U - remainBeats, req.data)
  val allBeatDone = edge.last(io.release)

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

  when(state === s_invalid && io.req.fire) {
    remainBeats := Mux(io.req.bits.hasData, refillCycles.U, 1.U)
  }.elsewhen(state === s_release_req && io.release.fire) {
    remainBeats := remainBeats - 1.U
  }

  val releaseAddr = req.lineAddr << blockOffBits

  // probe ack response
  val probeAck = edge.ProbeAck(
    fromSource = req.source,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    reportPermissions = req.perm,
  )

  // probe ack with data
  val probeAckData = edge.ProbeAck(
    fromSource = req.source,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    reportPermissions = req.perm,
    data = curBeatData,
  )

  // voluntary release
  val release = edge.Release(
    fromSource = req.source,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = req.perm,
  )._2

  // voluntary release with data
  val releaseData = edge.Release(
    fromSource = req.source,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = req.perm,
    data = curBeatData,
  )._2

  io.release.valid := (state === s_release_req) // TODO
  io.release.bits := Mux(
    req.voluntary,
    Mux(req.hasData, releaseData, release),
    Mux(req.hasData, probeAckData, probeAck),
  )

  dontTouch(io.release) // TODO: Delete

  // cache miss & addr is in wbq -> block the miss req
  io.missCheck.blockMiss := io.missCheck.valid && (state =/= s_invalid) && (io.missCheck.lineAddr === req.lineAddr)

  io.req.ready   := (state === s_invalid)
  io.grant.ready := true.B
}
