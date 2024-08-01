package grapecoveDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import utility._

class WritebackReq(params: TLBundleParameters) extends Bundle {
  val voluntary = Bool()
  val lineAddr  = UInt(lineAddrWidth.W)
  val perm      = UInt(TLPermissions.cWidth.W)
  val hasData   = Bool()
  val data      = UInt(dataWidth.W)
  // val source    = UInt(params.sourceBits.W) // tilelink source
}

class MissCheck extends Bundle {
  val valid     = Input(Bool())
  val lineAddr  = Input(UInt(lineAddrWidth.W))
  val blockMiss = Output(Bool())
}

class WritebackEntry(id: Int)(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module {
  val io = IO(new Bundle {
    val req       = Flipped(Decoupled(new WritebackReq(edge.bundle)))
    val missCheck = new MissCheck()
    val release   = Decoupled(new TLBundleC(edge.bundle))
    val grant     = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val wbFinish  = Output(Bool())
  })

  // helper function to get current beat data
  def getBeatData(count: UInt, data: UInt): UInt = {
    val beatData = Wire(UInt(beatBits.W))
    beatData := data >> (count << log2Up(beatBits))
    beatData
  }

  val reqReg = RegEnable(io.req.bits, io.req.fire)

  // * FSM Begin
  val s_invalid :: s_release_req :: s_release_resp :: Nil = Enum(3)

  val state     = RegInit(s_invalid)
  val nextState = WireDefault(s_invalid)

  val allBeatDone = edge.done(io.release)

  switch(state) {
    is(s_invalid) {
      when(io.req.fire) {
        nextState := s_release_req
      }.otherwise {
        nextState := s_invalid
      }
    }
    is(s_release_req) {
      when(allBeatDone) {
        when(reqReg.voluntary) {
          nextState := s_release_resp
        }.otherwise {
          nextState := s_invalid
        }
      }.otherwise {
        nextState := s_release_req
      }
    }
    is(s_release_resp) {
      when(io.grant.fire) {
        nextState := s_invalid
      }.otherwise {
        nextState := s_release_resp
      }
    }
  }

  state := nextState
  // * FSM End

  // calculate remain beats & get beat data
  val remainBeats = RegInit(0.U(log2Up(refillCycles + 1).W))
  val curBeatData = getBeatData(refillCycles.U - remainBeats, reqReg.data)

  when(state === s_invalid && io.req.fire) {
    remainBeats := Mux(io.req.bits.hasData, refillCycles.U, 1.U)
  }.elsewhen(state === s_release_req && io.release.fire) {
    remainBeats := remainBeats - 1.U
  }

  // * Organize C Channel Messages Begin
  val releaseAddr = reqReg.lineAddr << blockOffBits

  // probe ack response
  val probeAck = edge.ProbeAck(
    fromSource = id.U,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    reportPermissions = reqReg.perm,
  )

  // probe ack with data
  val probeAckData = edge.ProbeAck(
    fromSource = id.U,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    reportPermissions = reqReg.perm,
    data = curBeatData,
  )

  // voluntary release
  val release = edge.Release(
    fromSource = id.U,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = reqReg.perm,
  )._2

  // voluntary release with data
  val releaseData = edge.Release(
    fromSource = id.U,
    toAddress = releaseAddr,
    lgSize = log2Ceil(blockBytes).U,
    shrinkPermissions = reqReg.perm,
    data = curBeatData,
  )._2
  // * Organize TL-Channel C Messages End

  // release
  io.release.valid := (state === s_release_req)
  io.release.bits := Mux(
    reqReg.voluntary,
    Mux(reqReg.hasData, releaseData, release),
    Mux(reqReg.hasData, probeAckData, probeAck),
  )

  // writeback finish
  io.wbFinish := (state =/= s_invalid) && (nextState === s_invalid)

  // cache miss & addr is in wbq -> block the miss req
  io.missCheck.blockMiss := io.missCheck.valid &&
    (state =/= s_invalid) &&
    (io.missCheck.lineAddr === reqReg.lineAddr)

  io.req.ready   := (state === s_invalid)
  io.grant.ready := (state === s_release_resp)
}

class WritebackQueuePtr extends CircularQueuePtr[WritebackQueuePtr](nWBQEntries)

class WritebackQueue(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val req       = Flipped(Decoupled(new WritebackReq(edge.bundle)))
    val missCheck = new MissCheck()
    val release   = Decoupled(new TLBundleC(edge.bundle))
    val grant     = Flipped(Decoupled(new TLBundleD(edge.bundle)))
  })

  val enqPtr = RegInit(0.U.asTypeOf(new WritebackQueuePtr))
  val deqPtr = RegInit(0.U.asTypeOf(new WritebackQueuePtr))

  val wbqEntries = (0 until nWBQEntries).map { i =>
    val entryId = firstWBQ + i
    val entry   = Module(new WritebackEntry(entryId)(edge, p))

    // req
    entry.io.req.valid := io.req.valid && (enqPtr.value === i.U)
    entry.io.req.bits  := io.req.bits

    // miss check
    entry.io.missCheck.valid    := io.missCheck.valid
    entry.io.missCheck.lineAddr := io.missCheck.lineAddr

    // tl-d grant
    entry.io.grant.valid := (io.grant.valid && io.grant.bits.source === entryId.U)
    entry.io.grant.bits  := io.grant.bits

    entry
  }

  // enq
  when(io.req.fire) {
    enqPtr := enqPtr + 1.U
  }

  // release
  val releaseVec = VecInit(wbqEntries.map(_.io.release))
  releaseVec.zipWithIndex.foreach { case (source, i) =>
    source.ready := (deqPtr.value === i.U) && io.release.ready
  }

  io.release.valid := releaseVec(deqPtr.value).valid
  io.release.bits  := releaseVec(deqPtr.value).bits

  // tl-d grant
  val grantReadyVec = VecInit(wbqEntries.map(_.io.grant.ready))
  io.grant.ready := grantReadyVec(deqPtr.value)

  // wb finish
  val wbFinishVec = VecInit(wbqEntries.map(_.io.wbFinish))

  when(wbFinishVec(deqPtr.value)) {
    deqPtr := deqPtr + 1.U
  }

  // miss check
  val blockMissVec = VecInit(wbqEntries.map(_.io.missCheck.blockMiss))
  io.missCheck.blockMiss := blockMissVec.reduce(_ || _)

  io.req.ready := !isFull(enqPtr, deqPtr)
}
