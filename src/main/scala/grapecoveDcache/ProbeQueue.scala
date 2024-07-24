package grapecoveDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import AddrDecoder._

class ProbeQueue(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module {
  val io = IO(new Bundle {
    val memProbe    = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val mainPipeReq = Decoupled(new MainPipeReq)               // send probe req to main pipeline
    val wbReq       = Decoupled(new WritebackReq(edge.bundle)) // send probe req directly to writeback queue
    val lrscAddr    = Input(Valid(UInt(lineAddrWidth.W)))      // lrsc block
    val probeCheck  = Flipped(new ProbeMSHRFile())             // check probe addr in mshr
    val probeResp   = Input(ProbeRespStatus())                 // probe status in main pipeline
  })

  val s_invalid :: s_pipe_req :: s_wait_resp :: Nil = Enum(3)

  val state = RegInit(s_invalid)

  switch(state) {
    is(s_invalid) {
      when(io.memProbe.fire) {
        state := s_pipe_req
      }
    }
    is(s_pipe_req) {
      when(io.mainPipeReq.fire) {
        state := s_wait_resp
      }.elsewhen(io.wbReq.fire) {
        state := s_invalid
      }
    }
    is(s_wait_resp) {
      when(io.probeResp === ProbeRespStatus.probe_finish) {
        state := s_invalid
      }.elsewhen(io.probeResp === ProbeRespStatus.probe_replay) {
        state := s_pipe_req
      }
    }
  }

  val probeReq = RegEnable(io.memProbe.bits, io.memProbe.fire)

  // check mshr addr
  io.probeCheck.valid           := (state === s_pipe_req)
  io.probeCheck.lineAddr        := getLineAddr(probeReq.address)
  io.probeCheck.probePermission := probeReq.param

  val issueValid = (state === s_pipe_req) && ~io.lrscAddr.valid

  // orgranize probe req sent to pipeline
  io.mainPipeReq.valid := issueValid && (!io.probeCheck.hit && !io.probeCheck.pass)
  io.mainPipeReq.bits  := MainPipeReqConverter(probeReq)

  // organize probe req sent to wb
  io.wbReq.valid          := issueValid && io.probeCheck.pass
  io.wbReq.bits.voluntary := false.B
  io.wbReq.bits.lineAddr  := getLineAddr(probeReq.address)
  io.wbReq.bits.perm      := probeReq.param
  io.wbReq.bits.data      := DontCare // FIXME
  io.wbReq.bits.hasData   := false.B  // FIXME

  io.memProbe.ready := (state === s_invalid)

  assert(io.memProbe.bits.opcode === TLMessages.Probe || ~io.memProbe.valid)
}
