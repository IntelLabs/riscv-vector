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
    val mainPipeReq = Decoupled(new MainPipeReq(edge.bundle))  // send probe req to main pipeline
    val wbReq       = Decoupled(new WritebackReq(edge.bundle)) // send probe req directly to writeback queue
    val lrscAddr    = Input(Valid(UInt(lineAddrWidth.W)))      // lrsc block
    val probeCheck  = Flipped(new ProbeMSHRFile())             // check probe addr in mshr
    val probeResp   = Input(ProbeRespStatus())                 // probe status in main pipeline
  })

  // * FSM Begin
  val s_invalid :: s_check_mshr :: s_wait_mshr :: s_pipe_req :: s_wb_req :: s_wait_resp :: Nil = Enum(6)

  val state = RegInit(s_invalid)

  switch(state) {
    is(s_invalid) {
      when(io.memProbe.fire) {
        state := s_check_mshr
      }
    }
    is(s_check_mshr) {
      when(io.probeCheck.hit && io.probeCheck.pass) { // can directly writeback
        state := s_wb_req
      }.elsewhen(io.probeCheck.hit && !io.probeCheck.pass) { // blocked by mask
        state := s_wait_mshr
      }.otherwise { // can directly send to main pipeline
        state := s_pipe_req
      }
    }
    is(s_wait_mshr) {
      when(io.probeCheck.replaceFinish) {
        state := s_pipe_req
      }
    }
    is(s_pipe_req) {
      when(io.mainPipeReq.fire) {
        state := s_wait_resp
      }
    }
    is(s_wb_req) {
      when(io.wbReq.fire) {
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

  // * FSM End

  val probeReq = RegEnable(io.memProbe.bits, io.memProbe.fire)
  val probeCohMSHR = RegEnable(
    ClientMetadata(io.probeCheck.probeCoh),
    io.probeCheck.hit && io.probeCheck.pass,
  )

  // check mshr addr
  io.probeCheck.valid           := (state === s_check_mshr)
  io.probeCheck.lineAddr        := getLineAddr(probeReq.address)
  io.probeCheck.probePermission := probeReq.param

  // orgranize probe req sent to pipeline
  io.mainPipeReq.bits := MainPipeReqConverter(probeReq, edge.bundle)
  io.mainPipeReq.valid :=
    (state === s_pipe_req) &&
      (!io.lrscAddr.valid || io.lrscAddr.bits =/= getLineAddr(probeReq.address))

  // organize probe req sent to wb
  io.wbReq.valid          := (state === s_wb_req)
  io.wbReq.bits.voluntary := false.B
  io.wbReq.bits.lineAddr  := getLineAddr(probeReq.address)
  io.wbReq.bits.perm      := probeCohMSHR.onProbe(probeReq.param)._2
  io.wbReq.bits.source    := probeReq.source
  io.wbReq.bits.data      := DontCare // FIXME
  io.wbReq.bits.hasData   := probeCohMSHR.onProbe(probeReq.param)._1

  io.memProbe.ready := (state === s_invalid)

  assert(io.memProbe.bits.opcode === TLMessages.Probe || ~io.memProbe.valid)
}
