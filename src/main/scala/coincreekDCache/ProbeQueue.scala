package coincreekDCache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleC, TLBundleD, TLEdgeOut}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class ProbeAddrCheck extends Bundle {
  val valid      = Output(Bool())
  val addr       = Output(UInt(paddrWidth.W))
  val blockProbe = Input(Bool())
}

class ProbeQueue(
    implicit edge: TLEdgeOut,
    p:             Parameters,
) extends Module {
  val io = IO(new Bundle {
    val memProbe    = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val mainPipeReq = Decoupled(new MainPipeReq)

    val lsrcValid    = Input(Bool())
    val lsrcLineAddr = Input(UInt((paddrWidth - blockOffBits).W))
    val probeCheck   = new ProbeAddrCheck()

    val wbReady = Input(Bool())
  })

  val s_invalid :: s_pipe_req :: s_wait_resp :: Nil = Enum(3)

  val state = RegInit(s_invalid)

  val probeReq = RegEnable(io.memProbe.bits, io.memProbe.fire)

  switch(state) {
    is(s_invalid) {
      when(io.memProbe.fire) {
        state := s_pipe_req
      }
    }
    is(s_pipe_req) {
      when(io.mainPipeReq.fire) {
        state := s_wait_resp
      }
    }
    is(s_wait_resp) {
      when(io.mainPipeReq.fire) {
        state := s_invalid
      }
    }
  }

  // check mshr addr
  io.probeCheck.valid := (state === s_pipe_req)
  io.probeCheck.addr  := probeReq.address

  // orgranize probe req sent to pipeline
  val mainPipeReq = MainPipeReqConverter(probeReq)

  io.mainPipeReq.valid := (state === s_pipe_req) && !io.probeCheck.blockProbe & io.wbReady
  io.mainPipeReq.bits  := mainPipeReq

  io.memProbe.ready := true.B

  dontTouch(io.memProbe)
  assert(io.memProbe.bits.opcode === TLMessages.Probe || ~io.memProbe.valid)
}
