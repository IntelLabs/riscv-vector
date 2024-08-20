package smartVector
import chisel3._
import chisel3.util._
import darecreek.VDecode
import utils._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import xiangshan.MicroOp
import SmartParam._

class SVlsuWrapper(
    implicit p: Parameters
) extends Module {
  val io = IO(new LdstIO())

  val hLsu = Module(new SVHLsu()(p)) // horizontal lsu
  val vLsu = Module(new SVVLsu()(p)) // vertical lsu

  val uopEndInQ = RegInit(false.B)

  io.lsuReady := vLsu.io.lsuReady && hLsu.io.lsuReady && !uopEndInQ

  hLsu.io.mUop <> io.mUop
  hLsu.io.mUopMergeAttr <> io.mUopMergeAttr
  vLsu.io.mUop <> io.mUop
  vLsu.io.mUopMergeAttr <> io.mUopMergeAttr

  val validReq   = io.mUop.valid && io.lsuReady
  val validMerge = io.mUopMergeAttr.valid && io.lsuReady
  hLsu.io.mUop.valid          := validReq
  vLsu.io.mUop.valid          := validReq
  hLsu.io.mUopMergeAttr.valid := validMerge
  vLsu.io.mUopMergeAttr.valid := validMerge

  val ldstXcpt = io.lsuOut.bits.xcpt.exception_vld || io.lsuOut.bits.xcpt.update_vl
  when(validReq && io.mUop.bits.uop.uopEnd && io.mUop.bits.uop.ctrl.isLdst) {
    uopEndInQ := true.B
  }.elsewhen(io.lsuOut.valid && (io.lsuOut.bits.muopEnd || ldstXcpt)) {
    uopEndInQ := false.B
  }

  val dataReqArb = Module(new Arbiter(new RVUMemoryReq(), 2))
  dataReqArb.io.in(0) <> hLsu.io.dataExchange.req
  dataReqArb.io.in(1) <> vLsu.io.dataExchange.req
  io.dataExchange.req <> dataReqArb.io.out

  // resp & xcpt
  when((io.dataExchange.resp.bits.idx >> 4.U) === 1.U) {
    vLsu.io.dataExchange.resp <> io.dataExchange.resp
    vLsu.io.dataExchange.xcpt <> io.dataExchange.xcpt
    hLsu.io.dataExchange.resp.valid     := false.B
    hLsu.io.dataExchange.resp.bits      := DontCare
    hLsu.io.dataExchange.resp.bits.nack := false.B
    hLsu.io.dataExchange.xcpt           := 0.U.asTypeOf(new HellaCacheExceptions)
  }.otherwise {
    hLsu.io.dataExchange.resp <> io.dataExchange.resp
    hLsu.io.dataExchange.xcpt <> io.dataExchange.xcpt
    vLsu.io.dataExchange.resp.valid     := false.B
    vLsu.io.dataExchange.resp.bits      := DontCare
    vLsu.io.dataExchange.resp.bits.nack := false.B
    vLsu.io.dataExchange.xcpt           := 0.U.asTypeOf(new HellaCacheExceptions)
  }

  io.lsuOut.valid := hLsu.io.lsuOut.valid | vLsu.io.lsuOut.valid
  io.lsuOut.bits  := Mux(hLsu.io.lsuOut.valid, hLsu.io.lsuOut.bits, vLsu.io.lsuOut.bits)
}
