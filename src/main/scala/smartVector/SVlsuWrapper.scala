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

  // * lsu modules
  val hLsu = Module(new SVHLsu()(p)) // horizontal lsu
  val vLsu = Module(new SVVLsu()(p)) // vertical lsu

  // decode
  val validReq = io.mUop.valid && io.mUop.bits.uop.ctrl.isLdst && io.lsuReady
  val ldstCtrl = LSULdstDecoder(io.mUop.bits, io.mUopMergeAttr.bits)
  val mUopInfo = mUopInfoSelecter(io.mUop.bits, io.mUopMergeAttr.bits)

  // * lsu ready?
  val uopEndInQ = RegInit(false.B)
  val ldstXcpt  = io.lsuOut.bits.xcpt.exception_vld || io.lsuOut.bits.xcpt.update_vl

  when(validReq && io.mUop.bits.uop.uopEnd) {
    uopEndInQ := true.B
  }.elsewhen(io.lsuOut.valid && (io.lsuOut.bits.muopEnd || ldstXcpt)) {
    uopEndInQ := false.B
  }
  io.lsuReady := vLsu.io.lsuReq.ready && hLsu.io.lsuReq.ready && !uopEndInQ

  // * construct lsu req
  val lsuReq = Wire(new LSUReq())
  lsuReq.ldstCtrl := ldstCtrl
  lsuReq.muopInfo := mUopInfo
  lsuReq.uopIdx   := io.mUop.bits.uop.uopIdx
  lsuReq.uopEnd   := io.mUop.bits.uop.uopEnd
  lsuReq.vstart   := io.mUop.bits.uop.info.vstart
  lsuReq.vl       := io.mUop.bits.uop.info.vl

  // * connect lsu
  hLsu.io.lsuReq.valid := validReq && ldstCtrl.nfield === 1.U
  hLsu.io.lsuReq.bits  := lsuReq

  vLsu.io.lsuReq.valid := validReq && ldstCtrl.nfield > 1.U
  vLsu.io.lsuReq.bits  := lsuReq

  val dataReqArb = Module(new Arbiter(new RVUMemoryReq(), 2))
  dataReqArb.io.in(0) <> hLsu.io.dataExchange.req
  dataReqArb.io.in(1) <> vLsu.io.dataExchange.req
  io.dataExchange.req <> dataReqArb.io.out

  // resp & xcpt
  when(io.dataExchange.resp.bits.srcId === 1.U) {
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
