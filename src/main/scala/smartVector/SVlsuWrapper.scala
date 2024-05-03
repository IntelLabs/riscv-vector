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


class SVlsuWrapper(implicit p: Parameters) extends Module {
    val io = IO(new LdstIO())

    val hLsu = Module(new SVHLsu()(p))  // horizontal lsu
    val vLsu = Module(new SVVLsu()(p))  // vertical lsu

    io.lsuReady := vLsu.io.lsuReady && hLsu.io.lsuReady

    hLsu.io.mUop            <> io.mUop
    hLsu.io.mUopMergeAttr   <> io.mUopMergeAttr
    vLsu.io.mUop            <> io.mUop
    vLsu.io.mUopMergeAttr   <> io.mUopMergeAttr

    val dataReqArb = Module(new Arbiter(new RVUMemoryReq(), 2))
    dataReqArb.io.in(0) <> hLsu.io.dataExchange.req
    dataReqArb.io.in(1) <> vLsu.io.dataExchange.req
    io.dataExchange.req <> dataReqArb.io.out

    // resp & xcpt
    when((io.dataExchange.resp.bits.idx >> 4.U) === 1.U) {
        vLsu.io.dataExchange.resp       <> io.dataExchange.resp
        vLsu.io.dataExchange.xcpt       <> io.dataExchange.xcpt
        hLsu.io.dataExchange.resp.valid := false.B
        hLsu.io.dataExchange.resp.bits  := DontCare
        hLsu.io.dataExchange.xcpt       := 0.U.asTypeOf(new HellaCacheExceptions)
    } .otherwise {
        hLsu.io.dataExchange.resp       <> io.dataExchange.resp
        hLsu.io.dataExchange.xcpt       <> io.dataExchange.xcpt
        vLsu.io.dataExchange.resp.valid := false.B
        vLsu.io.dataExchange.resp.bits  := DontCare
        vLsu.io.dataExchange.xcpt       := 0.U.asTypeOf(new HellaCacheExceptions)
    }    


    io.lsuOut.valid := Mux(hLsu.io.lsuOut.valid, hLsu.io.lsuOut.valid, vLsu.io.lsuOut.valid)
    io.lsuOut.bits  := Mux(hLsu.io.lsuOut.valid, hLsu.io.lsuOut.bits, vLsu.io.lsuOut.bits)
    io.xcpt         := Mux(hLsu.io.xcpt.exception_vld || hLsu.io.lsuOut.valid, hLsu.io.xcpt, vLsu.io.xcpt)
}