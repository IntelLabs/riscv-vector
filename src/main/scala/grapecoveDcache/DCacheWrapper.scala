package grapecoveDCache
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import _root_.circt.stage.ChiselStage

class DCacheWrapper()(
    implicit p: Parameters
) extends LazyModule {

  val dcacheClient = LazyModule(new GPCDCache()(p))

  val ram = LazyModule(new TLRAM(AddressSet(0x80000000L, 0xffffL), beatBytes = beatBytes))

  ram.node :=
    TLXbar() :=*
      TLFragmenter(beatBytes, blockBytes) :=*
      TLCacheCork() :=*
      TLDelayer(0) :=*
      dcacheClient.node

  lazy val module = new DCacheWrapperImp(this)
}

class DCacheWrapperImp(outer: DCacheWrapper) extends LazyModuleImp(outer) {
  val io = IO(new DataExchangeIO())
  outer.dcacheClient.module.io <> io
}

object Main extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPortDeclSharing",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _),
    "--disable-annotation-unknown",
    "--disable-all-randomization",
  )

  lazy val dcacheWrapper = LazyModule(new DCacheWrapper()(Parameters.empty))
  ChiselStage.emitSystemVerilogFile(dcacheWrapper.dcacheClient.module, args, firtoolOptions)
}
