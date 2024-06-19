package coincreekDCache
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

  val dcacheClient = LazyModule(new CCDCache()(p))
  val zeroDevice = LazyModule(new TLZero(
    address = AddressSet(0x2000, 0xff),
    beatBytes = 64,
  ))

  zeroDevice.node := dcacheClient.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new DataExchangeIO())
    dcacheClient.module.io <> io
  }
}

object Main extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _)
  )

  lazy val dcacheWrapper = LazyModule(new DCacheWrapper()(Parameters.empty))
  ChiselStage.emitSystemVerilogFile(dcacheWrapper.module, args, firtoolOptions)
}
