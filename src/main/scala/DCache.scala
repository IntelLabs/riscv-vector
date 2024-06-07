package coincreekDCache

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class DCache extends Module {

  val io = IO(new DataExchangeIO())

  io.req.ready        := true.B
  io.resp.valid       := RegNext(io.req.valid)
  io.resp.bits.source := RegNext(io.req.bits.source)
  io.resp.bits.data   := DontCare

}

object Main extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _)
  )

  ChiselStage.emitSystemVerilogFile(new DCache(), args, firtoolOptions)
}
