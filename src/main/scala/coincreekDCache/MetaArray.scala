package coincreekDCache

import chisel3._
import chisel3.util._
import utility.SRAMTemplate

import _root_.circt.stage.ChiselStage

class Metadata extends Bundle {
  val tag = UInt(tagWidth.W)
  val coh = UInt(cohWidth.W)
}

object Metadata {

  def apply(tag: Bits, coh: Bits): Metadata = {
    val meta = Wire(new Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }

  def onReset(): Metadata =
    Metadata(0.U, 0.U)

}

class MetaReadReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
}

class MetaWriteReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
  val data   = new Metadata
}

class MetaArray[T <: Metadata](onReset: () => T) extends Module with DCacheParams {

  val rstVal   = onReset()
  val metaBits = rstVal.getWidth

  val io = IO(new Bundle {
    val read  = Flipped(Decoupled(new MetaReadReq))
    val write = Flipped(Decoupled(new MetaWriteReq))
    val resp  = Output(Vec(nWays, rstVal.cloneType))
  })

  val metaArray = Module(
    new SRAMTemplate(
      UInt(metaBits.W),
      set = nSets,
      way = nWays,
      shouldReset = false,
      holdRead = false,
      singlePort = false,
      bypassWrite = true,
    )
  )

  val rstCnt = RegInit(0.U(log2Up(nSets + 1).W))
  val rst    = rstCnt < nSets.U
  val waddr  = Mux(rst, rstCnt, io.write.bits.setIdx)
  val wdata  = Mux(rst, rstVal, io.write.bits.data).asUInt
  val wmask  = Mux(rst || (nWays == 1).B, -1.S, io.write.bits.wayEn.asSInt).asBools

  when(rst) {
    rstCnt := rstCnt + 1.U
  }

  // * Tag Write Begin
  val wen = rst || io.write.fire
  metaArray.io.w.req.valid := wen
  metaArray.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt,
  )
  // * Tag Write End

  // * Tag Read Begin
  val ren = io.read.fire
  metaArray.io.r.req.valid := ren
  metaArray.io.r.req.bits.apply(setIdx = io.read.bits.setIdx)
  io.resp := metaArray.io.r.resp.data.map(_.asTypeOf(chiselTypeOf(rstVal)))
  // * Tag Read End

  io.read.ready  := !rst
  io.write.ready := !rst
}

object Meta extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _)
  )

  ChiselStage.emitSystemVerilogFile(new MetaArray[Metadata](Metadata.onReset), args, firtoolOptions)
}
