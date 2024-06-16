package coincreekDCache

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class DCache extends Module {

  val io = IO(new DataExchangeIO())

  val dataArray = Module(new DataArray())
  val metaArray = Module(new MetaArray[Metadata](Metadata.onReset))

  // {{{ pipeline stage 0

  /**
   * Read Tag Array & Read Data Array
   */

  // read tag array
  metaArray.io.read.valid       := io.req.valid
  metaArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(io.req.bits.paddr)
  metaArray.io.read.bits.wayEn  := Mux(io.req.bits.specifyValid, UIntToOH(io.req.bits.specifyWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := io.req.valid
  dataArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(io.req.bits.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(AddrDecoder.getBankIdx(io.req.bits.paddr))
  dataArray.io.read.bits.wayEn  := Mux(io.req.bits.specifyValid, UIntToOH(io.req.bits.specifyWay), Fill(nWays, true.B))

  // }}}

  // {{{ pipeline stage 1

  /**
   *   1. Judge Tag & Meta 2. Organize Data 3. Return Resp
   */
  val s1_valid = RegNext(io.req.valid)
  val s1_req   = RegNext(io.req.bits)

  val s1_dataArrayResp = dataArray.io.resp // nways meta
  val s1_metaArrayResp = metaArray.io.resp // nways nbanks data

  // tag & coh match
  val s1_tagEqWay    = VecInit((0 until nWays).map(w => s1_metaArrayResp(w).tag === AddrDecoder.getTag(s1_req.paddr)))
  val s1_tagMatch    = s1_tagEqWay.asUInt.orR
  val s1_tagMatchWay = s1_tagEqWay

  // hit / miss
  val s1_hit = s1_valid && s1_tagMatch

  io.resp.valid       := s1_valid
  io.resp.bits.hit    := s1_hit
  io.resp.bits.source := RegNext(s1_req.source)
  io.resp.bits.data   := Mux1H(s1_tagMatchWay, s1_dataArrayResp).asUInt

  val s1_needWrite =
    s1_hit & MemoryOpConstants.isWrite(s1_req.cmd) || (s1_valid & s1_req.cmd === MemoryOpConstants.M_FILL) // TODO
  // }}}

  // {{{ pipeline stage 2
  val s2_valid   = RegNext(s1_valid & s1_needWrite)
  val s2_req     = RegNext(s1_req)
  val storeWayEn = Mux(s1_hit, s1_tagMatchWay, VecInit(UIntToOH(s1_req.specifyWay).asBools)).asUInt
  val s2_wdata   = VecInit((0 until nBanks).map(i => s2_req.wdata((i + 1) * rowBits - 1, i * rowBits)))

  // meta write
  metaArray.io.write.valid         := s2_valid
  metaArray.io.write.bits.setIdx   := AddrDecoder.getSetIdx(s2_req.paddr)
  metaArray.io.write.bits.wayEn    := storeWayEn
  metaArray.io.write.bits.data.tag := AddrDecoder.getTag(s2_req.paddr)
  metaArray.io.write.bits.data.coh := 0.U
  // data write
  dataArray.io.write.valid       := s2_valid
  dataArray.io.write.bits.setIdx := AddrDecoder.getSetIdx(s2_req.paddr)
  dataArray.io.write.bits.bankEn := UIntToOH(AddrDecoder.getBankIdx(s2_req.paddr))
  dataArray.io.write.bits.wayEn  := storeWayEn
  dataArray.io.write.bits.data   := s2_wdata
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO

  // }}}

  io.req.ready := true.B

}

object Main extends App {

  val firtoolOptions = Array(
    "--lowering-options=" + List(
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _)
  )

  ChiselStage.emitSystemVerilogFile(new DCache(), args, firtoolOptions)
}
