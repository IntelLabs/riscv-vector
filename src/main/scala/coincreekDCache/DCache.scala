package coincreekDCache

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class DCache extends Module {

  val io = IO(new DataExchangeIO())

  val dataArray = Module(new DataArray())
  val metaArray = Module(new MetaArray[Metadata](Metadata.onReset))

  // * Signal Define Begin
  // Store -> Load Bypassing
  val s1_storeBypass     = RegInit(false.B)
  val s1_storeBypassData = RegInit(0.U(dataWidth.W))
  // * Signal Define End

  // * pipeline stage 0 Begin

  // read tag array
  metaArray.io.read.valid       := io.req.valid
  metaArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(io.req.bits.paddr)
  metaArray.io.read.bits.wayEn  := Mux(io.req.bits.specifyValid, UIntToOH(io.req.bits.specifyWay), Fill(nWays, true.B))

  // read data array
  dataArray.io.read.valid       := io.req.valid
  dataArray.io.read.bits.setIdx := AddrDecoder.getSetIdx(io.req.bits.paddr)
  dataArray.io.read.bits.bankEn := UIntToOH(AddrDecoder.getBankIdx(io.req.bits.paddr))
  dataArray.io.read.bits.wayEn  := Mux(io.req.bits.specifyValid, UIntToOH(io.req.bits.specifyWay), Fill(nWays, true.B))
  // * pipeline stage 0 End

  // * pipeline stage 1 Begin
  // 1. Judge Tag & Meta
  // 2. Organize Data
  // 3. Return Resp
  val s1_valid = RegNext(io.req.valid)
  val s1_req   = RegNext(io.req.bits)

  // meta & data resp
  val s1_dataArrayResp = dataArray.io.resp // nways nbanks data
  val s1_metaArrayResp = metaArray.io.resp // nways meta

  // tag & coh match
  val s1_tagEqWay    = VecInit((0 until nWays).map(w => s1_metaArrayResp(w).tag === AddrDecoder.getTag(s1_req.paddr)))
  val s1_tagMatch    = s1_tagEqWay.asUInt.orR
  val s1_tagMatchWay = s1_tagEqWay
  val s1_hit         = s1_valid && s1_tagMatch

  // organize read data
  val s1_dataPreBypass = Mux1H(s1_tagMatchWay, s1_dataArrayResp).asUInt
  val s1_data          = Mux(s1_storeBypass, s1_storeBypassData, s1_dataPreBypass)
  val loadGen          = new LoadGen(s1_req.size, s1_req.signed, s1_req.paddr, s1_data, false.B, dataBytes)

  // organize store data
  val s1_storeGenMask   = new StoreGen(s1_req.size, s1_req.paddr, 0.U, dataBytes).mask
  val s1_maskInBytes    = Mux(s1_req.cmd === MemoryOpConstants.M_PWR, s1_req.wmask, s1_storeGenMask)
  val s1_mask           = FillInterleaved(8, s1_maskInBytes)
  val s1_mergeStoreData = s1_req.wdata & s1_mask | s1_data & ~s1_mask
  // TODO: PWR assertion
  // TODO: Merge Store & AMO Store

  // amo store data
  val amoalu          = Module(new AMOALU(dataWidth))
  val s1_amoStoreData = amoalu.io.out

  amoalu.io.mask := s1_maskInBytes
  amoalu.io.cmd  := s1_req.cmd
  amoalu.io.lhs  := s1_data
  amoalu.io.rhs  := s1_req.wdata

  val s1_storeData = Mux(MemoryOpConstants.isAMO(s1_req.cmd), s1_amoStoreData, s1_mergeStoreData)

  // return resp
  io.resp.valid       := s1_valid
  io.resp.bits.hit    := s1_hit
  io.resp.bits.source := RegNext(s1_req.source)
  io.resp.bits.data   := loadGen.data

  val s1_needWrite =
    s1_hit & MemoryOpConstants.isWrite(s1_req.cmd) || (s1_valid & s1_req.cmd === MemoryOpConstants.M_FILL) // TODO

  // * pipeline stage 1 End

  // * pipeline stage 2 Begin
  val s2_valid = RegNext(s1_valid & s1_needWrite)
  val s2_req   = Reg(new DataExchangeReq)
  val s2_wayEn = RegInit(0.U(nWays.W))

  when(s1_valid && s1_needWrite) {
    s2_req       := s1_req
    s2_wayEn     := Mux(s1_hit, s1_tagMatchWay, VecInit(UIntToOH(s1_req.specifyWay).asBools)).asUInt
    s2_req.wdata := s1_storeData
  }

  // meta write
  metaArray.io.write.valid         := s2_valid
  metaArray.io.write.bits.setIdx   := AddrDecoder.getSetIdx(s2_req.paddr)
  metaArray.io.write.bits.wayEn    := s2_wayEn
  metaArray.io.write.bits.data.tag := AddrDecoder.getTag(s2_req.paddr)
  metaArray.io.write.bits.data.coh := 0.U

  // data write
  dataArray.io.write.valid       := s2_valid
  dataArray.io.write.bits.setIdx := AddrDecoder.getSetIdx(s2_req.paddr)
  dataArray.io.write.bits.bankEn := Fill(nBanks, true.B).asUInt
  dataArray.io.write.bits.wayEn  := s2_wayEn
  dataArray.io.write.bits.data   := VecInit((0 until nBanks).map(i => s2_req.wdata((i + 1) * rowBits - 1, i * rowBits)))
  dataArray.io.write.bits.mask   := VecInit(Seq.fill(nBanks)(0.U)) // TODO

  // * pipeline stage 2 End

  // * pipeline stage 3 Begin
  val s3_valid = RegNext(s2_valid, false.B)
  val s3_req   = RegNext(s2_req)
  // * pipeline stage 3 End

  // *  Store -> Load Bypassing Begin
  // bypass list (valid, req, data)
  val bypassDataList = List(
    (s1_valid, s1_req, s1_storeData),
    (s2_valid, s2_req, s2_req.wdata),
    (s3_valid, s3_req, s3_req.wdata),
  ).map(r =>
    (r._1 && s1_req.paddr >> blockOffBits === r._2.paddr >> blockOffBits && MemoryOpConstants.isWrite(r._2.cmd), r._3)
  )

  s1_storeBypass     := bypassDataList.map(_._1).reduce(_ || _)
  s1_storeBypassData := PriorityMux(bypassDataList)
  // * Store -> Load Bypassing End

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
