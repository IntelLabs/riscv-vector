package grapecoveDCache

import chisel3._
import chisel3.util._
import utility.SRAMTemplate

// * Single DataBank

class DataBankReadReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
}

class DataBankWriteReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
  val data   = UInt(rowBits.W)
}

class DataSRAMBank extends Module {
  val io = IO(new Bundle {
    val read  = Flipped(ValidIO(new DataBankReadReq))
    val write = Flipped(ValidIO(new DataBankWriteReq))
    val resp  = Output(Vec(nWays, UInt(rowBits.W)))
  })
  val dataBank = Array.fill(nWays) {
    Module(
      new SRAMTemplate(
        UInt(rowBits.W),
        set = nSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = false,
        bypassWrite = false,
      )
    )
  }

  for (w <- 0 until nWays) {
    val wen = io.write.valid && io.write.bits.wayEn(w)
    dataBank(w).io.w.req.valid := wen
    dataBank(w).io.w.req.bits.apply(
      setIdx = io.write.bits.setIdx,
      data = io.write.bits.data,
      waymask = 1.U,
    )

    dataBank(w).io.r.req.valid := io.read.valid
    dataBank(w).io.r.req.bits.apply(setIdx = io.read.bits.setIdx)
  }

  io.resp := dataBank.map(_.io.r.resp.data(0))
}

// * DataArray Constructed By DataBanks

class DataReadReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
  val bankEn = UInt(nBanks.W)
}

class DataWriteReq extends Bundle {
  val setIdx = UInt(setIdxBits.W)
  val wayEn  = UInt(nWays.W)
  val bankEn = UInt(nBanks.W)
  val mask   = Vec(nBanks, UInt(rowWords.W))
  val data   = Vec(nBanks, UInt(rowBits.W))
}

class DataArray extends Module {
  val io = IO(new Bundle {
    val read  = Flipped(Decoupled(new DataReadReq))
    val write = Flipped(Decoupled(new DataWriteReq))
    val resp  = Output(Vec(nWays, Vec(nBanks, UInt(rowBits.W))))
  })

  val dataBanks = List.tabulate(nBanks)(i => Module(new DataSRAMBank()))

  for (b <- 0 until nBanks) {
    val wen = io.write.valid && io.write.bits.bankEn(b)
    dataBanks(b).io.write.valid       := wen
    dataBanks(b).io.write.bits.setIdx := io.write.bits.setIdx
    dataBanks(b).io.write.bits.wayEn  := io.write.bits.wayEn
    dataBanks(b).io.write.bits.data   := io.write.bits.data(b)

    dataBanks(b).io.read.valid       := io.read.valid
    dataBanks(b).io.read.bits.setIdx := io.read.bits.setIdx
    dataBanks(b).io.read.bits.wayEn  := io.read.bits.wayEn
  }

  for (w <- 0 until nWays) {
    io.resp(w) := VecInit(dataBanks.map(_.io.resp(w)))
  }

  io.read.ready  := true.B
  io.write.ready := true.B
}
