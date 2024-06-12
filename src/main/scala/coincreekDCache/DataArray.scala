package coincreekDCache

import chisel3._
import chisel3.util._
import utility.SRAMTemplate

// {{{ Single DataBank

class DataBankReadReq extends Bundle {
  val idx = UInt(setIdxBits.W)
  val way = UInt(nWays.W)
}

class DataBankWriteReq extends Bundle {
  val idx  = UInt(setIdxBits.W)
  val way  = UInt(nWays.W)
  val data = UInt(rowBits.W)
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
        bypassWrite = true,
      )
    )
  }

  for (w <- 0 until nWays) {
    val wen = io.write.valid && io.write.bits.way(w)
    dataBank(w).io.w.req.valid := wen
    dataBank(w).io.w.req.bits.apply(
      setIdx = io.write.bits.idx,
      data = io.write.bits.data,
      waymask = 1.U,
    )

    dataBank(w).io.r.req.valid := io.read.valid
    dataBank(w).io.r.req.bits.apply(setIdx = io.read.bits.idx)
  }

  io.resp := dataBank.map(_.io.r.resp.data(0))
}

// }}}

// {{{ DataArray Constructed By DataBanks

class DataReadReq extends Bundle {
  val idx  = UInt(setIdxBits.W)
  val way  = UInt(nWays.W)
  val bank = UInt(nBanks.W)
}

class DataWriteReq extends Bundle {
  val idx   = UInt(setIdxBits.W)
  val way   = UInt(nWays.W)
  val bank  = UInt(nBanks.W)
  val wmask = Vec(nBanks, UInt(rowWords.W))
  val data  = Vec(nBanks, UInt(rowBits.W))
}

class DataArray extends Module {
  val io = IO(new Bundle {
    val read  = Flipped(Decoupled(new DataReadReq))
    val write = Flipped(Decoupled(new DataWriteReq))
    val resp  = Output(Vec(nWays, Vec(nBanks, UInt(rowBits.W))))
  })

  val dataBanks = List.tabulate(nBanks)(i => Module(new DataSRAMBank()))

  for (b <- 0 until nBanks) {
    val wen = io.write.valid && io.write.bits.bank(b)
    dataBanks(b).io.write.valid     := wen
    dataBanks(b).io.write.bits.idx  := io.write.bits.idx
    dataBanks(b).io.write.bits.way  := io.write.bits.way
    dataBanks(b).io.write.bits.data := io.write.bits.data(b)

    dataBanks(b).io.read.valid    := io.read.valid
    dataBanks(b).io.read.bits.idx := io.read.bits.idx
    dataBanks(b).io.read.bits.way := io.read.bits.way
  }

  for (w <- 0 until nWays) {
    io.resp(w) := VecInit(dataBanks.map(_.io.resp(w)))
  }

  io.read.ready  := true.B
  io.write.ready := true.B
}

// }}}
