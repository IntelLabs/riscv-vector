package coincreekDCache

import chisel3._
import chisel3.util._

import MemoryOpConstants._
import DCacheParams._

class DataExchangeReq extends Bundle {
  val source = UInt(srcWidth.W)
  val cmd    = UInt(M_SZ.W)

}

class DataExchangeResp extends Bundle {
  val source = UInt(srcWidth.W)
  val data   = UInt(dataWidth.W)
}

class DataExchangeIO extends Bundle {
  val req  = Flipped(Decoupled(new DataExchangeReq))
  val resp = Decoupled(new DataExchangeResp)
}
