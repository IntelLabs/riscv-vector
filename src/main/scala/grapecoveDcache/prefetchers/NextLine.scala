package grapecoveDcache
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import grapecoveDCache.AddrDecoder.getLineAddr

case class NextLinePrefetcherParams(
) extends CanInstantiatePrefetcher {
  def desc() = "NextLine Prefetcher"
  def instantiate()(
      implicit p: Parameters
  ) = Module(new NextLinePrefetcher(this)(p))
}

class NextLinePrefetcher(
    params: NextLinePrefetcherParams
)(
    implicit p: Parameters
) extends AbstractPrefetcher()(p) {
  val desc = "NextLinePrefetcher"

  val s1_trainLineAddr = RegInit(0.U(lineAddrWidth.W))
  val s1_trainValid    = RegInit(false.B)

  // s0
  s1_trainValid    := io.train.fire
  s1_trainLineAddr := getLineAddr(io.train.bits.vaddr)

  // s1
  io.req.valid      := s1_trainValid
  io.req.bits.paddr := ((s1_trainLineAddr + 1.U) << blockOffBits)
  io.req.bits.write := false.B
}
