package smartVector

import chisel3._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.formal.MonitorDirection._
import chisel3.util._

class Scoreboard(n: Int, zero: Boolean = false)
{

  def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
  def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
  def read(addr: UInt): Bool = r(addr)
  def readBypassed(addr: UInt): Bool = _next(addr)
  val _r = RegInit(0.U(n.W))
  val r = if (zero) (_r >> 1 << 1) else _r
  var _next = r
  var ens = false.B
  def mask(en: Bool, addr: UInt) = Mux(en, 1.U << addr, 0.U)
  def update(en: Bool, update: UInt) = {
    _next = update
    ens = ens || en
    when (ens) { _r := _next }
  }
  printf("_r = %b\n", _r)
  printf("_next = %b\n", _next)
  printf("r = %b\n", r)
}

