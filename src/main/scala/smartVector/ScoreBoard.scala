package smartVector

import chisel3._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.formal.MonitorDirection._
import chisel3.util._

class Scoreboard(n: Int, zero: Boolean = false)
{

  def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
  def setN(en: Bool, addr: UInt, num: UInt): Unit = update(en, _next | maskN(en, addr, num))
  def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
  def clearN(en: Bool, addr: UInt, num: UInt): Unit = update(en, _next & ~maskN(en, addr, num))
  def clearAll(en: Bool): Unit = update(en, _next & Fill(n,~en))
  def read(addr: UInt): Bool = r(addr)
  def readBypassed(addr: UInt): Bool = _next(addr)
  def readBypassedN(n: UInt, addr: UInt) = {
      val mask = ((1.U << n) - 1.U) << addr
      (_next & mask).orR()
  } 
  val _r = RegInit(0.U(n.W))
  val r = if (zero) (_r >> 1 << 1) else _r
  var _next = r
  var ens = false.B
  def mask(en: Bool, addr: UInt) = Mux(en, 1.U << addr, 0.U)
  def maskN(en: Bool, addr: UInt, num: UInt) = Mux(en, ((1.U << num) - 1.U) << addr, 0.U)
  def update(en: Bool, update: UInt) = {
    _next = update
    ens = ens || en
    when (ens) { _r := _next }
  }
}

//class Scoreboard(n: Int, zero: Boolean = false) extends Module {
//  val io = IO(new Bundle {
//    val setEn   = Input(Bool())
//    val setAddr = Input(UInt(log2Ceil(n).W))
//    val clearEn = Input(Bool())
//    val clearAddr = Input(UInt(log2Ceil(n).W))
//    val readAddr = Input(UInt(log2Ceil(n).W))
//    //val setBypass = Output(Bool())
//    //val clearBypass = Output(Bool())
//    val read = Output(Bool())
//    val readBypassed = Output(Bool())
//  })
// 
//  val _r = RegInit(0.U(n.W))
//  val r = if (zero) (_r >> 1 << 1) else _r
//  var _next = r
//  var ens = false.B
// 
//  def mask(en: Bool, addr: UInt) = Mux(en, 1.U << addr, 0.U)
// 
//  def update(en: Bool, update: UInt) = {
//    _next = update
//    ens = ens || en
//    when (ens) { _r := _next }
//  }
// 
//  // Set logic
//  when(io.setEn) {
//    update(io.setEn, _next | mask(io.setEn, io.setAddr))
//  }
// 
//  // Clear logic
//  when(io.clearEn) {
//    update(io.clearEn, _next & ~mask(io.clearEn, io.clearAddr))
//  }
// 
//  // Read logic
//  io.read := r(io.readAddr)
// 
//  // Read Bypassed logic
//  io.readBypassed := _next(io.readAddr)
// 
//  // Bypass signals
//  //io.setBypass := io.setEn && _next(io.setAddr)
//  //io.clearBypass := io.clearEn && _next(io.clearAddr)
//}
// 
//