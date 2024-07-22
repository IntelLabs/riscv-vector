package grapecoveDCache

import chisel3._
import chisel3.util._

object MasterSource {
  val width = 2

  def CoreInt = 0.U(width.W)
  def CoreFp  = 1.U(width.W)
  def Vector  = 2.U(width.W)
}

// s1 cache return resp
object CacheRespStatus extends ChiselEnum {
  val hit, miss, replay, refill = Value
}

// mshr -> main pipe replace req status
object ReplaceStatus extends ChiselEnum {
  val replace_invalid, replace_finish, replace_replay = Value
}

// probe -> main pipe req status
object ProbeRespStatus extends ChiselEnum {
  val probe_invalid, probe_finish, probe_replay = Value
}

// mshr <--> probe status
object ProbeMSHRState {
  val width = 2

  def miss      = 0.U(width.W)
  def hitGo     = 1.U(width.W)
  def hitBlockN = 2.U(width.W)
  def hitBlockB = 3.U(width.W)
}
