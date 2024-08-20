package smartVector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

case object SmartParamsKey extends Field[SmartParameters]

// Todo: some parameters should come from scalar parameters

case class SmartParameters(
    val VLEN: Int = 128
) {
  val bVL:     Int = log2Up(VLEN) + 1
  val bVstart: Int = bVL - 1
  val bVSTART = bVL - 1
}

trait smartParas {
  val VLEN = 128
  val XLEN = 64
  val bVL:     Int = log2Up(VLEN) + 1
  val bVstart: Int = bVL - 1
  val bVSTART       = bVL
  val VPRegIdxWidth = 5
  val NVPhyRegs     = 32
  val NLanes        = 2
  val LaneWidth     = 64

  val vlenb      = VLEN / 8
  val vlenbWidth = log2Up(vlenb) + 1

  // LSU
  val addrWidth = 64
  val dataWidth = 128
  val dataBytes = dataWidth / 8
  // val ldstUopQueueSize  = 17
  // val ldstUopQueueWidth = log2Up(ldstUopQueueSize)

  val nHLsuQueueEntries = 4
  val nHLsuQueueWidth   = log2Up(nHLsuQueueEntries)

  val vLdstUopQueueSize  = 4
  val vLdstUopQueueWidth = log2Up(vLdstUopQueueSize)
}

object SmartParam extends smartParas
