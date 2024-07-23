package smartVector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config._

case object SmartParamsKey extends Field[SmartParameters]


// Todo: some parameters should come from scalar parameters

case class  SmartParameters (
  val VLEN: Int = 128
){
  val bVL: Int = log2Up(VLEN) + 1
  val bVstart: Int = bVL - 1
  val bVSTART = bVL - 1
}

trait smartParas {
  val VLEN = 128
  val XLEN = 64
  val bVL: Int = log2Up(VLEN) + 1
  val bVstart: Int = bVL - 1
  val bVSTART = bVL 
  val VPRegIdxWidth = 5
  val NVPhyRegs = 32
  val NLanes = 2
  val LaneWidth = 64

  val vlenb = VLEN / 8
  val vlenbWidth = log2Up(vlenb) + 1

  // LSU
  val addrWidth = 64
  val dataWidth = 128
  val dataBytes = dataWidth / 8
  val ldstUopQueueSize = 17
  val ldstUopQueueWidth = log2Up(ldstUopQueueSize)

  val vLdstUopQueueSize = 4
  val vLdstUopQueueWidth = log2Up(vLdstUopQueueSize)
}

object SmartParam extends smartParas
  //---- Come from scalar parameters ----
  //val xLen = 64
  //val NPhyRegs: Int = 192 // Scalar PRF
  //val RobSize = 192
  //val CommitWidth = 6
  ////-------------------------------------
//
  //val VLEN = 256  // Must be power of 2
  //val bVL = log2Up(VLEN) + 1
  //val bVstart = bVL - 1
//
  //// Vector Queue
  //val VQSize = 32
  //// Decode
  //val VDecodeWidth = 1
  //// Rename
  //val VRenameWidth = 2
  //// Paralle Expander
  //val ExpdLenAccWidth = log2Floor(8 * VRenameWidth) + 1 //Todo: test if it is enough
  //// Commit
  //val VCommitWidth = VRenameWidth
  //// val VRobSize = 96
  //val SysRobIdxWidth = 8  //system rob index width, will be replaced with scalar param
  //// Issue Queue
  //val NArithIQs = 1
  //val ArithIQSize = 40
  //val LsIQSize = 20
  //val LdIQSize = 20
  //val StaIQSize = 20
  //val StdIQSize = 20
  //val NLaneExuFUs = 4
  //val NArithFUs = NLaneExuFUs + 3 // Number of FUs in EXU
//
  //val NVPhyRegs: Int = 96  // Vector PRF
  //val SPRegIdxWidth = log2Up(NPhyRegs) // Scalar
  //val VPRegIdxWidth = log2Up(NVPhyRegs) // Vector
//
  //val nVRFWritePorts = NArithIQs + 1
//
  //val LaneWidth = 64  // constant
  //val NLanes = VLEN / LaneWidth  // must be power of 2
  //val LaneIdxWidth = log2Up(NLanes)
  //val NByteLane = LaneWidth / 8
//
  //val vlenb = VLEN / 8  //CSR
  //val vlenbWidth = log2Up(vlenb)
//
  ////---- Just for debug ----
  //val debug = true
  //val nVRFReadPortsRvfi = VCommitWidth


