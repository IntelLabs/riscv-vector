package darecreek.exu.vfu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

case object VFuParamsKey extends Field[VFuParameters]

case class VFuParameters
(
  XLEN: Int = 64,
  VLEN: Int = 128
)

trait HasVFuParameters {
  implicit val p: Parameters
  
  val vfuParams = p(VFuParamsKey)

  val XLEN = vfuParams.XLEN
  val VLEN = vfuParams.VLEN
  val bVL = log2Up(VLEN) + 1
  val bVSTART = bVL - 1
  val LaneWidth = 64  // constant
  def laneWidth = LaneWidth
  val NLanes = VLEN / LaneWidth  // must be power of 2
  val vlenb = VLEN / 8  //CSR
  def VLENB = vlenb
  val vlenbWidth = log2Up(vlenb)
}

abstract class VFuModule(implicit val p: Parameters) extends Module with HasVFuParameters
abstract class VFuBundle(implicit val p: Parameters) extends Bundle with HasVFuParameters

// object VFUParam {
//   val XLEN = 64
//   val VLEN = 128
//   val VLENB = VLEN/8
//   val bVL = log2Up(VLEN) + 1
//   val bVSTART = bVL - 1
// }

// trait DarecreekParametersVFU {
//   //---- Come from scalar parameters ----
//   val xLen = 64
//   val NPhyRegs: Int = 192 // Scalar PRF
//   val RobSize = 192
//   val CommitWidth = 6
//   //-------------------------------------

//   val VLEN = 128  // Must be power of 2
//   val bVL = log2Up(VLEN) + 1
//   val bVstart = bVL - 1

//   // Vector Queue
//   val VQSize = 32
//   // Decode
//   val VDecodeWidth = 1
//   // Rename
//   val VRenameWidth = 2
//   // Paralle Expander
//   val ExpdLenAccWidth = log2Floor(8 * VRenameWidth) + 1 //Todo: test if it is enough
//   // Commit
//   val VCommitWidth = VRenameWidth
//   val VRobSize = 96
//   val SysRobIdxWidth = 8  //system rob index width, will be replaced with scalar param
//   // Issue Queue
//   val NArithIQs = 1
//   val ArithIQSize = 40
//   val LsIQSize = 20
//   val LdIQSize = 20
//   val StaIQSize = 20
//   val StdIQSize = 20
//   val NLaneExuFUs = 4
//   val NArithFUs = NLaneExuFUs + 3 // Number of FUs in EXU

//   val NVPhyRegs: Int = 96  // Vector PRF
//   val SPRegIdxWidth = log2Up(NPhyRegs) // Scalar
//   val VPRegIdxWidth = log2Up(NVPhyRegs) // Vector

//   val nVRFWritePorts = NArithIQs + 1

//   val LaneWidth = 64  // constant
//   val NLanes = VLEN / LaneWidth  // must be power of 2
//   val LaneIdxWidth = log2Up(NLanes)
//   val NByteLane = LaneWidth / 8

//   val vlenb = VLEN / 8  //CSR
//   val vlenbWidth = log2Up(vlenb)

//   //---- Just for debug ----
//   val debug = true
//   val nVRFReadPortsRvfi = VCommitWidth
 
// }

// object DarecreekParamVFU extends DarecreekParametersVFU