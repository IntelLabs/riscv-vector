/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

package darecreek

import chisel3._
import chisel3.util._
import utils._
import matrix._
import matrix.MatrixParameters._

// OVI (Open Vector Interface)
class OVIissue extends Bundle {
  val inst = Input(UInt(32.W))
  val scalar_opnd = Input(UInt(64.W))
  val sb_id = Input(UInt(5.W))
  val v_csr = Input(UInt(43.W))
  val valid = Input(Bool())
  val credit = Output(Bool())
}
class OVIdispatch extends Bundle {
  val sb_id = Input(UInt(5.W))
  val next_senior = Input(Bool())
  val kill = Input(Bool())
}
class OVIcompleted extends Bundle {
  val sb_id = Output(UInt(5.W))
  val fflags = Output(UInt(5.W))
  val vxsat = Output(Bool())
  val valid = Output(Bool())
  val dest_reg = Output(UInt(64.W))
  val vstart = Output(UInt(14.W))
  val illegal = Output(Bool())
}
class OVImemop extends Bundle {
  val sync_start = Output(Bool())
  val sync_end = Input(Bool())
  val sb_id = Input(UInt(5.W))
  val vstart_vlfof = Input(UInt(15.W))
}
class OVIload extends Bundle {
  val data = Input(UInt(512.W))
  val seq_id = Input(UInt(34.W))
  val valid = Input(Bool())
  val mask = Input(UInt(64.W))
  val mask_valid = Input(Bool())
}
class OVIstore extends Bundle {
  val data = Output(UInt(VLEN.W))
  val valid = Output(Bool())
  val credit = Input(Bool())
}
class OVImaskIdx extends Bundle {
  val item = Output(UInt(65.W))
  val valid = Output(Bool())
  val last_idx = Output(Bool())
  val credit = Input(Bool())
}

// Decoder output
class VCtrl extends Bundle {
  //-- Fields of instruction --
  val lsrc = Vec(2, UInt(5.W)) //0: vs1/imm5   1: vs2
  val ldest = UInt(5.W)
  val vm = Bool() // vector mask
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W) // 14..12

//  //-- Decoded signals --
  val matrix  = if (hasMatrix) Bool() else null
  val illegal = Bool()
  val lsrcVal = Vec(3, Bool()) //0: vs1   1: vs2   2: 3rd operand (and vs3?)
  val ldestVal = Bool()
  val rdVal = Bool()  // Has scalar dest operand?
  val load = Bool()
  val store = Bool()
  val arith = Bool()
  val crossLane = Bool() // Goto Cross-lane EXU
  val alu = Bool() // All low-latency operations
  val mul = Bool()
  val fp = Bool()
  val div = Bool()
  val fixP = Bool()
  val redu = Bool()
  val mask = Bool()
  val perm = Bool()
  val widen = Bool()  // 2*sew = sew op sew  //Reduction not included
  val widen2 = Bool() // 2*sew = 2*sew op sew
  val narrow = Bool() // sew = 2*sew op sew
  val narrow_to_1 = Bool() // Compare, carry-out producing instructions
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2) 
  def vi = !funct3(2) && funct3(1) && funct3(0) 
  def fuSel = Seq(alu, mul, fp, redu, mask, perm, div)
  def laneExu = arith && !crossLane
  def isLdst = load || store
  def vs1_imm = lsrc(0)
  def opi = funct3(0) === funct3(1) // OPIVV/X/I
  def opm = funct3(1, 0) === 2.U //OPMVV/X
  def opf = funct3(1, 0) === 1.U // OPFVV/F
}

class VExcptInfo extends Bundle {
  val excpt = Bool()
  val excptVec = Vec(16, Bool())
  val excptPosition = UInt(bVstart.W)
}

// Vector CSR
class VCsr extends Bundle {
  val vstart = UInt(bVstart.W)
  val vl = UInt(bVL.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
  val vlmul = UInt(3.W)
  val vsew = UInt(3.W)
  val vill = Bool()
  val ma = Bool()
  val ta = Bool()
}
// Information of the micro-op which is not directly from decoder.
// Basically they are information for both unexpanded and expanded uop
class VInfo extends VCsr {
  val destEew = UInt(3.W) // Destination EEW
  val emulVd = UInt(4.W) // EMUL of vd
  val emulVs2 = UInt(4.W)
  val vstart_gte_vl = Bool()
}

class VCtrlInfo extends Bundle {
  val ctrl = new VCtrl
  val info = new VInfo
  def ill = ctrl.illegal || info.vill
}

// Add scalar data and info
class VMicroOp extends VCtrlInfo {
  val sb_id = UInt(5.W)
  val vRobIdx = new VRobPtr
  // val rs1 = UInt(xLen.W)  // scalar operand
  // val rs2 = UInt(xLen.W)  // scalar operand
  // val spdest = UInt(SPRegIdxWidth.W)
}

// Expanded micro-op before renaming
class VExpdUOpForRename extends VMicroOp {
  val expdLen = UInt(4.W) // Number of expanded uops
  val expdIdx = UInt(3.W) // Idx of expanded uop
  val expdEnd = Bool()    // The last one of expanded uops
  val lsrcExpd = Vec(2, UInt(5.W)) //0: vs1   1: vs2 
  val ldestExpd = UInt(5.W)
  val lsrcValExpd = Vec(3, Bool())  //only vector
  val lmaskValExpd = Bool()         //only vector
  val ldestValExpd = Bool()         //only vector
}

// Expanded micro-op after renaming
class VExpdUOp extends VMicroOp {
  // val vRobIdx = new VRobPtr
  val expdLen = UInt(4.W) // Number of expanded uops
  val expdIdx = UInt(3.W) // Idx of expanded uop
  val expdEnd = Bool()
  val lsrcExpd = Vec(2, UInt(5.W)) //0: vs1   1: vs2 
  val ldestExpd = UInt(5.W)
  val psrc = Vec(4, UInt(VPRegIdxWidth.W)) // Idx of physial RF of vector src operands
             // (0): vs1   (1): vs2   (2): old_pdest   (3): pmask
  val pdest = UInt(VPRegIdxWidth.W) // Idx of physical RF of vector dest operand
  val psrcVal = Vec(4, Bool())      // Valid of four vector source operands (the 4th is mask)
  val pdestVal = Bool()             // Valid of vector dest operand
}

class SewOH extends Bundle {  // 0   1   2   3
  // val oneHot = Vec(4, Bool()) // 8, 16, 32, 64
  val oneHot = UInt(4.W) // b0-b3: 8, 16, 32, 64
  def is8 = oneHot(0)
  def is16 = oneHot(1)
  def is32 = oneHot(2)
  def is64 = oneHot(3)
}
object SewOH {
  def apply(vsew: UInt): SewOH = {
    val sew = Wire(new SewOH)
    // sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U))
    sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)).asUInt
    sew
  }
}

// Input of Arith EXU
class VExuInput extends Bundle {
  val uop = new VExpdUOp
  val rs1 = UInt(xLen.W)
  val vSrc = Vec(4, Vec(NLanes, UInt(LaneWidth.W))) //vs1, vs2, old_vd, mask
  val vstartRemain = UInt(bVstart.W)
  val vlRemain = UInt(bVL.W)
}
// Outputs of Arith EXU
class VLaneExuOut extends Bundle {
  val uop = new VExpdUOp
  val vd = Vec(NLanes, UInt(LaneWidth.W))
  val fflags = UInt(5.W) // Floating-point accrued exception flag
  val vxsat = Bool() // Fixed-point accrued saturation flag
}
class VCrossExuOut extends Bundle {
  val uop = new VExpdUOp
  val vd = Vec(NLanes, UInt(LaneWidth.W))
  val fflags = UInt(5.W) // Floating-point accrued exception flag
}

// Input of the lane FU
class LaneFUInput extends Bundle {
  val uop = new VExpdUOp
  val vs1 = UInt(LaneWidth.W)
  val vs2 = UInt(LaneWidth.W)
  val old_vd = UInt(LaneWidth.W)
  val rs1 = UInt(xLen.W)
  val prestart = UInt(NByteLane.W)
  val mask = UInt(NByteLane.W)
  val tail = UInt(NByteLane.W)
}
// Output of the lane FU
class LaneFUOutput extends Bundle {
  val uop = new VExpdUOp
  val vd = UInt(LaneWidth.W)
  val fflags = UInt(5.W) // Floating-point accrued exception flag
  val vxsat = Bool() // Fixed-point accrued saturation flag
}

// Write back of arithmetic exu
class WbArith_lane extends Bundle {
  val uop = new VExpdUOp
  val fflags = UInt(5.W)
  val vxsat = Bool()
  val rd = UInt(xLen.W)  // Only for OVI
}
class WbArith_cross extends Bundle {
  val uop = new VExpdUOp
  val fflags = UInt(5.W)
  val rd = UInt(xLen.W)  // Only for OVI
}

class VRobCommitInfo extends Bundle {
  val ldest = UInt(5.W)
  val pdest = UInt(VPRegIdxWidth.W)
  val pdestVal = Bool()
  val old_pdest = UInt(VPRegIdxWidth.W)
}

class VRobCommitIO extends Bundle {
  val valid = Vec(VCommitWidth, Output(Bool()))
  val info = Vec(VCommitWidth, Output(new VRobCommitInfo))
}

class Redirect extends Bundle {
  val valid = Bool()
}

/**
  *  Debug signals
  */
class VRvfi extends Bundle {
  val valid = Bool()
  val sb_id = UInt(5.W)
  val vd_addr = UInt(5.W)
  val vd_emul = UInt(3.W)
  val vd_wdata = UInt((VLEN*8).W)
  val rd_addr = UInt(5.W)
  val rd_wdata = UInt(xLen.W)
  val fflags = UInt(5.W)
  val vxsat = Bool()
}