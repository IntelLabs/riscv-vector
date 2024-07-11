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

package darecreek.exu.vfu

import chisel3._
import chisel3.util._
// import darecreek.exu.vfu.VFUParam._
import chipsalliance.rocketchip.config.Parameters
import xiangshan.{MicroOp, Redirect}

// Temporary. Will replaced by system Uop class.
class VUopCtrl extends Bundle {
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W)
  val vm = Bool()
  val vs1_imm = UInt(5.W)
  val widen = Bool()
  val widen2 = Bool()
  val narrow = Bool()
  val narrow_to_1 = Bool()
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2)
  def vi = !funct3(2) && funct3(1) && funct3(0)
} 
class VUopInfo(implicit p: Parameters) extends VFuBundle {
  val ma = Bool()
  val ta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
  val vl = UInt(bVL.W)
  val vstart = UInt(bVSTART.W)
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
}

class VUop(implicit p: Parameters) extends Bundle with ConnectFromLaneUop {
  val ctrl = new VUopCtrl
  val info = new VUopInfo
  val uopIdx = UInt(3.W)
  val uopEnd = Bool()
  val scalarRegWriteEn = Bool()
  val floatRegWriteEn = Bool()
  val rfWriteEn = Bool()
  val ldest = UInt(5.W)
  //For Permutation
  val permExpdLen = UInt(4.W)
  val regDstIdx = UInt(5.W)
  val regCount  = UInt(4.W)
  // Temp: system uop
  val sysUop = new MicroOp
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

// Input of FU
class VFuInput(implicit p: Parameters) extends VFuBundle {
  val uop = new VUop
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val rs1 = UInt(XLEN.W)
  val oldVd = UInt(128.W)
  val mask = UInt(128.W)
}
// Output of ALU
class VAluOutput extends Bundle {
  val vd = UInt(128.W)
  val vxsat = Bool()
}
// class VFpuOutput extends Bundle {
//   val vd = UInt(128.W)
//   val fflags = UInt(5.W) // Floating-point accrued exception flag
// }

// Output of FPU
class VFpuOutput(implicit p: Parameters) extends Bundle {
  val uop = new VUop
  val vd = UInt(128.W)
  val fflags = UInt(5.W)
}

class VPermInput(implicit p: Parameters) extends Bundle {
  val uop = new VUop
  val rs1 = UInt(64.W)
  val vs1_preg_idx = Vec(8, UInt(8.W))
  val vs2_preg_idx = Vec(8, UInt(8.W))
  val old_vd_preg_idx = Vec(8, UInt(8.W))
  val mask_preg_idx = UInt(8.W)
  val uop_valid = Bool()
  val rdata = UInt(128.W)
  val rvalid = Bool()
}

class VPermOutput(implicit p: Parameters) extends Bundle {
  val uop = new VUop
  val rd_en = Bool()
  val rd_preg_idx = UInt(8.W)
  val wb_vld = Bool()
  val wb_data = UInt(128.W)
  val perm_busy = Bool()
}


/** Lane FU related intefaces.
 *  To wrapper the lane-based FP/DIV code.
 */
class VCtrl extends Bundle {
  val lsrc = Vec(2, UInt(5.W)) //0: vs1/imm5   1: vs2
  val ldest = UInt(5.W)
  val vm = Bool() // vector mask
  val funct6 = UInt(6.W)
  val funct3 = UInt(3.W) // 14..12

  // val illegal = Bool()
  // val lsrcVal = Vec(3, Bool()) //0: vs1   1: vs2   2: 3rd operand (and vs3?)
  // val ldestVal = Bool()
  // val rdVal = Bool()  // Has scalar dest operand?
  // val load = Bool()
  // val store = Bool()
  // val arith = Bool()
  // val crossLane = Bool() // Goto Cross-lane EXU
  // val alu = Bool() // All low-latency operations
  // val mul = Bool()
  // val fp = Bool()
  // val div = Bool()
  // val fixP = Bool()
  // val redu = Bool()
  // val mask = Bool()
  // val perm = Bool()
  val widen = Bool()  // 2*sew = sew op sew  //Reduction not included
  val widen2 = Bool() // 2*sew = 2*sew op sew
  val narrow = Bool() // sew = 2*sew op sew
  val narrow_to_1 = Bool() // Compare, carry-out producing instructions
  def vv = !funct3(2) && !(funct3(1) && funct3(0))
  def vx = funct3(2) 
  def vi = !funct3(2) && funct3(1) && funct3(0) 
  // def fuSel = Seq(alu, mul, fp, div, redu, mask, perm)
  // def laneExu = arith && !crossLane
  // def isLdst = load || store
}

class VInfo(implicit p: Parameters) extends VFuBundle {
  val vstart = UInt(bVSTART.W) // from CSR
  val vl = UInt(bVL.W) //---- Todo: width reduction
  val vxrm = UInt(2.W)
  val frm = UInt(3.W)
  val vlmul = UInt(3.W) // see spec
  val vsew = UInt(3.W)  // see spec
  // val vill = Bool()
  val ma = Bool() // mask agnostic
  val ta = Bool() // tail agnostic
  // val lmul = UInt(4.W) // 1, 2, 4, 8
  val destEew = UInt(3.W) // Destination EEW
  // val wenRF = Bool() // RF wen. E.g., vstart >= vl or vl=0 
}

class VCtrlInfo(implicit p: Parameters) extends Bundle {
  val ctrl = new VCtrl
  val info = new VInfo
}

// Expanded micro-op after renaming
class VExpdUOp(implicit p: Parameters) extends VCtrlInfo with ConnectFromVUop {
  // val vRobIdx = new VRobPtr
  // val expdLen = UInt(4.W) // Number of expanded uops
  val expdIdx = UInt(3.W) // Idx of expanded uop
  val expdEnd = Bool()
  // val lsrcExpd = Vec(2, UInt(5.W)) //0: vs1   1: vs2 
  // val ldestExpd = UInt(5.W)
  // val psrc = Vec(4, UInt(VPRegIdxWidth.W)) // Idx of physial RF of vector src operands
             // (0): vs1   (1): vs2   (2): old_pdest   (3): pmask
  // val pdest = UInt(VPRegIdxWidth.W) // Idx of physical RF of vector dest operand
  // val psrcVal = Vec(4, Bool())      // Valid of four vector source operands (the 4th is mask)
  val pdestVal = Bool()             // Valid of vector dest operand
  // Temp: system uop
  val sysUop = new MicroOp
}

// Input of the lane FU
class LaneFUInput(implicit p: Parameters) extends VFuBundle {
  val uop = new VExpdUOp
  val vs1 = UInt(64.W)
  val vs2 = UInt(64.W)
  val old_vd = UInt(64.W)
  val rs1 = UInt(XLEN.W)
  val prestart = UInt(8.W)
  val mask = UInt(8.W)
  val tail = UInt(8.W)
}
// Output of the lane FU
class LaneFUOutput(implicit p: Parameters) extends Bundle {
  val uop = new VExpdUOp
  val vd = UInt(64.W)
  val fflags = UInt(5.W) // Floating-point accrued exception flag
  val vxsat = Bool() // Fixed-point accrued saturation flag
}

class LaneUnit(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new LaneFUInput))
    val out = DecoupledIO(new LaneFUOutput)
  })
}
 
trait ConnectFromLaneUop { this: VUop =>
  def connectFromLaneUop(laneUop: VExpdUOp)(implicit p: Parameters) = {
    this.elements.foreach {case (name, data) =>
      data match {
        case x:Bundle =>
        if (name != "sysUop") {
        x.elements.foreach {case (name2, data2) =>
          if (name2 != "vs1_imm") {
            data2 := laneUop.elements(name).asTypeOf( 
              {if (name == "ctrl") {new VCtrl} else {new VInfo()(p)}}).elements(name2)
          }
        }
        }
        case _ => {}
      }
    }
    this.ctrl.vs1_imm := laneUop.ctrl.lsrc(0)
    this.uopIdx := laneUop.expdIdx
    this.uopEnd := laneUop.expdEnd
    this.sysUop := laneUop.sysUop
  }
}

trait ConnectFromVUop { this: VExpdUOp =>
  def connectFromVUop(vUop: VUop, isDiv: Boolean = false)(implicit p: Parameters) = {
    this.elements.foreach {case (name, data) =>
      data match {
        case x:Bundle =>
        if (name != "sysUop") {
        x.elements.foreach {case (name2, data2) =>
          if (!(Array("destEew", "lsrc", "ldest").contains(name2))) {
            data2 := vUop.elements(name).asTypeOf( 
              {if (name == "ctrl") {new VUopCtrl} else {new VUopInfo()(p)}}).elements(name2)
          }
        }
        }
        case _ => {}
      }
    }
    this.info.destEew := {if (isDiv) {vUop.info.vsew} else {
      Mux(vUop.ctrl.widen || vUop.ctrl.widen2, vUop.info.vsew + 1.U, vUop.info.vsew)
    }}
    this.expdIdx := vUop.uopIdx
    this.expdEnd := vUop.uopEnd
    this.ctrl.lsrc(0) := vUop.ctrl.vs1_imm
    this.ctrl.lsrc(1) := DontCare
    this.ctrl.ldest := DontCare
    this.pdestVal := DontCare
    this.sysUop := vUop.sysUop
  }
}