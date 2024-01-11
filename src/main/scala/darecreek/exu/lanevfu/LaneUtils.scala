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

package darecreek.exu.lanevfu

import chisel3._
import chisel3.util._
import darecreek.exu.vfucore._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._

object LaneConnectFP {
  def uopInputFromCtrlToFu(fromUop: darecreek.VExpdUOp)(implicit p: Parameters) = {
    val toUop = Wire(new VExpdUOp)
    toUop.elements.foreach {case (name, data) =>
      if (name != "sysUop") {
        data match {
          case x: Bundle =>
            x.elements.foreach {case (name2, data2) =>
              data2 := fromUop.elements(name).asTypeOf( 
                if (name == "ctrl") {new VCtrl} else {new VInfo}).elements(name2)
            }
          case _ => {}
        }
      }
    }
    toUop.expdIdx := fromUop.expdIdx
    toUop.expdEnd := fromUop.expdEnd
    toUop.pdestVal := fromUop.pdestVal
    // toUop.sysUop := sysUop
    toUop
  }

  def laneInputFromCtrlToFu(fromLane: darecreek.LaneFUInput)(implicit p: Parameters) = {
    val toLane = Wire(new LaneFUInput)
    toLane.uop := uopInputFromCtrlToFu(fromLane.uop)
    toLane.elements.foreach { case (name, data) => 
      if (name != "uop") {
        data := fromLane.elements(name)
      }
    }
    toLane
  }

  def uopOutputFromFuToCtrl(fromUop: VExpdUOp)(implicit p: Parameters) = {
    val toUop = WireInit(0.U.asTypeOf(new darecreek.VExpdUOp))
    fromUop.elements.foreach {case (name, data) =>
      if (name != "sysUop") {
        data match {
          case x: Bundle =>
            x.elements.foreach {case (name2, data2) =>
              toUop.elements(name).asTypeOf( 
                if (name == "ctrl") {new VCtrl} else {new VInfo}).elements(name2) := data2
            }
          case _ => {}
        }
      }
    }
    toUop.expdIdx := fromUop.expdIdx
    toUop.expdEnd := fromUop.expdEnd
    toUop.pdestVal := fromUop.pdestVal
    toUop
  }

  def laneOutputFromFuToCtrl(fromLane: LaneFUOutput)(implicit p: Parameters) = {
    val toLane = Wire(new darecreek.LaneFUOutput)
    toLane.uop := uopOutputFromFuToCtrl(fromLane.uop)
    toLane.elements.foreach { case (name, data) => 
      if (name != "uop") {
        data := fromLane.elements(name)
      }
    }
    toLane
  }
    
}