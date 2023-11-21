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

/**
  * Constraint: so far all inputs of io.in share one 'ready'.
  *             All outputs of one Iq share one 'ready'.
  *             So far there is only one arithmetic issue queue (NArithIQs = 1).
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._

class VDispatch extends Module {
  val io = IO(new Bundle {
    val in = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
    // to Issue Queues
    val out = new Bundle {
      val toArithIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
      val toLdIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
      val toStaIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
      // val toStdIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
    }
    val toRob = Vec(VRenameWidth, Decoupled(new VExpdUOp))
    // To read the busy table
    val readBusyTable = Vec(VRenameWidth, Vec(4, Output(UInt(VPRegIdxWidth.W))))
    val flush = Input(Bool())
  })

  val inputHasValid = Cat(io.in.map(_.valid)).orR
  val rdyRob = io.toRob(0).ready
  for (i <- 0 until VRenameWidth) {
    val canOut = io.out.toArithIQ(0).ready && io.out.toLdIQ(0).ready &&
                 io.out.toStaIQ(0).ready // && io.out.toStdIQ(0).ready && io.toRob(0).ready
    io.in(i).ready := !inputHasValid || canOut
    
    val isArith = io.in(i).bits.ctrl.arith
    val isLd = io.in(i).bits.ctrl.load
    val isSt = io.in(i).bits.ctrl.store
    val rdyArith = io.out.toArithIQ(0).ready
    val rdyLd = io.out.toLdIQ(0).ready
    val rdySta = io.out.toStaIQ(0).ready
    
    // -- NOTE: so far there is only one arithmetic issue queue (NArithIQs = 1)
    io.out.toArithIQ(i).valid := io.in(i).valid && isArith && rdyLd && rdySta && rdyRob
    io.out.toLdIQ(i).valid := io.in(i).valid && isLd && rdyArith && rdySta && rdyRob
    io.out.toStaIQ(i).valid := io.in(i).valid && isSt && rdyArith && rdyLd && rdyRob
    // io.out.toStdIQ(i).valid := io.in(i).valid && isSt
    io.toRob(i).valid := io.in(i).valid && rdyArith && rdyLd && rdySta
    io.out.toArithIQ(i).bits := io.in(i).bits
    io.out.toLdIQ(i).bits := io.in(i).bits
    io.out.toStaIQ(i).bits := io.in(i).bits
    // io.out.toStdIQ(i).bits := io.in(i).bits
    io.toRob(i).bits := io.in(i).bits

    // // To write busy table 
    // io.allocPregs(i).valid := io.in(i).valid && io.in(i).bits.pdestVal && !io.flush
    // io.allocPregs(i).bits := io.in(i).bits.pdest
    // To read busy table
    io.readBusyTable(i) := io.in(i).bits.psrc
  }

}