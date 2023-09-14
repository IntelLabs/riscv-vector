/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// package xiangshan.backend.rename
package darecreek

// import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
// import xiangshan._
import utils._

class VBusyTableReadIO extends Bundle {
  val req = Input(UInt(VPRegIdxWidth.W))
  val resp = Output(Bool())
}

class VBusyTable(numReadPorts: Int, numWritePorts: Int) extends Module {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    // set preg state to busy
    val allocPregs = Vec(VRenameWidth, Flipped(ValidIO(UInt(VPRegIdxWidth.W))))
    // set preg state to ready (write back regfile + rob walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(VPRegIdxWidth.W))))
    // read preg state
    val read = Vec(numReadPorts, new VBusyTableReadIO)
  })

  val table = RegInit(0.U(NVPhyRegs.W))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)

  val tableAfterWb = table & (~wbMask).asUInt
  val tableAfterAlloc = tableAfterWb | allocMask

  io.read.map(r => r.resp := table(r.req))

  table := tableAfterAlloc

  when (io.flush) {
    table := 0.U
  }

  // XSDebug(p"table    : ${Binary(table)}\n")
  // XSDebug(p"tableNext: ${Binary(tableAfterAlloc)}\n")
  // XSDebug(p"allocMask: ${Binary(allocMask)}\n")
  // XSDebug(p"wbMask   : ${Binary(wbMask)}\n")
  // for (i <- 0 until NRPhyRegs) {
  //   XSDebug(table(i), "%d is busy\n", i.U)
  // }

  // XSPerfAccumulate("busy_count", PopCount(table))

  // val perfEvents = Seq(
  //   ("std_freelist_1_4_valid", (PopCount(table) < (NRPhyRegs.U/4.U))                                             ),
  //   ("std_freelist_2_4_valid", (PopCount(table) > (NRPhyRegs.U/4.U)) & (PopCount(table) <= (NRPhyRegs.U/2.U))    ),
  //   ("std_freelist_3_4_valid", (PopCount(table) > (NRPhyRegs.U/2.U)) & (PopCount(table) <= (NRPhyRegs.U*3.U/4.U))),
  //   ("std_freelist_4_4_valid", (PopCount(table) > (NRPhyRegs.U*3.U/4.U))                                         )
  // )
  // generatePerfEvent()
}
