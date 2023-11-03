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
import darecreek.lsu._

class VPUCore extends Module {
  val io = IO(new Bundle {
    // OVI issue
    val ovi_issue = new OVIissue
    // OVI dispatch
    val ovi_dispatch = new OVIdispatch
    // OVI completed
    val ovi_completed = new OVIcompleted
    // OVI memory related
    val ovi_memop = new OVImemop
    val ovi_load = new OVIload
    val ovi_store = new OVIstore
    val ovi_maskIdx = new OVImaskIdx
    // Debug: RVFI
    val rvfi = Output(new VRvfi)
    val pc = Input(UInt(xLen.W))
    val pc_out = Output(UInt(xLen.W))
  })

  val ctrlBlock = Module(new VCtrlBlock)
  val issueBlock = Module(new VIssueBlock)
  val exuBlock = Module(new VExuBlock)
  val lsu = Module(new VLsu)

  ctrlBlock.io.ovi_issue <> io.ovi_issue
  ctrlBlock.io.ovi_dispatch := io.ovi_dispatch
  io.ovi_completed := ctrlBlock.io.ovi_completed

  for (i <- 0 until VRenameWidth) {
    issueBlock.io.in.toArithIQ(i) <> ctrlBlock.io.out.toArithIQ(i) 
    issueBlock.io.in.toLsIQ(0)(i) <> ctrlBlock.io.out.toLdIQ(i)
    issueBlock.io.in.toLsIQ(1)(i) <> ctrlBlock.io.out.toStaIQ(i)
  }
  issueBlock.io.fromBusyTable := ctrlBlock.io.readBusyTable
  ctrlBlock.io.wbArith := issueBlock.io.wbArith
  ctrlBlock.io.wbLSU := issueBlock.io.wbLSU
  issueBlock.io.flush := ctrlBlock.io.flush
  issueBlock.io.get_rs1 <> ctrlBlock.io.get_rs1

  exuBlock.io.in <> issueBlock.io.toExu
  exuBlock.io.flush := ctrlBlock.io.flush
  issueBlock.io.toExu.readys := exuBlock.io.in.readys
  issueBlock.io.fromExu := exuBlock.io.out


  lsu.io.fromIQ.ld <> issueBlock.io.toLSU.ld
  lsu.io.fromIQ.st <> issueBlock.io.toLSU.st
  issueBlock.io.fromLSU.ld := lsu.io.wb.ld
  issueBlock.io.fromLSU.st := lsu.io.wb.st
  issueBlock.io.fromLSU.stateIsStore := lsu.io.stateIsStore

  lsu.io.ovi_memop <> io.ovi_memop
  lsu.io.ovi_load := io.ovi_load
  lsu.io.ovi_store <> io.ovi_store
  lsu.io.ovi_maskIdx <> io.ovi_maskIdx

  /**
    *  Debug
    */
  val rvfiBlock = Module(new VRvfiBlock)
  rvfiBlock.io.commits := ctrlBlock.io.commits
  rvfiBlock.io.sb_id := ctrlBlock.io.rvfi_sb_id
  rvfiBlock.io.commitEnd := ctrlBlock.io.commitEnd
  rvfiBlock.io.rfRd <> issueBlock.io.rfRdRvfi
  if (debug) {
    io.rvfi := rvfiBlock.io.rvfi
    io.pc_out := RegNext(io.pc)
  } else {
    io.rvfi := 0.U.asTypeOf(new VRvfi)
    io.pc_out := 0.U
  }
}

object Main extends App {

  println("Generating the VPU Core hardware")

  emitVerilog(new VPUCore(), Array("--target-dir", "generated"))

}