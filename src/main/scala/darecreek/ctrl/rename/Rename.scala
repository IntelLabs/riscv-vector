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
  * Constraint: so far all inputs of io.in share one 'ready'. 'Ready' of all outputs are from one source.
  */

package darecreek

import chisel3._
import chisel3.util._

class VRename extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val robCommits = Flipped(new VRobCommitIO)
    // from parallel expander
    val in = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOpForRename)))
    // from/to rename table
    val ratReadPorts = Vec(VRenameWidth, Vec(4, Input(UInt(VPRegIdxWidth.W))))
    val ratWritePorts = Vec(VRenameWidth, Output(new VRatWritePort))
    // output
    val out = Vec(VRenameWidth, Decoupled(new VExpdUOp))
    // Allocated PRegs from Rename (to update the busy table)
    val allocPregs = Vec(VRenameWidth, ValidIO(UInt(VPRegIdxWidth.W)))
  })

  val freeList = Module(new VStdFreeList(NVPhyRegs - 32))
  freeList.io.flush := io.flush
  freeList.io.doAllocate := io.out(0).ready

  val canOut = io.out(0).ready && freeList.io.canAllocate
  
  val validCount = PopCount(io.in.map(_.valid))
  val robIdxHead = RegInit(0.U.asTypeOf(new VRobPtr))
  val robIdxHeadNext = Mux(io.flush, 0.U.asTypeOf(new VRobPtr),
                       Mux(canOut, robIdxHead + validCount, robIdxHead))
  robIdxHead := robIdxHeadNext

  val uops = Wire(Vec(VRenameWidth, new VExpdUOp))

  val needDest = Wire(Vec(VRenameWidth, Bool()))
  // val ratWen = Wire(Vec(VRenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR

  for (i <- 0 until VRenameWidth) {
    uops(i).ctrl := io.in(i).bits.ctrl
    uops(i).info := io.in(i).bits.info
    // uops(i).excptInfo := io.in(i).bits.excptInfo
    // uops(i).rs1 := io.in(i).bits.rs1
    uops(i).expdIdx := io.in(i).bits.expdIdx
    uops(i).expdLen := io.in(i).bits.expdLen
    uops(i).expdEnd := io.in(i).bits.expdEnd
    uops(i).lsrcExpd := io.in(i).bits.lsrcExpd
    uops(i).ldestExpd := io.in(i).bits.ldestExpd
    uops(i).psrcVal(0) := io.in(i).bits.lsrcValExpd(0)
    uops(i).psrcVal(1) := io.in(i).bits.lsrcValExpd(1)
    uops(i).psrcVal(2) := io.in(i).bits.lsrcValExpd(2)
    uops(i).psrcVal(3) := io.in(i).bits.lmaskValExpd
    uops(i).pdestVal := io.in(i).bits.ldestValExpd

    io.in(i).ready := !hasValid || canOut

    // allocate new phy regs
    needDest(i) := io.in(i).valid && io.in(i).bits.ldestValExpd
    freeList.io.allocateReq(i) := needDest(i)

    // uops(i).vRobIdx := robIdxHead + PopCount(io.in.take(i).map(_.valid))
    uops(i).vRobIdx := io.in(i).bits.vRobIdx
    uops(i).sb_id := io.in(i).bits.sb_id

    uops(i).psrc := io.ratReadPorts(i)

    uops(i).pdest := Mux(needDest(i), freeList.io.allocatePhyReg(i), 0.U)

    io.out(i).valid := io.in(i).valid && freeList.io.canAllocate
    io.out(i).bits := uops(i)
  }

  /**
    * Bypass (i is idx of parallel inputs):
    *  psrc(i) = Mux(bypass(i,i-1), pdest(i-1),
    *            Mux(bypass(i,i-2), pdest(i-2),
    *            ...
    *            Mux(bypass(i,0),   pdest(0),  rat_out(i))...))
    */
  for (i <- 1 until VRenameWidth) {
    val bypass = Wire(Vec(4, Vec(i, Bool())))
    // 0: src1  1: src2   2: old dest  3: mask
    bypass(0) := (0 until i) map { j => uops(i).lsrcExpd(0) === uops(j).ldestExpd && needDest(j) }
    io.out(i).bits.psrc(0) := bypass(0).zip(uops.take(i).map(_.pdest)).foldLeft(uops(i).psrc(0)) {
      case (psrc, (bypass, pdest)) => Mux(bypass, pdest, psrc)
    }
    bypass(1) := (0 until i) map { j => uops(i).lsrcExpd(1) === uops(j).ldestExpd && needDest(j) }
    io.out(i).bits.psrc(1) := bypass(1).zip(uops.take(i).map(_.pdest)).foldLeft(uops(i).psrc(1)) {
      case (psrc, (bypass, pdest)) => Mux(bypass, pdest, psrc)
    }
    bypass(2) := (0 until i) map { j => uops(i).ldestExpd === uops(j).ldestExpd && needDest(j) }
    io.out(i).bits.psrc(2) := bypass(2).zip(uops.take(i).map(_.pdest)).foldLeft(uops(i).psrc(2)) {
      case (psrc, (bypass, pdest)) => Mux(bypass, pdest, psrc)
    }
    bypass(3) := (0 until i) map { j => uops(j).ldestExpd === 0.U && needDest(j) }
    io.out(i).bits.psrc(3) := bypass(3).zip(uops.take(i).map(_.pdest)).foldLeft(uops(i).psrc(3)) {
      case (psrc, (bypass, pdest)) => Mux(bypass, pdest, psrc)
    }
  }

  // RAT write and free list deallocation
  for (i <- 0 until VRenameWidth) {
    io.ratWritePorts(i).wen := needDest(i) && freeList.io.canAllocate && freeList.io.doAllocate && !io.flush
    io.ratWritePorts(i).addr := uops(i).ldestExpd
    io.ratWritePorts(i).data := freeList.io.allocatePhyReg(i)

    freeList.io.freeReq(i) := io.robCommits.valid(i) && io.robCommits.info(i).pdestVal
    freeList.io.freePhyReg(i) := io.robCommits.info(i).old_pdest
  }

  for (i <- 0 until VRenameWidth) {
    // To write busy table 
    io.allocPregs(i).valid := io.out(i).valid && io.out(i).bits.pdestVal && !io.flush
    io.allocPregs(i).bits := io.out(i).bits.pdest
  }
}