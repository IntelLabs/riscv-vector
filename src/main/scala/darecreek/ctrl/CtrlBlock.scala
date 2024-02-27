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

class PartialVInfo extends Bundle {
  val vRobPtr = new VRobPtr
  val destEew = UInt(3.W)
  val emulVd = UInt(4.W)
  val emulVs2 = UInt(4.W)
  val vstart_gte_vl = Bool()
}

class VCtrlBlock extends Module {
  val io = IO(new Bundle {
    // OVI issue
    val ovi_issue = new OVIissue
    // OVI dispatch
    val ovi_dispatch = new OVIdispatch
    // OVI completed
    val ovi_completed = new OVIcompleted
    // Dispatch outputs
    val out = new Bundle {
      val toArithIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
      val toLsIQ = Vec(VRenameWidth, Decoupled(new VExpdUOp))
    }
    // writeback: to update busyTable, to ROB
    val wbArith_laneAlu = Input(ValidIO(new WbArith_lane))
    val wbArith_laneMulFp = Input(ValidIO(new WbArith_lane))
    val wbArith_cross = Input(ValidIO(new WbArith_cross))
    val wbLSU = Input(ValidIO(new VExpdUOp))
    val readBusyTable = Vec(VRenameWidth, Vec(4, Output(Bool())))
    // rs1 read requests from issue queues
    val get_rs1 = Flipped(new GetScalarOperand)
    // flush generated by ROB
    val flush = Output(Bool())
    // Just for debug
    val commits = new VRobCommitIO
    val rvfi_sb_id = Output(UInt(5.W))
    val commitEnd = Output(Bool())
  })

  val decoder = Module(new VDecodeUnit)
  val infoCalc = Module(new VInfoCalc)
  val vq = Module(new VecQueue)
  val expander = Module(new ParallelExpander)
  val rat = Module(new VRenameTableWrapper)
  val rename = Module(new VRename)
  val busyTable = Module(new VBusyTable(VRenameWidth * 4, nVRFWritePorts))
  val dispatch = Module(new VDispatch)
  val rob = Module(new VRob)

  val flush = rob.io.flush.valid
  io.flush := flush

  // decode
  decoder.io.in <> io.ovi_issue
  decoder.io.issueCredit := rob.io.ovi_issueCredit

  // infoCalc
  infoCalc.io.ctrl := decoder.io.out.bits.ctrl
  infoCalc.io.csr := decoder.io.out.bits.csr
  val partialVInfo_wire = Wire(ValidIO(new PartialVInfo))
  val partialVInfo_reg = Reg(ValidIO(new PartialVInfo))
  // For narrow-to-1, set destEew = veewVs2 to facilitate tail/mask generation 
  partialVInfo_wire.bits.destEew := Mux(decoder.io.out.bits.ctrl.narrow_to_1,
                    infoCalc.io.infoAll.veewVs2, infoCalc.io.infoAll.veewVd)
  partialVInfo_wire.bits.emulVd := infoCalc.io.infoAll.emulVd
  partialVInfo_wire.bits.emulVs2 := infoCalc.io.infoAll.emulVs2
  partialVInfo_wire.bits.vstart_gte_vl := infoCalc.io.infoAll.vstart_gte_vl
  partialVInfo_wire.bits.vRobPtr := vq.io.enqPtrOut
  partialVInfo_wire.valid := decoder.io.out.valid
  when (decoder.io.out.valid) {
    partialVInfo_reg := partialVInfo_wire
  }
  partialVInfo_reg.valid := decoder.io.out.valid
  
  // vq
  vq.io.in := decoder.io.out
  vq.io.flush := rob.io.flush
  vq.io.get_rs1 <> io.get_rs1

  DecoupledConnect(vq.io.out, expander.io.in(0), flush)
  // Temp: disable the second input of expander
  expander.io.in(1).valid := false.B
  expander.io.in(1).bits := 0.U.asTypeOf(new VMicroOp)
  for (i <- 0 until VRenameWidth) {
    DecoupledConnect(expander.io.out(i), rename.io.in(i), flush)
    DecoupledConnect(rename.io.out(i), dispatch.io.in(i), flush)
  }

  // Rename Table
  for ((r, i) <- rat.io.readPorts.zipWithIndex) {
    r(0).addr := expander.io.out(i).bits.lsrcExpd(0)
    r(1).addr := expander.io.out(i).bits.lsrcExpd(1)
    r(2).addr := expander.io.out(i).bits.ldestExpd
    r(3).addr := 0.U
    rename.io.ratReadPorts(i) := r.map(_.data)
    r.foreach(_.hold := !rename.io.in(i).ready)
  }
  rat.io.renamePorts := rename.io.ratWritePorts
  rat.io.robCommits := rob.io.commits
  rat.io.flush := flush

  // Rename
  rename.io.flush := flush
  rename.io.robCommits := rob.io.commits

  dispatch.io.flush := flush

  io.out <> dispatch.io.out
  busyTable.io.allocPregs := rename.io.allocPregs
  //read busyTable : 4 + 4 + 4 + ... + 4
  for (i <- 0 until VRenameWidth * 4) {
    busyTable.io.read(i).req := dispatch.io.readBusyTable(i / 4)(i % 4)
    io.readBusyTable(i / 4)(i % 4) := busyTable.io.read(i).resp
  }
  busyTable.io.wbPregs(0).valid := io.wbArith_laneAlu.valid && io.wbArith_laneAlu.bits.uop.pdestVal
  busyTable.io.wbPregs(0).bits := io.wbArith_laneAlu.bits.uop.pdest
  busyTable.io.wbPregs(1).valid := io.wbArith_laneMulFp.valid && io.wbArith_laneMulFp.bits.uop.pdestVal
  busyTable.io.wbPregs(1).bits := io.wbArith_laneMulFp.bits.uop.pdest
  busyTable.io.wbPregs(2).valid := io.wbArith_cross.valid && io.wbArith_cross.bits.uop.pdestVal
  busyTable.io.wbPregs(2).bits := io.wbArith_cross.bits.uop.pdest
  busyTable.io.wbPregs(nVRFWritePorts-1).valid := io.wbLSU.valid && io.wbLSU.bits.pdestVal
  busyTable.io.wbPregs(nVRFWritePorts-1).bits := io.wbLSU.bits.pdest
  busyTable.io.flush := flush

  // Illegal instrn module
  val vIllegalInstrn = Module(new VIllegalInstrn)
  vIllegalInstrn.io.validIn := decoder.io.out.valid
  vIllegalInstrn.io.ctrl := decoder.io.out.bits.ctrl
  vIllegalInstrn.io.csr := decoder.io.out.bits.csr
  vIllegalInstrn.io.infoAll := infoCalc.io.infoAll
  vIllegalInstrn.io.extraInfo_for_VIllegal := infoCalc.io.extraInfo_for_VIllegal
  vIllegalInstrn.io.robPtrIn := vq.io.enqPtrOut
  vq.io.illegal := vIllegalInstrn.io.ill
  vq.io.partialVInfo_wire := partialVInfo_wire
  vq.io.partialVInfo_reg := partialVInfo_reg

  // ROB
  rob.io.in.valid := decoder.io.out.valid
  rob.io.in.sb_id := decoder.io.out.bits.sb_id
  rob.io.in.ldestVal := decoder.io.out.bits.ctrl.ldestVal
  rob.io.in.rdVal := decoder.io.out.bits.ctrl.rdVal
  rob.io.ovi_dispatch := io.ovi_dispatch
  rob.io.illegal := vIllegalInstrn.io.ill
  rob.io.partialVInfo := partialVInfo_reg
  rob.io.fromDispatch <> dispatch.io.toRob
  rob.io.wbArith_laneAlu := io.wbArith_laneAlu
  rob.io.wbArith_laneMulFp := io.wbArith_laneMulFp
  rob.io.wbArith_cross := io.wbArith_cross
  rob.io.wbLSU := io.wbLSU
  io.ovi_completed := rob.io.ovi_completed


  /**
    *  Debug
    */
  io.commits := rob.io.commits
  io.rvfi_sb_id := rob.io.rvfi_sb_id
  io.commitEnd := rob.io.commitEnd
}