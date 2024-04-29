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
  * IssueBlock: issue queues + physical register file
  *   So far only one arith IQ is supported
  */

package darecreek

import chisel3._
import chisel3.util._
import utils._
import darecreek.lsu._
import darecreek.exu.crosslane.PermRdRF

class GetScalarOperand extends Bundle {
  val exu = new Bundle {
    val addr = Output(new VRobPtr)
    val data = Input(UInt(xLen.W))
  }
  val ls = new Bundle {
    val addr = Output(new VRobPtr)
    val data = Input(UInt(xLen.W))
  }
}

class VIssueBlock extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    // from Dispatch
    val in = new Bundle {
      val toArithIQ = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
      val toLsIQ = Vec(VRenameWidth, Flipped(Decoupled(new VExpdUOp)))
    }
    // from BusyTable
    val fromBusyTable = Vec(VRenameWidth, Vec(4, Input(Bool())))
    // Read rs1 from VQ
    val get_rs1 = new GetScalarOperand
    // EXU
    val toExu = new Bundle {
      val bits = Output(new VExuInput)
      val valid = Output(Bool())
      val readys = Input(Vec(NArithFUs, Bool()))
    }
    val fromExu = new Bundle {
      val laneAlu = Input(ValidIO(new VLaneExuOut))
      val laneMulFp = Input(ValidIO(new VLaneExuOut))
      val cross = Input(ValidIO(new VCrossExuOut))
    }
    // For permutation read register file
    val perm = Flipped(new PermRdRF)
    // LSU
    val toLSU = new Bundle {
      val ld = Decoupled(new VLdInput)
      val st = Decoupled(new VStInput)
    }
    val fromLSU = new Bundle {
      val ld = Flipped(ValidIO(new VLdOutput))
      val st = Flipped(ValidIO(new VStOutput))
    }
    // writeback: to BusyTable, to ROB
    val wbArith_laneAlu = ValidIO(new WbArith_lane)
    val wbArith_laneMulFp = ValidIO(new WbArith_lane)
    val wbArith_cross = ValidIO(new WbArith_cross)
    val wbLSU = ValidIO(new VExpdUOp)
    // Just for debug
    val rfRdRvfi = Vec(VCommitWidth, new VRFReadPort(LaneWidth))
  })

  val arithIQ = Module(new VArithIssueQ(ArithIQSize, VRenameWidth))
  val lsIQ = Module(new VLdstIssueQ(LsIQSize, VRenameWidth))
  // Arith: 4    Ld: 3 (idx + old_dst + mask)  st: 3 (idx + vs3 + mask)  permutation: 1
  // Todo: reduce num of read ports of ld/st
  val nVRFReadPorts = NArithIQs * 4 + 3 + 1 // So far load and store are serial, they need 3 read ports
  val regFile = Module(new VRegFile(nVRFReadPorts + nVRFReadPortsRvfi, nVRFWritePorts))

  // Write-back addresses: for wakeup (ready gen) of IQs
  val wbRFAddrs = Wire(Vec(nVRFWritePorts, ValidIO(UInt(VPRegIdxWidth.W))))
  wbRFAddrs(0).valid := io.fromExu.laneAlu.valid && io.fromExu.laneAlu.bits.uop.pdestVal
  wbRFAddrs(0).bits := io.fromExu.laneAlu.bits.uop.pdest
  wbRFAddrs(1).valid := io.fromExu.laneMulFp.valid && io.fromExu.laneMulFp.bits.uop.pdestVal
  wbRFAddrs(1).bits := io.fromExu.laneMulFp.bits.uop.pdest
  wbRFAddrs(2).valid := io.fromExu.cross.valid && io.fromExu.cross.bits.uop.pdestVal
  wbRFAddrs(2).bits := io.fromExu.cross.bits.uop.pdest
  wbRFAddrs(3).valid := io.fromLSU.ld.valid && io.fromLSU.ld.bits.uop.pdestVal
  wbRFAddrs(3).bits := io.fromLSU.ld.bits.uop.pdest

  /** ---- Arithmetic ----
    *  IQ --> Read RF --> EXU
    */
  val validPipe_arith = RegInit(false.B)
  // Inputs of Arith IQ
  arithIQ.io.flush := io.flush
  arithIQ.io.in <> io.in.toArithIQ
  arithIQ.io.fromBusyTable := io.fromBusyTable
  arithIQ.io.vRFWriteback := wbRFAddrs
  // Ready to issue to EXU
  val toExuReady = (io.toExu.readys zip io.toExu.bits.uop.ctrl.fuSel map {case (r, v) => r && v}
                   ).reduce(_ || _)
  arithIQ.io.out.ready := toExuReady || !validPipe_arith
  // A uop of arith IQ is successfully issued to read RF
  val fireArithIQout = arithIQ.io.out.fire
  // Read RF, and regEnable the read data
  for (srcIdx <- 0 until 4) {
    regFile.io.read(4*0 + srcIdx).addr := arithIQ.io.out.bits.psrc(srcIdx)
    io.toExu.bits.vSrc(srcIdx) := RegEnable(regFile.io.read(4*0 + srcIdx).data, fireArithIQout)
  }
  // RegEnable the uop
  io.toExu.bits.uop := RegEnable(arithIQ.io.out.bits, fireArithIQout)
  // RegEnable the rs1
  io.get_rs1.exu.addr := arithIQ.io.out.bits.vRobIdx
  io.toExu.bits.rs1 := RegEnable(io.get_rs1.exu.data, fireArithIQout)
  // Pipeline of valid
  when (io.flush) {
    validPipe_arith := false.B
  }.elsewhen (arithIQ.io.out.ready) {
    validPipe_arith := arithIQ.io.out.valid
  }
  io.toExu.valid := validPipe_arith
  // VL remain
  val destEewExu = SewOH(io.toExu.bits.uop.info.destEew)
  io.toExu.bits.vlRemain := calcRemain(destEewExu, fireArithIQout, 
                                  arithIQ.io.out.bits, arithIQ.io.out.bits.info.vl)
  // vstart remain
  // io.toExu.bits.vstartRemain := calcRemain(io.toExu.bits.uop.info.destEew, fireArithIQout, 
  //                                 arithIQ.io.out.bits.expdIdx, arithIQ.io.out.bits.info.vstart)
  io.toExu.bits.vstartRemain := 0.U
  
  // RF write
  regFile.io.write(0).wen := io.fromExu.laneAlu.valid && io.fromExu.laneAlu.bits.uop.pdestVal
  regFile.io.write(0).addr := io.fromExu.laneAlu.bits.uop.pdest
  regFile.io.write(0).data := io.fromExu.laneAlu.bits.vd
  regFile.io.write(1).wen := io.fromExu.laneMulFp.valid && io.fromExu.laneMulFp.bits.uop.pdestVal
  regFile.io.write(1).addr := io.fromExu.laneMulFp.bits.uop.pdest
  regFile.io.write(1).data := io.fromExu.laneMulFp.bits.vd
  regFile.io.write(2).wen := io.fromExu.cross.valid && io.fromExu.cross.bits.uop.pdestVal
  regFile.io.write(2).addr := io.fromExu.cross.bits.uop.pdest
  regFile.io.write(2).data := io.fromExu.cross.bits.vd
  // Write-backs: to BusyTable, to ROB 
  io.wbArith_laneAlu.valid := io.fromExu.laneAlu.valid
  io.wbArith_laneAlu.bits.uop := io.fromExu.laneAlu.bits.uop
  io.wbArith_laneAlu.bits.fflags := io.fromExu.laneAlu.bits.fflags
  io.wbArith_laneAlu.bits.vxsat := io.fromExu.laneAlu.bits.vxsat
  io.wbArith_laneAlu.bits.rd := io.fromExu.laneAlu.bits.vd(0)(xLen-1, 0)
  io.wbArith_laneMulFp.valid := io.fromExu.laneMulFp.valid
  io.wbArith_laneMulFp.bits.uop := io.fromExu.laneMulFp.bits.uop
  io.wbArith_laneMulFp.bits.fflags := io.fromExu.laneMulFp.bits.fflags
  io.wbArith_laneMulFp.bits.vxsat := io.fromExu.laneMulFp.bits.vxsat
  io.wbArith_laneMulFp.bits.rd := io.fromExu.laneMulFp.bits.vd(0)(xLen-1, 0)
  io.wbArith_cross.valid := io.fromExu.cross.valid
  io.wbArith_cross.bits.uop := io.fromExu.cross.bits.uop
  io.wbArith_cross.bits.fflags := io.fromExu.cross.bits.fflags
  io.wbArith_cross.bits.rd := io.fromExu.cross.bits.vd(0)(xLen-1, 0)

  /**
    * ---- Load/Store ----
    */
  // Inputs of ld/st IQs
  lsIQ.io.flush := io.flush
  lsIQ.io.in <> io.in.toLsIQ
  lsIQ.io.fromBusyTable := io.fromBusyTable
  lsIQ.io.vRFWriteback := wbRFAddrs
  
  // -- Load --
  // A uop of load/store IQ is successfully issued
  val fireLsIQout = lsIQ.io.out.fire
  // Info for LSU
  // Read RF, and regEnable the read data
  regFile.io.read(NArithIQs * 4).addr := lsIQ.io.out.bits.psrc(1)
  regFile.io.read(NArithIQs * 4 + 1).addr := lsIQ.io.out.bits.psrc(2)
  regFile.io.read(NArithIQs * 4 + 2).addr := lsIQ.io.out.bits.psrc(3)
  io.toLSU.ld.bits.vs2 := RegEnable(Cat(regFile.io.read(NArithIQs * 4).data.reverse), fireLsIQout)
  io.toLSU.ld.bits.oldVd := RegEnable(Cat(regFile.io.read(NArithIQs * 4 + 1).data.reverse), fireLsIQout)
  io.toLSU.ld.bits.vmask := RegEnable(Cat(regFile.io.read(NArithIQs * 4 + 2).data.reverse), fireLsIQout)
  // RegEnable the uop
  io.toLSU.ld.bits.uop := RegEnable(lsIQ.io.out.bits, fireLsIQout)
  // RegEnable the rs1
  io.get_rs1.ls.addr := lsIQ.io.out.bits.vRobIdx
  io.toLSU.ld.bits.rs2 := RegEnable(io.get_rs1.ls.data, fireLsIQout)
  // So far, the ready of io.toLSU is ld.ready && st.ready
  // Todo: support overlapping of load and store excution
  val io_toLSU_ready = io.toLSU.ld.ready && io.toLSU.ld.bits.uop.ctrl.load ||
                       io.toLSU.st.ready && io.toLSU.ld.bits.uop.ctrl.store
  // Pipeline of valid
  val validPipe_ls = RegInit(false.B)
  when (io.flush) {
    validPipe_ls := false.B
  }.elsewhen (lsIQ.io.out.ready) {
    validPipe_ls := lsIQ.io.out.valid
  }
  io.toLSU.ld.valid := validPipe_ls && io.toLSU.ld.bits.uop.ctrl.load
  // Ready
  lsIQ.io.out.ready := io_toLSU_ready || !validPipe_ls
  // RF write of Ld
  regFile.io.write(nVRFWritePorts-1).wen := io.fromLSU.ld.valid
  regFile.io.write(nVRFWritePorts-1).addr := io.fromLSU.ld.bits.uop.pdest
  for (i <- 0 until NLanes) {regFile.io.write(nVRFWritePorts-1).data(i) := UIntSplit(io.fromLSU.ld.bits.vd)(i)}

  // -- Store --
  // Read RF, and regEnable the read data
  io.toLSU.st.bits.vs2 := io.toLSU.ld.bits.vs2 
  io.toLSU.st.bits.vs3 := io.toLSU.ld.bits.oldVd 
  io.toLSU.st.bits.vmask := io.toLSU.ld.bits.vmask 
  // RegEnable the uop
  io.toLSU.st.bits.uop := io.toLSU.ld.bits.uop
  // RegEnable the rs1
  io.toLSU.st.bits.rs2 := io.toLSU.ld.bits.rs2

  io.toLSU.st.valid := validPipe_ls && io.toLSU.ld.bits.uop.ctrl.store
  // Write-back: to BusyTable, to ROB 
  io.wbLSU.valid := io.fromLSU.ld.valid || io.fromLSU.st.valid
  io.wbLSU.bits := Mux(io.fromLSU.ld.valid, io.fromLSU.ld.bits.uop, io.fromLSU.st.bits.uop)

  /**
    * One read port for permutation unit
    */
  regFile.io.read(nVRFReadPorts - 1).addr := io.perm.rd_preg_idx
  io.perm.rdata := RegEnable(Cat(regFile.io.read(nVRFReadPorts - 1).data.reverse), io.perm.rd_en)
  io.perm.rvalid := RegNext(io.perm.rd_en)

  def calcRemain(destEew: SewOH, fireIn: Bool, uop: VExpdUOp, vl: UInt) = {
    // Notice: max VL must <= 10000...0   max vstart <= 01111...1
    val remain = Reg(UInt(bVL.W)) 
    val nElemVLEN = Wire(UInt(bVL.W))
    // nElemVLEN := vlenb.U >> destEew
    nElemVLEN := Mux1H(Seq(
      destEew.is8  -> vlenb.U,
      destEew.is16 -> (vlenb.U >> 1),
      destEew.is32 -> (vlenb.U >> 2),
      destEew.is64 -> (vlenb.U >> 3),
    ))
    val sub = Wire(UInt(bVL.W))
    sub := remain - Mux(uop.ctrl.narrow && uop.expdIdx(0), 0.U, nElemVLEN)
    when (fireIn) {
      remain := Mux(uop.expdIdx === 0.U, vl, Mux(sub(bVL-1), 0.U, sub))
    }
    remain
  }

  /**
    * Debug print
    */
  // when (arithIQ.io.out.fire) {
  //   printf(p"----Get one arith IQ out \n") 
  // }
  // when (lsIQ(0).io.out.fire) {
  //   printf(p"----Get one load IQ out \n") 
  // }
  // when (lsIQ(1).io.out.fire) {
  //   printf(p"----Get one store IQ out \n") 
  // }

  /**
    * Debug
    */
  io.rfRdRvfi(0) <> regFile.io.read(nVRFReadPorts)
  io.rfRdRvfi(1) <> regFile.io.read(nVRFReadPorts + 1)

}