package darecreek

import chisel3._
import chisel3.util._
import utils._

class subVRFReadPort(regLen: Int) extends Bundle {
  val addrOH = Input(Vec(NVPhyRegs, Bool()))
  val data = Output(UInt(regLen.W))
}
class subVRFWritePort(regLen: Int) extends Bundle {
  val wen = Input(Bool())
  val addrOH = Input(Vec(NVPhyRegs, Bool()))
  val data = Input(UInt(regLen.W))
}

// A portion of vector RF (only 64-bit)
class subVRegFile(numRead: Int, numWrite: Int, regLen: Int) extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, new subVRFReadPort(regLen))
    val write = Vec(numWrite, new subVRFWritePort(regLen))
  })

  val rf = Reg(Vec(NVPhyRegs, UInt(regLen.W)))
  for (rd <- io.read) {
    rd.data := ParallelMux(rd.addrOH zip rf)
  }
  for (i <- 0 until NVPhyRegs) {
    val wrHit = io.write.map(x => x.wen && x.addrOH(i))
    when (wrHit.reduce(_ || _)) {
      rf(i) := Mux1H(wrHit, io.write.map(_.data))
    }
  }
}

class VRFReadPort(regLen: Int) extends Bundle {
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Output(Vec(NLanes, UInt(regLen.W)))
}
class VRFWritePort(regLen: Int) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Input(Vec(NLanes, UInt(regLen.W)))
}

class VRegFile(numRead: Int, numWrite: Int) extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, new VRFReadPort(LaneWidth))
    val write = Vec(numWrite, new VRFWritePort(LaneWidth))
  })
  
  val subRFs = Seq.fill(NLanes)(Module(new subVRegFile(numRead, numWrite, LaneWidth)))
  val allAddrs = VecInit.tabulate(NVPhyRegs)(_.U(VPRegIdxWidth.W))
  val rdOneHot = Seq.tabulate(numRead)(i => allAddrs.map(_ === io.read(i).addr))
  val wrOneHot = Seq.tabulate(numWrite)(i => allAddrs.map(_ === io.write(i).addr))
  for (laneIdx <- 0 until NLanes) {
    for (i <- 0 until numRead) {
      subRFs(laneIdx).io.read(i).addrOH := rdOneHot(i)
      io.read(i).data(laneIdx) := subRFs(laneIdx).io.read(i).data
    }
    for (i <- 0 until numWrite) {
      subRFs(laneIdx).io.write(i).wen := io.write(i).wen
      subRFs(laneIdx).io.write(i).addrOH := wrOneHot(i)
      subRFs(laneIdx).io.write(i).data := io.write(i).data(laneIdx)
    }
  }
}