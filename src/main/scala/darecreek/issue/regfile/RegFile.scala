package darecreek

import chisel3._
import chisel3.util._

class subVRFReadPort(regLen: Int) extends Bundle {
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Output(UInt(regLen.W))
}
class subVRFWritePort(regLen: Int) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Input(UInt(regLen.W))

}

// A portion of vector RF (only 64-bit)
class subVRegFile(numRead: Int, numWrite: Int, regLen: Int) extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, new subVRFReadPort(regLen))
    val write = Vec(numWrite, new subVRFWritePort(regLen))
  })

  val rf = Reg(Vec(NVPhyRegs, UInt(regLen.W)))
  for (r <- io.read) {
    r.data := rf(r.addr)
  }
  for (w <- io.write) {
    when (w.wen) {
      rf(w.addr) := w.data
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
  for (laneIdx <- 0 until NLanes) {
    for (i <- 0 until numRead) {
      subRFs(laneIdx).io.read(i).addr := io.read(i).addr
      io.read(i).data(laneIdx) := subRFs(laneIdx).io.read(i).data
    }
    for (i <- 0 until numWrite) {
      subRFs(laneIdx).io.write(i).wen := io.write(i).wen
      subRFs(laneIdx).io.write(i).addr := io.write(i).addr
      subRFs(laneIdx).io.write(i).data := io.write(i).data(laneIdx)
    }
  }
}