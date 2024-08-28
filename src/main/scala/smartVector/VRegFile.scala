package smartVector

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import SmartParam._
import chipsalliance.rocketchip.config.Parameters
import matrix.MatrixParameters.hasMatrix

class subVRFReadPort(regLen: Int) extends Bundle {
  val ren  = Input(Bool())
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Output(UInt(regLen.W))
  // override def cloneType: subVRFReadPort.this.type =
    // new subVRFReadPort(regLen).asInstanceOf[this.type]
}

class subVRFWritePort(regLen: Int) extends Bundle {
  val wen   = Input(Bool())
  val wmask = Input(UInt((regLen/8).W))
  val addr  = Input(UInt(VPRegIdxWidth.W))
  val data  = Input(UInt(regLen.W))
  // override def cloneType: subVRFWritePort.this.type =
  //   new subVRFWritePort(regLen).asInstanceOf[this.type]

}

// A portion of vector RF (only 64-bit)
class subVRegFile(numRead: Int, numWrite: Int, regLen: Int) extends Module {
  val io = IO(new Bundle {
    val read   = Vec(numRead, new subVRFReadPort(regLen))
    val write  = Vec(numWrite, new subVRFWritePort(regLen))
    //TODO: This is reserved for verification, delete it later
    val rfData = Output(Vec(NVPhyRegs, UInt((VLEN/NLanes).W)))
  })

  val rf = Reg(Vec(NVPhyRegs, Vec(regLen/8, UInt(8.W))))
  for (r <- io.read) { 
      r.data := rf(r.addr).asUInt
  }
  for (w <- io.write) {
    when (w.wen) {
      for (i <- 0 until regLen/8) {
        when (!w.wmask(i)) {
          rf(w.addr)(i) := w.data(i*8+7, i*8)
        }
      }
    }
  }

  //TODO: This is reserved for verification, delete it later
  for(i <- 0 until NVPhyRegs){
    io.rfData(i) := rf(i).asUInt
  }
}

class VRFReadPort(regLen: Int) extends Bundle {
  val ren  = Input(Bool())
  val addr = Input(UInt(VPRegIdxWidth.W))
  val data = Output(Vec(NLanes, UInt(regLen.W)))
  // override def cloneType: VRFReadPort.this.type =
  //   new VRFReadPort(regLen).asInstanceOf[this.type]
}
class VRFWritePort(regLen: Int) extends Bundle {
  val wen   = Input(Bool())
  val wmask = Input(Vec(NLanes, UInt((regLen/8).W)))
  val addr  = Input(UInt(VPRegIdxWidth.W))
  val data  = Input(Vec(NLanes, UInt(regLen.W)))
  // override def cloneType: VRFWritePort.this.type =
  //   new VRFWritePort(regLen).asInstanceOf[this.type]
}

class SVRegFile(implicit p : Parameters, numRead: Int, numWrite: Int) extends Module {
  val io = IO(new Bundle {
    val read  = Vec(numRead,  new VRFReadPort(LaneWidth))
    val write = Vec(numWrite, new VRFWritePort(LaneWidth))
    val mma_toRegFileWrite = if (hasMatrix) Some(Input(Vec(NVPhyRegs / 2, new regWriteIn))) else None
    //TODO: This is reserved for verification, delete it later
    val rfData = Output(Vec(NVPhyRegs, UInt(VLEN.W)))
  })

  val subRFs = Seq.fill(NLanes)(Module(new subVRegFile(numRead, numWrite, LaneWidth)))
  for (laneIdx <- 0 until NLanes) {
    for (i <- 0 until numRead) {
      subRFs(laneIdx).io.read(i).ren  := io.read(i).ren
      subRFs(laneIdx).io.read(i).addr := io.read(i).addr
      io.read(i).data(laneIdx)        := subRFs(laneIdx).io.read(i).data
    }
    for (i <- 0 until numWrite) {
      subRFs(laneIdx).io.write(i).wen   := io.write(i).wen
      subRFs(laneIdx).io.write(i).wmask := io.write(i).wmask(laneIdx)
      subRFs(laneIdx).io.write(i).addr  := io.write(i).addr
      subRFs(laneIdx).io.write(i).data  := io.write(i).data(laneIdx)
    }
  }

  //TODO: This is reserved for verification, delete it later
  for(i <- 0 until NVPhyRegs){
    io.rfData(i) := Cat(subRFs(1).io.rfData(i), subRFs(0).io.rfData(i))
  }

  for(i <- 0 until NVPhyRegs/2) {
    for (j <- 0 until NLanes) {
      subRFs(j).io.write(i).wen   := io.mma_toRegFileWrite.get(i).rfWriteEn
      subRFs(j).io.write(i).wmask := io.mma_toRegFileWrite.get(i).rfWriteMask(4*(j+1)-1,4*j)
      subRFs(j).io.write(i).addr  := (i+16).U
      subRFs(j).io.write(i).data  := io.mma_toRegFileWrite.get(i).rfWriteData(64*(j+1)-1,64*j)
    }
  }
    //io.rfData := Vec.tabulate(NVPhyRegs){i=>Cat(subRFs(1).io.rfData, subRFs(0).io.rfData)}
}