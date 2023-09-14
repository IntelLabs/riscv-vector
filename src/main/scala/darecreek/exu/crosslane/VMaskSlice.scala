package darecreek

import chisel3._
import chisel3.util._

class VMaskSlice extends Module {
  val io = IO(new Bundle {
      val ihasone = Input(Bool())
      val vs1     = Input(UInt(LaneWidth.W))
      val vs2     = Input(Vec(LaneWidth, UInt(1.W)))
      val first   = Output(SInt(xLen.W))
      val sbf     = Output(UInt(LaneWidth.W))
      val ohasone = Output(Bool())
      val vmand   = Output(UInt(LaneWidth.W)) 
      val vmandn  = Output(UInt(LaneWidth.W)) 
      val vmxor   = Output(UInt(LaneWidth.W)) 
      val vmor    = Output(UInt(LaneWidth.W)) 
      val vmorn   = Output(UInt(LaneWidth.W)) 
  })

val flag = Wire(Vec(LaneWidth, UInt(1.W)))
val sbf = Wire(Vec(LaneWidth, UInt(1.W)))
val index = Wire(Vec(LaneWidth, SInt(xLen.W)))
val vs2 = Cat(io.vs2.reverse)  

for (i<-0 until LaneWidth) {
  if(i == 0) {
    flag(i) := io.vs2(i)
    sbf(i) := Mux(io.vs2(0).asBool, 0.U, 1.U)
  } else {
    flag(i) := io.vs2(i) | flag(i-1)
    sbf(i) := Mux(flag(i).asBool, 0.U, 1.U)
  }
}

for (i<-0 until LaneWidth) {
  if(i == (LaneWidth-1)) 
    index(i) := Mux(io.vs2(i).asBool, i.S, -1.S)
  else
    index(i) := Mux(io.vs2(i).asBool, i.S, index(i+1))
}
 
io.first := index(0)
io.sbf := Mux(io.ihasone, 0.U, Cat(sbf.reverse))
io.ohasone := io.sbf.orR | io.ihasone
io.vmand   := vs2 & io.vs1 
io.vmandn  := vs2 & ~io.vs1  
io.vmxor   := vs2 ^ io.vs1 
io.vmor    := vs2 | io.vs1 
io.vmorn   := vs2 | ~io.vs1 
}



