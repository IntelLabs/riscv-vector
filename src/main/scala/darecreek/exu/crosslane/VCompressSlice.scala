package darecreek

import chisel3._
import chisel3.util._

class VCompressSlice extends Module {
  val io = IO(new Bundle {
      val fire = Input(Bool())
      val vs1  = Input(Vec(NByteLane, UInt(1.W)))
      val vs2  = Input(UInt(LaneWidth.W))
      val slice_data  = Output(Vec(NByteLane, UInt(8.W)))
      val slice_byte_cnt  = Output(UInt(4.W))
  })

val byte_cnt  = Wire(Vec((NByteLane+1), UInt(4.W)))

byte_cnt(0) := 0.U(4.W)
for (i<-1 until (NByteLane+1)) {
  when (io.fire && (io.vs1(i-1)===1.U(1.W))) { 
    byte_cnt(i) := byte_cnt(i-1) +1.U(4.W)
  } .otherwise {
    byte_cnt(i) := byte_cnt(i-1)
  }
}

for (i<-0 until NByteLane) {
  io.slice_data(i) := 0.U(8.W) 
  when (io.fire && (io.vs1(i)===1.U(1.W))) { 
    io.slice_data(byte_cnt(i+1)-1.U) := io.vs2((i+1)*8-1, i*8) 
  }
}

io.slice_byte_cnt := byte_cnt(NByteLane) 
}



