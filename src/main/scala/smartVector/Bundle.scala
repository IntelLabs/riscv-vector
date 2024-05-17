package smartVector
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}

import SmartParam._


class regReadIn(implicit p: Parameters) extends Bundle {
  val rfReadEn = Vec(3,Bool()) //0: vs1   1: vs2   2: old_vd/vs3
  //val rfWriteEn = Bool()
  val rfReadIdx = Vec(3,UInt(5.W))
  //val rfWriteIdx = UInt(5.W)
  //val rfWriteData = UInt(VLEN.W)
  //val vxsat = Bool()
}

class regWriteIn(implicit p: Parameters) extends Bundle {
  //val rfReadEn = Vec(2,Bool())
  val rfWriteEn = Vec(2,Bool())
  val rfWriteMask = Vec(2,UInt((VLEN/8).W))
  //val rfReadIdx = Vec(2,UInt(5.W))
  val rfWriteIdx = Vec(2,UInt(5.W))
  val rfWriteData = Vec(2,UInt(VLEN.W))
  //val vxsat = Bool()
}

class regOut extends Bundle{
  val writeDone = Bool()
  val readVld   = Vec(4,Bool())
  val readData  = Vec(4,UInt(VLEN.W))
}




