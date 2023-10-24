package smartVector
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}

import SmartParam._


class VIexOutput(implicit p: Parameters) extends Bundle{
  val toReg = ValidIO(new regIn)
}


class regIn(implicit p: Parameters) extends Bundle {
  val rfReadEn = Vec(2,Bool())
  val rfWriteEn = Bool()
  val rfReadIdx = Vec(2,UInt(5.W))
  val rfWriteIdx = UInt(5.W)
  val rfWriteData = UInt(VLEN.W)
  val vxsat = Bool()
}

class regOut extends Bundle{
  val writeDone = Bool()
  val readVld   = Vec(2,Bool())
  val readData  = Vec(2,UInt(VLEN.W))
}


