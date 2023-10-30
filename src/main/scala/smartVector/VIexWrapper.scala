package smartVector

//import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
//import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chipsalliance.rocketchip.config
import xiangshan.backend.rob.RobPtr


class VIexWrapper(implicit p : Parameters) extends Module {
  
  val io = IO(new Bundle {
    val in = Input(ValidIO(new UopQueueOutput))
    val out = ValidIO(new VAluOutput)
  })


  //val p = Parameters.empty.alterPartial({
  //case SmartParamsKey => SmartParameters(VLEN = 128)})


  val SValu = Module(new VAluWrapper()(p))

  SValu.io.in.valid := io.in.valid
  SValu.io.in.bits.vfuInput.uop := io.in.bits.uop
  
  SValu.io.in.bits.vfuInput.vs1   := io.in.bits.uopRegInfo.vs1
  SValu.io.in.bits.vfuInput.vs2   := io.in.bits.uopRegInfo.vs2
  SValu.io.in.bits.vfuInput.rs1   := io.in.bits.scalar_opnd_1
  SValu.io.in.bits.vfuInput.oldVd := 0.U
  SValu.io.in.bits.vfuInput.mask  := 0.U

  io.out.bits  := SValu.io.out.bits
  io.out.valid := RegNext(io.in.valid)
  //io.out.bits.toReg.bits.rfReadEn(0) := false.B
  //io.out.bits.toReg.bits.rfReadEn(1) := false.B
  //io.out.bits.toReg.bits.rfReadIdx   := DontCare
  
  //io.out.bits.toReg.bits.rfWriteEn   := RegEnable(io.in.bits.uopRegInfo.rfWriteEn, io.in.valid)
  //io.out.bits.toReg.bits.rfWriteIdx  := RegEnable(io.in.bits.uopAttribute.ldest, io.in.valid)
  //io.out.bits.toReg.bits.rfWriteData := SValu.io.out.bits.vd
  //io.out.bits.toReg.bits.vxsat       := SValu.io.out.bits.vxsat

  //io.out.bits.toReg.valid := io.out.bits.toReg.bits.rfWriteEn
}




