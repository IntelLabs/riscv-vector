package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.fp._
import chipsalliance.rocketchip.config._
import xiangshan._
import xiangshan.backend.rob.RobPtr


class VAluWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VAluOutput)
  })
  val vAlu = Module(new VAlu)
  vAlu.io.in.bits := io.in.bits
  io.out.bits := vAlu.io.out.bits

  vAlu.io.in.valid := io.in.valid
  io.out.valid := vAlu.io.out.valid
  io.in.ready := io.out.ready
}

/** @note VMac has the same IO ports as VAlu */
class VMacWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})
  
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VAluOutput)
  })

  val vMac = Module(new VMac)
  vMac.io.in.bits := io.in.bits
  io.out.bits := vMac.io.out.bits

  vMac.io.in.valid := io.in.valid
  io.out.valid := vMac.io.out.valid
  io.in.ready := io.out.ready
}

class VDivWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFpuInput))
    val out = Decoupled(new VFpuOutput)
  })
  val vdiv = Module(new VDiv)
  vdiv.io.in.valid := io.in.valid
  vdiv.io.in.bits := io.in.bits.in
  io.in.ready := vdiv.io.in.ready
  vdiv.io.redirect := io.in.bits.redirect

  vdiv.io.out <> io.out
}

class VFPUExternalWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})

  val io = IO(new Bundle {
    val dontCare = Input(Bool())
    val in = Flipped(DecoupledIO(new VFuInput))
    val redirect = Input(ValidIO(new Redirect))
    val out = DecoupledIO(new VFpuOutput)
  })
  val vfpu = Module(new VFPUWrapper)
  vfpu.io.in.valid := io.in.valid
  when(io.dontCare) {
    vfpu.io.in.bits := DontCare

    /*vuopInfo.ma := DontCare
    vuopInfo.ta := DontCare
    vuopInfo.vsew := DontCare
    vuopInfo.vlmul := DontCare
    vuopInfo.vl := DontCare
    vuopInfo.vstart := DontCare
    vuopInfo.vxrm := DontCare
    vuopInfo.frm := DontCare
      
    vuop.uopIdx := DontCare
    vuop.uopEnd := DontCare

    vfpu.io.in.uop.ctrl.funct6 := DontCare
    vfpu.io.in.uop.ctrl.funct3 := DontCare
    vfpu.io.in.uop.ctrl.vm := DontCare
    vfpu.io.in.uop.ctrl.vs1_imm := DontCare
    vfpu.io.in.uop.ctrl.widen := DontCare
    vfpu.io.in.uop.ctrl.widen2 := DontCare
    vfpu.io.in.uop.ctrl.narrow := DontCare
    vfpu.io.in.uop.ctrl.narrow_to_1 := DontCare

    vfpu.io.in.vs1 := DontCare
    vfpu.io.in.vs2 := DontCare
    vfpu.io.in.oldVd := DontCare
    vfpu.io.in.mask := DontCare
    vfpu.io.in.rs1 := DontCare*/
  }.otherwise {
    vfpu.io.in.bits := io.in.bits
  }
  io.in.ready := vfpu.io.in.ready
  vfpu.io.redirect := io.redirect

  vfpu.io.out <> io.out
}


/*class VMaskWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VAluOutput)
  })

  val vMask = Module(new VMask)
  vMask.io.in.bits := io.in.bits
  io.out.bits := vMask.io.out.bits

  vMask.io.in.valid := io.in.valid
  io.out.valid := vMask.io.out.valid
  io.in.ready := io.out.ready
}*/

/*class VPermWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VPermInput))
    val out = Decoupled(new VIFuOutput)
  })

  val vPerm = Module(new Permutation)
  vPerm.io.in.bits := io.in.bits
  io.out.bits.vd := vPerm.io.out.vd
  io.out.bits.vxsat := vPerm.io.out.vxsat


  vPerm.io.in.valid := io.in.valid
  io.out.valid := RegNext(io.in.valid)
  io.in.ready := io.out.ready
}*/