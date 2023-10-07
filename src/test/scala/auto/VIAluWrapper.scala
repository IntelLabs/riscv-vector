package darecreek.vfuAutotest.alu

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
import org.chipsalliance.cde.config._
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