package vfu.alu

import chisel3._
import chisel3.util._
import yunsuan.vector.{VIFuInfo, VIFuInput, VIFuOutput}
import yunsuan.vector.alu.{VIAlu}

class VIAluWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VIFuInput))
    val out = Decoupled(new VIFuOutput)
  })

  val vIAlu = Module(new VIAlu)
  vIAlu.io.in.bits := io.in.bits
  io.out.bits := vIAlu.io.out.bits

  vIAlu.io.in.valid := io.in.valid
  io.out.valid := vIAlu.io.out.valid
  io.in.ready := io.out.ready
}