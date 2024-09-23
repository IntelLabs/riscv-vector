/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import chisel3._
import chisel3.util._

abstract class RegisterSlice[T<: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val flush = Input(Bool())
  })
}

class ForwardRegistered[T<: Data](gen: T) extends RegisterSlice[T](gen) {

  val valid = RegInit(false.B)
  when (io.out.ready) { valid := false.B }
  when (io.in.valid) { valid := true.B }
  when (io.flush) { valid := false.B }

  val payload = RegEnable(io.in.bits, io.in.fire)

  io.out.valid := valid
  io.out.bits := payload
  io.in.ready := io.out.ready | !valid
}

class BackwardRegistered[T<: Data](gen: T) extends RegisterSlice[T](gen) {

  val ready = RegInit(true.B)
  val payload_buf = RegInit(0.U.asTypeOf(gen))
  val is_buf_full = RegInit(false.B)

  switch (is_buf_full) {
    is (false.B) {
      when (io.in.valid & !io.out.ready) {
        is_buf_full := true.B
        payload_buf := io.in.bits
        ready := false.B
      }
    }
    is (true.B) {
      when (io.out.ready) {
        is_buf_full := false.B
        ready := true.B
      }
    }
  }

  when (io.flush) {
    ready := true.B
    is_buf_full := false.B
  }

  io.out.valid := is_buf_full | io.in.valid
  io.out.bits := Mux(is_buf_full, payload_buf, io.in.bits)
  io.in.ready := ready
}

class FullyRegistered[T<: Data](gen: T) extends RegisterSlice[T](gen) {

  val in_ready = RegInit(true.B)
  val out_valid = RegInit(false.B)

  val payload_buf, out_payload = RegInit(0.U.asTypeOf(gen))

  val s_empty :: s_busy :: s_full :: Nil = Enum(3)
  val state = RegInit(s_empty)

  switch(state) {
    is (s_empty) {
      when (io.in.fire) {
        out_valid := true.B
        out_payload := io.in.bits
        state := s_busy
      }
    }
    is (s_busy) {
      when (io.in.fire && io.out.fire) {
        out_payload := io.in.bits
      }
      when (!io.in.fire && io.out.fire) {
        out_valid := false.B
        state := s_empty
      }
      when (io.in.fire && !io.out.fire) {
        in_ready := false.B
        payload_buf := io.in.bits
        state := s_full
      }
    }
    is (s_full) {
      when (io.out.fire) {
        in_ready := true.B
        out_payload := payload_buf
        state := s_busy
      }
    }
  }

  when (io.flush) {
    in_ready := true.B
    out_valid := false.B
    state := s_empty
  }

  io.out.valid := out_valid
  io.out.bits := out_payload
  io.in.ready := in_ready
}


object RegisterSlice {
  def fromString[T <: Data](s: String, gen: T): RegisterSlice[T] = s.toLowerCase match {
    case "forward"  => Module(new ForwardRegistered(gen))
    case "backward" => Module(new BackwardRegistered(gen))
    case "fully"    => Module(new FullyRegistered(gen))
    case t          => throw new IllegalArgumentException(s"unknown RegisterSlice type $t")
  }

  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],flush: Bool, mode: String): RegisterSlice[T] = {
    val slice = fromString(mode, left.bits)
    slice.io.in <> left
    slice.io.out <> right
    slice.io.flush := flush
    slice
  }
}