package smartVector

import chisel3._
import chisel3.util._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import smartVector._
import SmartParam._

object test_init {
  def apply(dut: SmartVectorTestWrapper): Unit = {
    dut.clock.setTimeout(0)
    dut.io.rvuIssue.valid.poke(false.B)
  }
}
object next_is_load_and_step {
  def apply(dut: SmartVectorTestWrapper): Unit = {
    dut.io.rvuIssue.valid.poke(false.B)
    dut.clock.step(1)
  }
}

case class CtrlBundle(instrn: String = "h0",
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vsew: Int = 0,
                      vlmul: Int = 1,
                      vl: Int = 16,
                      vstart: Int = 0,
                      vxrm: Int = 0,
                      frm: Int = 0,
)

case class SrcBundleLd(rs1: String = "h1000",
                       rs2: String = "h0",
)              

trait BundleGenHelper {
  def genCtrl(c: CtrlBundle) = {
    (new CtrlTest).Lit(
        _.inst -> c.instrn.U,
        _.info_ma -> c.ma.B,
        _.info_ta -> c.ta.B,
        _.info_vsew -> c.vsew.U,
        _.info_vlmul -> c.vlmul.U,
        _.info_vl -> c.vl.U,
        _.info_vstart -> c.vstart.U,
        _.info_vxrm -> c.vxrm.U,
        _.info_frm -> c.frm.U,
    )
  }

  def genLdInput(c: CtrlBundle, s: SrcBundleLd) = {
    (new VIssueTest).Lit(
        _.ctrl -> genCtrl(c),
        _.rs1 -> s.rs1.U,
        _.rs2 -> s.rs2.U,
    )
  }
}

class CtrlTest extends Bundle {
    val inst        = UInt(32.W)
    val info_ma     = Bool() // vector mask agnostic, data unknown or undisturbed
    val info_ta     = Bool() // vector tail agnostic, data unknown or undisturbed
    val info_vsew   = UInt(3.W)
    val info_vlmul  = UInt(3.W)
    val info_vl     = UInt(bVL.W)
    val info_vstart = UInt(bVstart.W)
    val info_vxrm   = UInt(2.W)
    val info_frm    = UInt(3.W)
}

class VIssueTest extends Bundle {
    val ctrl = new CtrlTest
    val rs1 = UInt(XLEN.W)
    val rs2 = UInt(XLEN.W)
}

class FakeLdDCache extends Module {
    val io = IO(Flipped(new RVUMemory))

    val dataTable = Seq(
        // addr, data, exception
        (0x0fd0.U, BigInt("4040404040404404", 16).U, false.B),
        (0x0fd8.U, BigInt("3030303030303030", 16).U, false.B),
        (0x0fe0.U, BigInt("2020202020202020", 16).U, false.B),
        (0x0fe8.U, BigInt("1010101010101010", 16).U, false.B),
        (0x0ff0.U, BigInt("5678901234503489", 16).U, false.B),
        (0x0ff8.U, BigInt("eeeeeeeeeeeeeeee", 16).U, false.B),
        (0x1000.U, BigInt("0123456789abcdef", 16).U, false.B),
        (0x1008.U, BigInt("ffffffffffffffff", 16).U, false.B),
        (0x1010.U, BigInt("0f0f0f0f0f0f0f0f", 16).U, false.B),
        (0x1018.U, BigInt("fedcba9876543210", 16).U, false.B),
        (0x1020.U, BigInt("1234567890123456", 16).U, false.B),
        (0x1028.U, BigInt("0101010101010101", 16).U, false.B),
        (0x1030.U, BigInt("2345678901234567", 16).U, false.B),
        (0x1038.U, BigInt("1111111111111111", 16).U, false.B),
        (0x1040.U, BigInt("2222222222222222", 16).U, false.B),
        (0x1048.U, BigInt("3333333333333333", 16).U, false.B),
        (0x1050.U, BigInt("4444444444444444", 16).U, false.B),
        (0x1058.U, BigInt("5555555555555555", 16).U, false.B),
        (0x1060.U, BigInt("6666666666666666", 16).U, true.B),
    )
    
    io.req.ready            := true.B
    io.resp.bits.data       := 0.U
    io.resp.bits.nack       := false.B
    io.xcpt                 := 0.U.asTypeOf(new HellaCacheExceptions())

    val hasXcpt = WireInit(false.B)
    val hasMiss = WireInit(false.B)

    val s1_valid = RegNext(io.req.valid)
    val s1_req   = RegNext(io.req.bits)
    val s2_valid = RegNext(s1_valid)
    val s2_req   = RegNext(s1_req)

    when(hasXcpt || hasMiss) {
        s1_valid := false.B
        s2_valid := false.B
    }

    io.resp.valid           := s2_valid
    io.resp.bits.has_data   := true.B
    io.resp.bits.mask       := 0.U
    io.resp.bits.idx        := s2_req.idx

    val noise = RegInit("b011000001".U(32.W))
    noise := noise >> 1.U
    val random = noise(0)

    when(s2_valid) {
        when(random) {
            io.resp.bits.nack := true.B
            hasMiss := true.B
            io.resp.bits.has_data := false.B
        }.otherwise {
            io.resp.bits.nack := false.B
            hasMiss := false.B
            io.resp.bits.has_data := true.B
        }
        for(i <- 0 until dataTable.length) {
            when(dataTable(i)._1 === s2_req.addr) {
                io.resp.bits.data := dataTable(i)._2

                when(dataTable(i)._3) {
                    io.xcpt.ma.ld := true.B
                    hasXcpt := true.B
                }.otherwise {
                    io.xcpt.ma.ld := false.B
                    hasXcpt := false.B
                }
            }
        }
    }
}