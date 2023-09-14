package darecreek.vfutest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config._
import xiangshan._

class VFpuInput(implicit p: Parameters) extends Bundle {
  // val in = new VFuInput
  val in = new VFuInput
  val redirect = ValidUndirectioned(new Redirect)
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

trait VDivBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val vdivu = CtrlBundle(VDIVU_VV)
  val vdivu_vx = CtrlBundle(VDIVU_VX)
  val vdiv = CtrlBundle(VDIV_VV)
  val vdiv_vx = CtrlBundle(VDIV_VX)
  val vremu = CtrlBundle(VREMU_VV)
  val vrem = CtrlBundle(VREM_VV)
  val vfdiv = CtrlBundle(VFDIV_VV)
  val vfrdiv = CtrlBundle(VFRDIV_VF)
  val vfsqrt = CtrlBundle(VFSQRT_V)

  def vDivTest0(): Unit = {
    it should "pass the integer div" in {
      test(new VDivWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessDiv.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // int div(u)
          genVFpuInput(SrcBundle("h0f0e0d0c0b0a09080706050403020100", "h05050505050505050505050505050505"),
                      vdivu.copy(vsew=0, vl=16)),
          genVFpuInput(SrcBundle("h0f0e0d0c0b0a09080706050403020100", rs1="h0000000000000005"),
                      vdivu_vx.copy(vsew=0, vl=16)),
          genVFpuInput(SrcBundle("h000f000e000d000c000b000a00090008", "h00050005000500050005000500050005"),
                      vdiv.copy(vsew=1, vl=8)),
          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", "h00050005000500050005000500050005"),
                      vdiv.copy(vsew=1, vl=8)),
          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", "hfffbfffbfffbfffbfffbfffbfffbfffb"),
                      vdiv.copy(vsew=1, vl=8)),
          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", rs1="hfffffffffffffffb"),
                      vdiv_vx.copy(vsew=1, vl=8)),

          // genVFpuInput(SrcBundle("hde0fc1f2180c9d58c7dafb59e271c0d1", "h2c698d306b35b9201634c698359adc99"),
          //             vdiv.copy(vsew=2, vl=16, uopIdx=0)),
          // genVFpuInput(SrcBundle("h0a794f2283425666f444888a4da779df", "h58d31a60d66b722e429e53c8a0d095a7"),
          //             vdiv.copy(vsew=2, vl=16, uopIdx=1)),
          // genVFpuInput(SrcBundle("h36e2dc52ee780f7420ae15bab8dd32ed", "h853ca79141a12b3c6f07e0f90c064eb5"),
          //             vdiv.copy(vsew=2, vl=16, uopIdx=2)),
          // genVFpuInput(SrcBundle("h634c698359adc8824d17a2eb2412ebfb", "hb1a634c1acd6e44a9b716e29773c07c3"),
          //             vdiv.copy(vsew=2, vl=16, uopIdx=3)),

          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", rs1="hfffffffffffffffb", old_vd="h12345678abcddcba87654321"),
                      vdiv_vx.copy(vsew=1, vl=8, vstart=6)),
          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", rs1="hfffffffffffffffb", old_vd="habcdabcdabcdabcd"),
                      vdiv_vx.copy(vsew=1, vl=8, vstart=9)),

          // int rem
          genVFpuInput(SrcBundle("h0f0e0d0c0b0a09080706050403020100", "h06060606060606060606060606060606"),
                      vremu.copy(vsew=0, vl=16)),
          genVFpuInput(SrcBundle("hfff0fff1fff2fff3fff4fff5fff6fff7", "hfffbfffbfffbfffbfffbfffbfffbfffb"),
                      vrem.copy(vsew=1, vl=8)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // int div
          genVFpuOutput(vdivu, "h03020202020201010101010000000000"), // vdivu
          genVFpuOutput(vdivu_vx, "h03020202020201010101010000000000"), // vdivu
          genVFpuOutput(vdiv.copy(vsew=1, vl=8), "h00030002000200020002000200010001"), // vdiv
          genVFpuOutput(vdiv.copy(vsew=1, vl=8), "hfffdfffdfffefffefffefffefffeffff"), // vdiv
          genVFpuOutput(vdiv.copy(vsew=1, vl=8), "h00030003000200020002000200020001"), // vdiv
          genVFpuOutput(vdiv_vx.copy(vsew=1, vl=8), "h00030003000200020002000200020001"), // vdiv
          
          // genVFpuOutput(vdiv.copy(vsew=2, vl=16, uopIdx=0), "h0000000000000000fffffffe00000000"), // vdiv
          // genVFpuOutput(vdiv.copy(vsew=2, vl=16, uopIdx=1), "h00000000000000030000000000000000"), // vdiv
          // genVFpuOutput(vdiv.copy(vsew=2, vl=16, uopIdx=2), "h000000000000000000000000fffffffb"), // vdiv
          // genVFpuOutput(vdiv.copy(vsew=2, vl=16, uopIdx=3), "hffffffffffffffff0000000000000000"), // vdiv
          
          genVFpuOutput(vdiv_vx.copy(vsew=1, vl=8, vstart=6), "h0003000312345678abcddcba87654321"), // vdiv
          genVFpuOutput(vdiv_vx.copy(vsew=1, vl=8, vstart=9), "habcdabcdabcdabcd"), // vdiv

          // int rem
          genVFpuOutput(vremu, "h03020100050403020100050403020100"), // vremu
          genVFpuOutput(vrem.copy(vsew=1, vl=8), "hffff0000fffcfffdfffeffff0000fffc"), // vrem
        )

        fork {
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(1)
      }
    }
  }

  def vDivTest1(): Unit = {
    it should "pass the FP div/sqrt" in {
      test(new VDivWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessDiv.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vfdiv
          genVFpuInput(SrcBundle("h3f8000003f8000003f8000003f800000", "h3f0000003f0000003f0000003f000000"),
                      vfdiv.copy(vsew=2, vl=4)),
          genVFpuInput(SrcBundle("h40e0000040400000400000003f800000", "h3f0000003f0000003f0000003f000000"),
                      vfdiv.copy(vsew=2, vl=4)),
          genVFpuInput(SrcBundle("hBf800000Bf800000Bf800000Bf800000", "hBf000000Bf0000003f0000003f000000"),
                      vfdiv.copy(vsew=2, vl=4)),
          genVFpuInput(SrcBundle("h5de1c6304819e2cd47acff98127f0646", "h3ffffffffffffffe000000397e80627c", old_vd="h00ff55aa75cd39e1f05a7a1e0fdca593"),
                      vfdiv.copy(vsew=2, vl=4, vstart=1, frm=1)),
          // vfrdiv
          genVFpuInput(SrcBundle("h41000000410000004000000040000000", rs1="h3f000000"),
                      vfrdiv.copy(vsew=2, vl=4)),
          // vfsqrt
          genVFpuInput(SrcBundle("h40000000400000004080000040800000"),
                      vfsqrt.copy(vsew=2, vl=4)),

        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vfdiv
          genVFpuOutput(vfdiv.copy(vsew=2, vl=4), "h40000000400000004000000040000000"), // vfdiv
          genVFpuOutput(vfdiv.copy(vsew=2, vl=4), "h4160000040C000004080000040000000"), // vfdiv
          genVFpuOutput(vfdiv.copy(vsew=2, vl=4), "h4000000040000000c0000000c0000000"), // vfdiv
          genVFpuOutput(vfdiv.copy(vsew=2, vl=4, vstart=1, frm=1), "h5d61c6307fc000007f7fffff0fdca593", 5), // vfdiv
          // vfrdiv
          genVFpuOutput(vfrdiv.copy(vsew=2, vl=4), "h3d8000003d8000003e8000003e800000"), // vfrdiv
          // vfsqrt
          genVFpuOutput(vfsqrt.copy(vsew=2, vl=4), "h3fb504f33fb504f34000000040000000", fflags=1), // vfsqrt
        )

        fork {
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(1)
      }
    }
  }

  

}

class VDivSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VDivBehavior {
  behavior of "Div test"
  it should behave like vDivTest0()
  it should behave like vDivTest1()
}