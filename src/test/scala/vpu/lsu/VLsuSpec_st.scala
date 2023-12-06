package darecreek.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.lsu._
import darecreek.ctrl.decode.VInstructions._
import darecreek._

trait VLsuBehavior_st {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val stReqSrc_default = SrcBundleSt()
  val vse8 = CtrlBundle(VSE8_V)
  
  def vLsuTest0(): Unit = {
    it should "pass: unit-stride store" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val stReqs = Seq(
          (vse8.copy(vl=32, uopIdx=0, uopEnd=false), stReqSrc_default),
          (vse8.copy(vl=32, uopIdx=1, uopEnd=true), stReqSrc_default),
        )

        next_is_store_and_step(dut)
        for ((c, s) <- stReqs) {
          while (!dut.io.fromIQ.st.ready.peekBoolean()) {
            dut.clock.step(1)
          }
          dut.io.fromIQ.st.valid.poke(true.B)
          dut.io.fromIQ.st.bits.poke(genStInput(c, s))
          dut.clock.step(1)
        }
        dut.io.fromIQ.st.valid.poke(false.B)
        dut.clock.step(4)

        fork {
          dut.clock.step(8)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.st.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.st.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else if (wb_cnt == 1) {
                dut.io.wb.st.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }
            }
            dut.clock.step(1)
          }
        }.fork {
          dut.clock.step(4)
          for (i <- stReqs) {
            dut.io.ovi_store.credit.poke(true.B)
            dut.clock.step(1)
          }
          dut.io.ovi_store.credit.poke(false.B)
        }.join()

        dut.clock.step(4)
      }
    }
  }

  
}

class VLsuSpec_st extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_st {
  behavior of "LSU test"
  it should behave like vLsuTest0()  // unit-stride store
}