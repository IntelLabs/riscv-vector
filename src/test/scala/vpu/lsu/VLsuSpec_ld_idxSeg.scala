package darecreek.lsutest

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.lsu._
import darecreek.ctrl.decode.VInstructions._
import darecreek._

//---- indexed/segment load ----
trait VLsuBehavior_ld_idxSeg {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val ldReqSrc_default = SrcBundleLd()
  val vluxei8 = CtrlBundle(VLUXEI8_V)
  val vluxei16 = CtrlBundle(VLUXEI16_V)
  val vluxei32 = CtrlBundle(VLUXEI32_V)
  val vluxei64 = CtrlBundle(VLUXEI64_V)

  
  def vLsuTest0(): Unit = {
    it should "pass: indexed load" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val vSew = 0
        val ldReqs = Seq(
          (vluxei8.copy(destEew=vSew, vsew=vSew, vl=4, uopIdx=0, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=5, el_id=0), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=6, el_id=1), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=7, el_id=2), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=8, el_id=3), ("h0", false)),
        )
        next_is_load_and_step(dut)
        fork {
          for ((c, s) <- ldReqs) {
            while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
              dut.clock.step(1)
            }
            dut.io.fromIQ.ld.valid.poke(true.B)
            dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(false.B)
          dut.clock.step(4)
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffffa1".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 1) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffffa2".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 2) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffffa3".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 3) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffffa4".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_ffffffff6789abcd".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffff_1211_100f0e0d0c0b0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest1(): Unit = {
    it should "pass: indexed load" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val vSew = 1
        val ldReqs = Seq(
          (vluxei32.copy(destEew=vSew, vsew=vSew, vl=4, uopIdx=0, uopEnd=true), ldReqSrc_default),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=5, el_id=0), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=6, el_id=1), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=7, el_id=2), ("h0", false)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=8, el_id=3), ("h0", false)),
        )
        next_is_load_and_step(dut)
        fork {
          for ((c, s) <- ldReqs) {
            while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
              dut.clock.step(1)
            }
            dut.io.fromIQ.ld.valid.poke(true.B)
            dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(false.B)
          dut.clock.step(4)
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_a4a3a2a1".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 1) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_a8a7a6a5".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 2) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_acabaaa9".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 3) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_b0afaead".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffffffff_ffffffffffffffff_cdef012345670123".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffff_1211_100f0e0d0c0b0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest2(): Unit = {
    it should "pass: indexed load" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val vSew = 1
        val ldReqs = Seq(
          (vluxei64.copy(destEew=vSew, vsew=vSew, vl=6, uopIdx=0, uopEnd=true, vm=false, vstart=2), 
                         ldReqSrc_default.copy(mask="h125")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=5, el_id=2), ("h1", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=6, el_id=3), ("h0", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=7, el_id=4), ("h0", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=8, el_id=5), ("h1", true)),
        )
        next_is_load_and_step(dut)
        fork {
          for ((c, s) <- ldReqs) {
            while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
              dut.clock.step(1)
            }
            dut.io.fromIQ.ld.valid.poke(true.B)
            dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(false.B)
          dut.clock.step(4)
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h1_b8b7b6b5b4b3b2b1".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 1) {
                dut.io.ovi_maskIdx.item.expect("h0_c0bfbebdbcbbbab9".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 2) {
                dut.io.ovi_maskIdx.item.expect("h0_a8a7a6a5a4a3a2a1".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 3) {
                dut.io.ovi_maskIdx.item.expect("h1_b0afaeadacabaaa9".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("hffffffff_ffffffffffffffff_ffffffffffffffff_cdef0a0908070123_04030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } /*else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_ffffffffffff_1211_100f0e0d0c0b0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }*/
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }

  def vLsuTest3(): Unit = {
    it should "pass: indexed load" in {
      test(new VLsuTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        test_init(dut)
        dut.clock.step(1)
        val vSew = 3
        val ldReqs = Seq(
          (vluxei16.copy(destEew=vSew, vsew=vSew, vl=7, uopIdx=0, uopEnd=false, vm=false, vstart=2), 
                         ldReqSrc_default.copy(mask="h165")),
          (vluxei16.copy(destEew=vSew, vsew=vSew, vl=7, uopIdx=1, uopEnd=true, vm=false, vstart=2), 
                         ldReqSrc_default.copy(mask="h0")),
        )
        val ldResps = Seq(
          ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=5, el_id=2), ("h1", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=6, el_id=3), ("h0", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=7, el_id=4), ("h0", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=0, el_id=5), ("h1", true)),
           ("h0004000300020001_0008000700060005_000c000b000a0009_0001000f000e000d" + 
            "0123456701234567_89abcdef89abcdef_0123456701234567_89abcdef89abcdef",
           SeqId(el_count=1, el_off=1, el_id=6), ("h1", true)),
        )
        next_is_load_and_step(dut)
        fork {
          for ((c, s) <- ldReqs) {
            while (!dut.io.fromIQ.ld.ready.peekBoolean()) {
              dut.clock.step(1)
            }
            dut.io.fromIQ.ld.valid.poke(true.B)
            dut.io.fromIQ.ld.bits.poke(genLdInput(c, s))
            dut.clock.step(1)
          }
          dut.io.fromIQ.ld.valid.poke(false.B)
          dut.clock.step(4)
        }.fork {
          var maskIdx_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.ovi_maskIdx.valid.peekBoolean()) {
              if (maskIdx_cnt == 0) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffa6a5".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 1) {
                dut.io.ovi_maskIdx.item.expect("h0_ffffffff_ffffa8a7".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 2) {
                dut.io.ovi_maskIdx.item.expect("h0_ffffffff_ffffaaa9".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 3) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffacab".U)
                maskIdx_cnt += 1
              } else if (maskIdx_cnt == 4) {
                dut.io.ovi_maskIdx.item.expect("h1_ffffffff_ffffaead".U)
                maskIdx_cnt += 1
              }
              dut.io.ovi_maskIdx.credit.poke(true.B)
            } else {
              dut.io.ovi_maskIdx.credit.poke(false.B)
            }
            dut.clock.step(1)
          }
        }.join()

        fork {
          for ((ldData, seqId, mask)  <- ldResps) {
            one_512b_load_resp(dut, ldData, seqId.asUInt, mask)
            dut.clock.step(1)
          }
          dut.io.ovi_load.valid.poke(false.B)
          dut.io.ovi_memop.sync_end.poke(true.B)
          dut.clock.step(1)
          dut.io.ovi_memop.sync_end.poke(false.B)
        }.fork {
          var wb_cnt = 0
          for (i <- 0 until 10) {
            if (dut.io.wb.ld.valid.peekBoolean()) {
              if (wb_cnt == 0) {
                dut.io.wb.ld.bits.vd.expect("h201f1e1d1c1b1a19_000c000b000a0009_100f0e0d0c0b0a09_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(0.U)
                wb_cnt += 1
              } else {
                dut.io.wb.ld.bits.vd.expect("hffffffffffffffff_0123456701234567_89abcdef89abcdef_0807060504030201".U)
                dut.io.wb.ld.bits.uop.expdIdx.expect(1.U)
                wb_cnt += 1
              }
            }
            dut.clock.step(1)
          }
        }.join()

        dut.clock.step(4)
      }
    }
  }


}

class VLsuSpec_ld_idxSeg extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VLsuBehavior_ld_idxSeg {
  behavior of "LSU test"
  // it should behave like vLsuTest0()
  // it should behave like vLsuTest1()
  // it should behave like vLsuTest2()
  it should behave like vLsuTest3()
}