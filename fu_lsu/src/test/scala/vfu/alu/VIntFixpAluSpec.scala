package vfu.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import yunsuan.vector._
import yunsuan.vector.alu._
import vfu._
import chiseltest.WriteVcdAnnotation
import vfu.dataType._
import yunsuan.vector.alu.VAluOpcode._

trait VAluBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val vadd = CtrlBundle(opcode = 0)
  val vsub = CtrlBundle(opcode = 1)
  val vand = CtrlBundle(opcode = 7)
  val vor = CtrlBundle(opcode = 11)
  val vxor = CtrlBundle(opcode = 10)
  val sll = CtrlBundle(opcode = 15)
  val srl = CtrlBundle(opcode = 16)
  val sra = CtrlBundle(opcode = 17)

  def vIntFixpTest(): Unit = {
    it should "pass the integer test" in {
      test(new VIAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // Add/sub
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), vadd.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), vadd.copy(s16, s16, s16)),
          genVAluInput(SrcBundle("h000000000000000f0000000200000000", "hfffffff8000000030000000100000000"), vadd.copy(s32, s32, s32)),
          genVAluInput(SrcBundle("h00000000000000020000000000000000", "h00000000000000010000000000000000"), vadd.copy(s64, s64, s64)),
          genVAluInput(SrcBundle("h00ffff01ff0001077f0008f8000f0200", "hffff01ff00ff7f7f007ff800f8030100"), vsub.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h7fff00000008fff80000000f00020000", "h00007ffffff80000fff8000300010000"), vsub.copy(s16, s16, s16)),
          genVAluInput(SrcBundle("h000000000000000f0000000200000000", "hfffffff8000000030000000100000000"), vsub.copy(s32, s32, s32)),
          genVAluInput(SrcBundle("h00000000000000020000000000000000", "h00000000000000010000000000000000"), vsub.copy(s64, s64, s64)),
          // and/or/xor
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), vand.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), vor.copy(s16, s16, s16)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), vxor.copy(s16, s16, s16)),
          // sll/srl/sra
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), srl.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), srl.copy(s16, s16, s16)),
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), sra.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), sra.copy(s16, s16, s16)),
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), sll.copy(s8, s8, s8)),
          genVAluInput(SrcBundle("h000107ff0000fff80000000f00020000", "h7fff7fff7fffff80fff8000300010000"), sll.copy(s16, s16, s16)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // Add/sub
          genVAluOutput("h7f00f8120300feff80867f00f8120300"),
          genVAluOutput("h800087fe7fffff78fff8001200030000"),
          genVAluOutput("hfffffff8000000120000000300000000"),
          genVAluOutput("h00000000000000030000000000000000"),
          genVAluOutput("h0100fe02ff0182887f8110f8080c0100"),
          genVAluOutput("h7fff80010010fff80008000c00010000"),
          genVAluOutput("h000000080000000c0000000100000000"),
          genVAluOutput("h00000000000000010000000000000000"),
          // and/or/xor
          genVAluOutput("h000800030000ff000107000800030000"),
          genVAluOutput("h7fff7fff7ffffff8fff8000f00030000"),
          genVAluOutput("h7ffe78007fff0078fff8000c00030000"),
          // sll/srl/sra
          genVAluOutput("h00080001010001000000000800010100"),
          genVAluOutput("h000000000000fff80000000100010000"),
          genVAluOutput("h000800010100ff000000000800010100"),
          genVAluOutput("h000000000000fff80000000100010000"),
          genVAluOutput("h00080078040080008080000800780400"),
          genVAluOutput("h800080000000fff80000007800040000"),

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

  def vMaskTest(): Unit = {
    it should "pass the mask test" in {
      test(new VIAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // Fake !!
          genVAluInput(SrcBundle("h0008000f0200ff0001070008000f0200", "h7ff8f8030100ffff7f7f7ff8f8030100"), vadd.copy(s8, s8, s8)),
          // Fake !!
          genVAluInput(SrcBundle(vs2 = "h0008000f0200ff0001070008000f0200", vs1 = "h7ff8f8030100ffff7f7f7ff8f8030100", old_vd = "h1", mask = "h2"), 
                       CtrlBundle(4, 4, 4, 30))
                       // vadd.copy(vdType = 4, s8, s8)),  val vadd = CtrlBundle(opcode = 0)

        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // Fake !!
          genVAluOutput("h7f00f8120300feff80867f00f8120300"),
          genVAluOutput("h800087fe7fffff78fff8001200030000"),

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

class VAluSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VAluBehavior {
  behavior of "Int fixP test"
  it should behave like vIntFixpTest()
  // behavior of "Mask/reduction/permutation test"
  // it should behave like vMaskTest()
}