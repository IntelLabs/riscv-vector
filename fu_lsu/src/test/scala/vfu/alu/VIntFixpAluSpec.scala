/***************************************************************************************
*Copyright (c) 2023-2024 Intel Corporation
*Vector Acceleration IP core for RISC-V* is licensed under Mulan PSL v2.
*You can use this software according to the terms and conditions of the Mulan PSL v2.
*You may obtain a copy of Mulan PSL v2 at:
*        http://license.coscl.org.cn/MulanPSL2
*THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
*EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
*MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*See the Mulan PSL v2 for more details.
***************************************************************************************/

package vfutest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import vfu._
import vfu.alu._
import vfutest._
import chiseltest.WriteVcdAnnotation
import vfutest.dataType._
import vfu.alu.VAluOpcode._

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

  val vredsum   = CtrlBundle(opcode = 33) 
  val vredmax   = CtrlBundle(opcode = 34) 
  val vredmin   = CtrlBundle(opcode = 35) 
  val vredand   = CtrlBundle(opcode = 36) 
  val vredor    = CtrlBundle(opcode = 37) 
  val vredxor   = CtrlBundle(opcode = 38) 
  val vcpop     = CtrlBundle(mask, mask, mask, opcode = 39, vl = 16) 
  val vfirst    = CtrlBundle(mask, mask, mask, opcode = 40, vl = 16) 
  val vmsbf     = CtrlBundle(mask, mask, mask, opcode = 41, vm = false, vl = 16) 
  val vmsif     = CtrlBundle(mask, mask, mask, opcode = 42, vm = false, vl = 16) 
  val vmsof     = CtrlBundle(mask, mask, mask, opcode = 43, vm = false, vl = 16) 
  val viota     = CtrlBundle(opcode = 44) 
  val vid       = CtrlBundle(opcode = 45) 
  val vslideup  = CtrlBundle(opcode = 46) 
  val vslidedn  = CtrlBundle(opcode = 47) 
  val vslide1up = CtrlBundle(opcode = 48) 
  val vslide1dn = CtrlBundle(opcode = 49) 
  val vrgather  = CtrlBundle(opcode = 50) 
  val vcompress = CtrlBundle(opcode = 51) 
 
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
        //   // vcpop
        //   genVAluInput(SrcBundle("ha8f5c55c2cfd89003038efd4f88dd8de", "h0",  "h0", "h0"), vcpop.copy()),
        //   // vfirst
        //   genVAluInput(SrcBundle("ha8f5c55c2cfd89003038efd4f88dd8de", "h0",  "h0", "h0"), vfirst.copy()),
          // vmsbf
          genVAluInput(SrcBundle("ha8f5c55c2cfd89003038efd4f88dd8de", "h0",  "h6da3c06ee39c40c57d911a582ed947c7", "hb9415b8cec802448e68db7e6ec545628"), vmsbf.copy()),
          // vmsof
          genVAluInput(SrcBundle("h3c9b240e0ec54401c9fe38ad28979917", "h0",  "h36c90632b926876605f1f0759e38eec3", "hb9415b8cec802448e68db7e6ec545628"), vmsof.copy()),
          // vmsif
          genVAluInput(SrcBundle("hb9415b8cec802448e68db7e6ec545628", "h0",  "h42753db3ca32c7000fe5bd1b936d3b0d", "h0"), vmsif.copy(vm = true)),
          // // vid 
          // genVAluInput(SrcBundle("h0a09090807070706050403020101ef95", "h0",  "h0", "h0a09090807070706050403020101ef95"), vmsif.copy(u8, mask,mask, vm = false, vl=16)),
          // vcomprss
          genVAluInput(SrcBundle("h0008060f0206ff0001070008060f0206", "h7ff8f8030105ffff7f7f7ff8f8030105",  "h8344c0aa5b76129530ac75dc32808aa4", "h0"), vcompress.copy(u8, u8, u8, vm = true, ta = false, vl=16)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
        //   // vcpop
        //   genVAluOutput("ha"),
        //   // vfirst
        //   genVAluOutput("h1"),
          // vmsbf
          genVAluOutput("hffffffffffffffffffffffffffff01c7"),
          // vmsof
          genVAluOutput("hffffffffffffffffffffffffffffb8c3"),
          // vmsif
          genVAluOutput("hffffffffffffffffffffffffffff000f"),
          // // vid
          // genVAluOutput("h0f0e0d000b0a09080700000400020000"),
          // vompress
          genVAluOutput("h8344c0aa5b76129530ac75dc32000f06"),


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
  behavior of "Mask/reduction/permutation test"
  it should behave like vMaskTest()
}
