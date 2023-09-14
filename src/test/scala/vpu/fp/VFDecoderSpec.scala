package vpu.fp

import chiseltest._
import chisel3._
import darecreek.exu.fp.{VFDecoder, VFInputGen}
import org.scalatest.flatspec.AnyFlatSpec
import darecreek._ //to do change later
trait DecoderBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>
  def decode_test():Unit = {
    it should "decode" in {
      test(new VFDecoder).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.clock.step(1)
        dut.io.instr.poke("b1011110000000000010100000".U)  // VFNMSAC_VF
        dut.clock.step(1)
//        dut.io.fpCtrl.expandRs1.expect(true)
//        dut.io.fpCtrl.narrowDst.expect(true)
        dut.io.fpCtrl.fmaCmd.expect(3)
        dut.io.fpCtrl.negVs1.expect(true)
        dut.io.fpCtrl.isMisc.expect(false)
        dut.io.fpCtrl.isCvt.expect(false)
        dut.clock.step(1)

        dut.io.instr.poke("b0010100000000000010100000".U)  // VFSGNJX_VF
        dut.clock.step(1)
//        dut.io.fpCtrl.expandRs1.expect(true)
        dut.io.fpCtrl.isMisc.expect(true)
        dut.io.fpCtrl.miscCmd.expect(1)
        dut.io.fpCtrl.miscSubCmd.expect(2)
        dut.io.fpCtrl.isCvt.expect(false)
        dut.io.fpCtrl.fmaCmd.expect(0)
        dut.clock.step(1)

        dut.io.instr.poke("b0100100000000111000100000".U)  // VFWCVT_RTZ_XU_F_V
        dut.clock.step(1)
//        dut.io.fpCtrl.wideningDst.expect(true)
        dut.io.fpCtrl.isCvt.expect(true)
        dut.io.fpCtrl.cvtCmd.expect(2)
        dut.io.fpCtrl.cvtRm.expect(2)
        dut.io.fpCtrl.isMisc.expect(false)
        dut.io.fpCtrl.fmaCmd.expect(0)
        dut.clock.step(1)

        dut.io.instr.poke("b0110010000000000000100000".U)  // VMFLE_VV
        dut.clock.step(1)
        dut.io.fpCtrl.isMisc.expect(true)
        dut.io.fpCtrl.miscCmd.expect(3)
        dut.io.fpCtrl.miscSubCmd.expect(1)
        dut.io.fpCtrl.isCvt.expect(false)
        dut.io.fpCtrl.fmaCmd.expect(0)
        //VFFMA
        //dut.io.instr.poke("b10010010000000000001000001010111".U) //vfmul_vv_VF
        //dut.io.instr.poke("b00001010000000000001000001010111".U) //vfsub.vv_VF
        //dut.io.instr.poke("b10011110000000000001000001010111".U) //vfrsub.vf_F
        //dut.io.instr.poke("b10110010000000000001000001010111".U) //vfmacc.vv_VF
        //dut.io.instr.poke("b10100010000000000001000001010111".U) //vfmadd.vv_VF
        //dut.io.instr.poke("b10100110000000000001000001010111".U) //vfnmadd.vv_VF
        //dut.io.instr.poke("b11000010000000000001000001010111".U) //vfwadd.vv_VF
        //dut.io.instr.poke("b11010010000000000001000001010111".U) //vfwadd.wv_VF
        //dut.io.instr.poke("b11110010000000000001000001010111".U) //vfwmacc.vv_VF
        //VFMisc
        //dut.io.instr.poke("b00100010000000000001000001010111".U)//vfsgnj.vv
        //dut.io.instr.poke("b00100110000000000001000001010111".U)//vfsgnjn.vv
        //dut.io.instr.poke("b00101010000000000001000001010111".U)//vfsgnjx.vv
        //VFCVT      ---------VS1
        //dut.io.instr.poke("b00000110000000000101000001010111".U) //vfcvt.xu.f.v
        //dut.io.instr.poke("b00001110000000000101000001010111".U) //vfcvt.x.f.v
        //dut.io.instr.poke("b00110110000000000101000001010111".U) //vfcvt.rtz.xu.f.v
        //dut.io.instr.poke("b00111110000000000101000001010111".U) //vfcvt.rtz.x.f.v 
        //dut.io.instr.poke("b01000110000000000101000001010111".U) //vfwcvt.xu.f.v
        //dut.io.instr.poke("b01001110000000000101000001010111".U) //vfwcvt.x.f.v
        //dut.io.instr.poke("b01110110000000000101000001010111".U) //vfwcvt.rtz.xu.f.v
        //dut.io.instr.poke("b01111110000000000101000001010111".U) //vfwcvt.rtz.x.f.v
        //dut.io.instr.poke("b10000110000000000101000001010111".U) //vfncvt.xu.f.w
        //dut.io.instr.poke("b10001110000000000101000001010111".U) //vfncvt.x.f.w
        //dut.io.instr.poke("b10110110000000000101000001010111".U) //vfncvt.rtz.xu.f.w
        //dut.io.instr.poke("b10111110000000000101000001010111".U) //vfncvt.rtz.x.f.w




      }
    }
  }
  def inputGen_test():Unit = {
    it should "expand rs1" in {
      val ctrl = VCtrlBundle(vsew = 2, funct6 = 10, funct3 = 5) // VFSGNJX_VF
      val srcs = SrcBundle(vs1 = "h1000000020000000", rs1 = "h1000000120000002")
      test(new VFInputGen).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        dut.io.in.poke(genInput(srcs, ctrl))
        dut.clock.step(1)
        dut.io.out.vs1.expect("h2000000220000002".U)
        dut.io.out.uop.vfpCtrl.isMisc.expect(true)
        dut.io.out.uop.vfpCtrl.miscCmd.expect(1)
        dut.io.out.uop.vfpCtrl.miscSubCmd.expect(2)
        dut.io.out.uop.vfpCtrl.isCvt.expect(false)
        dut.io.out.uop.vfpCtrl.fmaCmd.expect(0)
      }
    }
  }
}

class VFDecoderSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with DecoderBehavior {
  behavior of "VFInputGen"
  it should behave like decode_test
  it should behave like inputGen_test
}
