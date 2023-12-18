package darecreek.vfutest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import smartVector._

class FakeDCache extends Module {
    val io = IO(Flipped(new RVUMemory))
    io.req.ready  := true.B
    io.resp.valid := false.B
    io.resp.bits  := DontCare
    io.xcpt       := 0.U.asTypeOf(new HellaCacheExceptions())
}

class RVUTestResult extends Bundle {
    val commit_vld   = Output(Bool())
    val alu_data     = Output(UInt(128.W))
    //val reg_data     = Output(UInt(128.W))
}
class SmartVectorWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})

  val io = IO(new Bundle {
    val in =  Flipped(Decoupled(new RVUissue))
    val out = Output(new Bundle{
            val rvuCommit = new RVUCommit
            val rvuExtra  = new RVUExtra
        })
    val rfData = Output(Vec(32, UInt(128.W)))
  })
  val smartVector = Module(new SmartVector)
  val dcache      = Module(new FakeDCache)

  smartVector.io.in <> io.in
  //smartVector.io.in.valid := io.in.valid
  smartVector.io.rvuMemory <> dcache.io
  io.out := smartVector.io.out
  io.rfData := smartVector.io.rfData
}

trait SmartBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with SBundleGenHelper =>

  val vadd = CtrlBundle(VADD_VV)
  val vsub = CtrlBundle(VSUB_VV)
  val vand = CtrlBundle(VAND_VV)
  val vor = CtrlBundle(VOR_VV)
  val vxor = CtrlBundle(VXOR_VV)
  val vsll = CtrlBundle(VSLL_VV)
  val vsrl = CtrlBundle(VSRL_VV)
  val vsra = CtrlBundle(VSRA_VV)
  val vmin = CtrlBundle(VMIN_VV)
  val vminu = CtrlBundle(VMINU_VV)
  val vmax = CtrlBundle(VMAX_VV)
  val vmaxu = CtrlBundle(VMAXU_VV)
  val vwadd = CtrlBundle(VWADD_VV)
  val vwaddu = CtrlBundle(VWADDU_VV)
  val vwsub = CtrlBundle(VWSUB_VV)
  val vwsubu = CtrlBundle(VWSUBU_VV)
  val vwadd_wv = CtrlBundle(VWADD_WV)
  val vwaddu_wv = CtrlBundle(VWADDU_WV)
  val vwsub_wv = CtrlBundle(VWSUB_WV)
  val vwsubu_wv = CtrlBundle(VWSUBU_WV)
  val vzext_vf2 = CtrlBundle(VZEXT_VF2)
  val vsext_vf2 = CtrlBundle(VSEXT_VF2)
  val vzext_vf4 = CtrlBundle(VZEXT_VF4)
  val vsext_vf4 = CtrlBundle(VSEXT_VF4)
  val vzext_vf8 = CtrlBundle(VZEXT_VF8)
  val vsext_vf8 = CtrlBundle(VSEXT_VF8)
  val vmerge = CtrlBundle(VMERGE_VVM)
  val vmv = CtrlBundle(VMV_V_V)
  val vadc = CtrlBundle(VADC_VVM)
  val vsbc = CtrlBundle(VSBC_VVM)
  val vnsrl = CtrlBundle(VNSRL_WV)
  val vnsra = CtrlBundle(VNSRA_WV)
  val vmseq = CtrlBundle(VMSEQ_VV)
  val vmsne = CtrlBundle(VMSNE_VV)
  val vmslt = CtrlBundle(VMSLT_VV)
  val vmsltu = CtrlBundle(VMSLTU_VV)
  val vmsle = CtrlBundle(VMSLE_VV)
  val vmsleu = CtrlBundle(VMSLEU_VV)
  val vmsgt_vx = CtrlBundle(VMSGT_VX)
  val vmsgtu_vx = CtrlBundle(VMSGTU_VX)
  val vmadc_vvm = CtrlBundle(VMADC_VVM)
  val vmadc_vxm = CtrlBundle(VMADC_VXM)
  val vmsbc_vvm = CtrlBundle(VMSBC_VVM)
  val vmadc_vv = CtrlBundle(VMADC_VV)
  val vmsbc_vv = CtrlBundle(VMSBC_VV)
  val vsaddu = CtrlBundle(VSADDU_VV)
  val vsadd = CtrlBundle(VSADD_VV)
  val vssubu = CtrlBundle(VSSUBU_VV)
  val vssub = CtrlBundle(VSSUB_VV)
  val vaaddu = CtrlBundle(VAADDU_VV)
  val vaadd = CtrlBundle(VAADD_VV)
  val vasubu = CtrlBundle(VASUBU_VV)
  val vasub = CtrlBundle(VASUB_VV)
  val vssrl = CtrlBundle(VSSRL_VV)
  val vssra = CtrlBundle(VSSRA_VV)
  val vnclipu = CtrlBundle(VNCLIPU_WV)
  val vnclip = CtrlBundle(VNCLIP_WV)
  val vmvxs = CtrlBundle(VMV_X_S)
  val vmvsx = CtrlBundle(VMV_S_X)
  val vfmvfs = CtrlBundle(VFMV_F_S)
  val vfmvsf = CtrlBundle(VFMV_S_F)
  val vmvnrr = CtrlBundle(VMV1R_V)

 
  def vAluTest0(): Unit = {
    it should "pass the test: add/sub, and/or/xor, sll/srl/sra" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // Add/sub
          genVFuInput(SrcBundle("ha3d70a38a3d70a3891a2b3c091a2b3c0", "h7f6e5d487f6e5d486d3a06d06d3a06d0", "h5b05b0585b05b05848d159e048d159e0"), vadd.copy(vsew=0, vl=0, vlmul=5)), //vadd.copy(s8, s8, s8, vl=0, vlmul=5)),
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vadd), //vadd.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("hb4e81ac9b4e81ab8a2b3c451a2b3c440", "h907f6dd9907f6dc87e4b17617e4b1750"), vadd.copy(vsew=1, vl=8)), //vadd.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("hfb72e9cbfb72e9b8e93e9353e93e9340", "hd70a3cdbd70a3cc8c4d5e663c4d5e650"), vadd.copy(vsew=2, vl=4)), //vadd.copy(s32, s32, s32, vl=4)),
          genVFuInput(SrcBundle("h41fdb8ce41fdb8b82fc962562fc96240", "h1d950bde1d950bc80b60b5660b60b550"), vadd.copy(vsew=3, vl=2)), //vadd.copy(s64, s64, s64, vl=2)),
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vsub), //vsub.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("hb4e81ac9b4e81ab8a2b3c451a2b3c440", "h907f6dd9907f6dc87e4b17617e4b1750"), vsub.copy(vsew=1, vl=8)), //vsub.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("hfb72e9cbfb72e9b8e93e9353e93e9340", "hd70a3cdbd70a3cc8c4d5e663c4d5e650"), vsub.copy(vsew=2, vl=4)), //vsub.copy(s32, s32, s32, vl=4)),
          genVFuInput(SrcBundle("h41fdb8ce41fdb8b82fc962562fc96240", "h1d950bde1d950bc80b60b5660b60b550"), vsub.copy(vsew=3, vl=2)), //vsub.copy(s64, s64, s64, vl=2)),
          // and/or/xor
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vand), //vand.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("hb4e81ac9b4e81ab8a2b3c451a2b3c440", "h907f6dd9907f6dc87e4b17617e4b1750"), vor.copy(vsew=1, vl=8)), //vor.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("hfb72e9cbfb72e9b8e93e9353e93e9340", "hd70a3cdbd70a3cc8c4d5e663c4d5e650"), vxor.copy(vsew=2, vl=4)), //vxor.copy(s32, s32, s32, vl=4)),
          // sll/srl/sra
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vsll), //vsll.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("h8d159d8c8d159d787ae147147ae14700", "h68acf09c68acf08856789a2456789a10"), vsll.copy(vsew=2, vl=4)), //vsll.copy(s32, s32, s32, vl=4)),
          genVFuInput(SrcBundle("hb4e81ac9b4e81ab8a2b3c451a2b3c440", "h907f6dd9907f6dc87e4b17617e4b1750"), vsrl.copy(vsew=1, vl=8)), //vsrl.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("hd3a06c8ed3a06c78c16c1616c16c1600", "haf37bf9eaf37bf889d0369269d036910"), vsrl.copy(vsew=3, vl=2)), //vsrl.copy(s64, s64, s64, vl=2)),
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vsra), //vsra.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("h468ace8a468ace783456781234567800", "h2222219a222221880fedcb220fedcb10"), vsra.copy(vsew=1, vl=8)), //vsra.copy(s16, s16, s16, vl=8)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // Add/sub
          genVAluOutput("h5b05b0585b05b05848d159e048d159e0"), //vadd
          genVAluOutput("hb751e99eb751e98093e83dae93e83d90"), //vadd 350
          genVAluOutput("h456788a24567888020fedbb220fedb90"), //vadd 402
          genVAluOutput("hd27d26a6d27d2680ae1479b6ae147990"), //vadd 454
          genVAluOutput("h5f92c4ac5f92c4803b2a17bc3b2a1790"), //vadd 506
          genVAluOutput("h2569adf02569adf02568adf02568adf0"), //vsub 350
          genVAluOutput("h2469acf02469acf02468acf02468acf0"), //vsub 402
          genVAluOutput("h2468acf02468acf02468acf02468acf0"), //vsub 454
          genVAluOutput("h2468acf02468acf02468acf02468acf0"), //vsub 506
          // and/or/xor
          genVAluOutput("h48540ac748540a881400404f14004040"),
          genVAluOutput("hb4ff7fd9b4ff7ff8fefbd771fefbd750"),
          genVAluOutput("h2c78d5102c78d5702deb75302deb7510"),
          // sll/srl/sra
          genVAluOutput("hdcd0c080dcd0c0b80028f5800028f540"),
          genVAluOutput("hc0000000159d7800ae14714047000000"),
          genVAluOutput("h0001000d0001001a001462280014c440"),
          genVAluOutput("h00d3a06c8ed3a06c0000c16c1616c16c"),
          genVAluOutput("h370501ff370501b80028f5000028f540"),
          genVAluOutput("h11a2fff311a2ffce00011e0400017800"),

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

  def vAluTest1(): Unit = {
    it should "pass the test: min/max(u), vwadd/vwsub(u)" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // min(u) max(u)
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vmin.copy(vsew=0, vl=16)),//vmin.copy(s8, s8, s8, vl=16)),
          genVFuInput(SrcBundle("hfb72e9cbfb72e9b8e93e9353e93e9340", "hd70a3cdbd70a3cc8c4d5e663c4d5e650"), vmin.copy(vsew=2, vl=4)),//vmin.copy(s32, s32, s32, vl=4)),
          genVFuInput(SrcBundle("hb4e81ac9b4e81ab8a2b3c451a2b3c440", "h907f6dd9907f6dc87e4b17617e4b1750"), vminu.copy(vsew=1, vl=8)),//vmin.copy(u16, u16, u16, vl=8)),
          genVFuInput(SrcBundle("h41fdb8ce41fdb8b82fc962562fc96240", "h1d950bde1d950bc80b60b5660b60b550"), vminu.copy(vsew=3, vl=2)),//vmin.copy(u64, u64, u64, vl=2)),
          genVFuInput(SrcBundle("h468ace8a468ace783456781234567800", "h2222219a222221880fedcb220fedcb10"), vmax.copy(vsew=1, vl=8)),//vmax.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("h41fdb8ce41fdb8b82fc962562fc96240", "h1d950bde1d950bc80b60b5660b60b550"), vmax.copy(vsew=3, vl=2)),//vmax.copy(s64, s64, s64, vl=2)),
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), vmaxu.copy(vsew=0, vl=16)),//vmax.copy(u8, u8, u8, vl=16)),
          genVFuInput(SrcBundle("h8d159d8c8d159d787ae147147ae14700", "h68acf09c68acf08856789a2456789a10"), vmaxu.copy(vsew=2, vl=4)),//vmax.copy(u32, u32, u32, vl=4)),
          // vwadd(u).vv vwsub(u).vv
          genVFuInput(SrcBundle("hb3c4d55ab3c4d548a1907ee2a1907ed0", "h8f5c286a8f5c28587d27d1f27d27d1e0"), vwadd.copy(vsew=1, vl=32, widen=true, uopIdx=7)),//vadd.copy(s32, s16, s16, vl=32, uopIdx=7)),
          genVFuInput(SrcBundle("hb3c4d55ab3c4d548a1907ee2a1907ed0", "h8f5c286a8f5c28587d27d1f27d27d1e0"), vwadd.copy(vsew=1, vl=32, widen=true, uopIdx=4)),//vadd.copy(s32, s16, s16, vl=32, uopIdx=4)),
          genVFuInput(SrcBundle("h91a2b34891a2b3387f6e5cd07f6e5cc0", "h6d3a06586d3a06485b05afe05b05afd0"), vwaddu.copy(vsew=0, vl=16, widen=true, uopIdx=0)),//vadd.copy(u16, u8, u8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("h91a2b34891a2b3387f6e5cd07f6e5cc0", "h6d3a06586d3a06485b05afe05b05afd0"), vwaddu.copy(vsew=0, vl=16, widen=true, uopIdx=1)),//vadd.copy(u16, u8, u8, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("hb3c4d55ab3c4d548a1907ee2a1907ed0", "h8f5c286a8f5c28587d27d1f27d27d1e0"), vwsub.copy(vsew=1, vl=8, widen=true, uopIdx=0)),//vsub.copy(s32, s16, s16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("hb3c4d55ab3c4d548a1907ee2a1907ed0", "h8f5c286a8f5c28587d27d1f27d27d1e0"), vwsub.copy(vsew=1, vl=8, widen=true, uopIdx=1)),//vsub.copy(s32, s16, s16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h8bf2581d8bf2580879be01a579be0190", "h6789ab2d6789ab18555554b5555554a0"), vwsubu.copy(vsew=2, vl=4, widen=true, uopIdx=0)),//vsub.copy(u64, u32, u32, vl=4, uopIdx=0)),
          genVFuInput(SrcBundle("h8bf2581d8bf2580879be01a579be0190", "h6789ab2d6789ab18555554b5555554a0"), vwsubu.copy(vsew=2, vl=4, widen=true, uopIdx=1)),//vsub.copy(u64, u32, u32, vl=4, uopIdx=1)),
          // vwadd(u).wv vwsub(u).wv
          genVFuInput(SrcBundle("h080fafe44aa23804cacde65e39bce220", "hed61d264223a7cc075a7129203311402", "h4b0f02011885df170c1445e2acd9a072", "h677f2cc6e11864dd3ba1ef10b39ec020"), 
                      vwadd_wv.copy(vl=16, widen2=true, uopIdx=1, vm=false, ta=false, ma=false)), //vadd.copy(u16, u16, s8, vl=16, uopIdx=1, vm=false, ta=false, ma=false)),
          genVFuInput(SrcBundle("hf809197ef8091968e5d4c306e5d4c2f0", "hd3a06c8ed3a06c78c16c1616c16c1600"), vwadd_wv.copy(vsew=2, vl=4, widen2=true, uopIdx=0)), //vadd.copy(s64, s64, s32, vl=4, uopIdx=0)),
          genVFuInput(SrcBundle("h1c71c66f1c71c6580a3d6ff70a3d6fe0", "hd3a06c8ed3a06c78c16c1616c16c1600"), vwadd_wv.copy(vsew=2, vl=4, widen2=true, uopIdx=1)), //vadd.copy(s64, s64, s32, vl=4, uopIdx=1)),
          genVFuInput(SrcBundle("h8d159d8c8d159d787ae147147ae14700", "h444443ac44444398320fed34320fed20"), vwaddu_wv.copy(vsew=1, vl=8, widen2=true, uopIdx=1)), //vadd.copy(u32, u32, u16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h68acf09c68acf08856789a2456789a10", "h444443ac44444398320fed34320fed20"), vwaddu_wv.copy(vsew=1, vl=8, widen2=true, uopIdx=0)), //vadd.copy(u32, u32, u16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("h2468ac782468ac6812345600123455f0", "hffffff87ffffff78edcba90fedcba900"), vwsub_wv.copy(vsew=0, vl=16, widen2=true, uopIdx=0)), //vsub.copy(s16, s16, s8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("h48d1596848d15958369d02f0369d02e0", "hffffff87ffffff78edcba90fedcba900"), vwsub_wv.copy(vsew=0, vl=16, widen2=true, uopIdx=1)), //vsub.copy(s16, s16, s8, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("hb2a18febb2a18fd8a06d3973a06d3960", "h69d0360b69d035f8579bdf93579bdf80"), vwsubu_wv.copy(vsew=1, vl=8, widen2=true, uopIdx=1)), //vsub.copy(u32, u32, u16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h8e38e2fb8e38e2e87c048c837c048c70", "h69d0360b69d035f8579bdf93579bdf80"), vwsubu_wv.copy(vsew=1, vl=8, widen2=true, uopIdx=0)), //vsub.copy(u32, u32, u16, vl=8, uopIdx=0)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // min(u) max(u)
          genVAluOutput("h49f49ec749f49eb837c0f54f37c0f540"), //vmin 350
          genVAluOutput("hd70a3cdbd70a3cc8c4d5e663c4d5e650"), //vmin 454
          genVAluOutput("h907f1ac9907f1ab87e4b17617e4b1750"), //vminu 402
          genVAluOutput("h1d950bde1d950bc80b60b5660b60b550"), //vminu 506
          genVAluOutput("h468a219a468a21883456781234567800"), //vmax 415
          genVAluOutput("h41fdb8ce41fdb8b82fc962562fc96240"), //vmax 506
          genVAluOutput("h6ef49ed76ef49ec85cc0f55f5cc0f550"), //vmaxu 350
          genVAluOutput("h8d159d8c8d159d787ae147147ae14700"), //vmaxu 467
          // vwadd(u).vv vwsub(u).vv
          genVAluOutput("hffff4320fffffdc4ffff4320fffffda0"), //vwadd 402
          genVAluOutput("h00001eb7000050d400001eb7000050b0"), //vwadd 402
          genVAluOutput("h00da0073010b01b000da0073010b0190"), //vwaddu 363
          genVAluOutput("h00fe00dc00b900a000fe00dc00b90080"), //vwaddu 363
          genVAluOutput("hffff24690000acf0ffff24690000acf0"), //vwsub 402
          genVAluOutput("h00002468ffffacf000002468ffffacf0"), //vwsub 402
          genVAluOutput("h000000002468acf0000000002468acf0"), //vwsubu 454
          genVAluOutput("h000000002468acf0000000002468acf0"), //vwsubu 454
          // vwadd(u).wv vwsub(u).wv
          genVAluOutput("h07fcb0451885df170c1445e2acd9a072"),
          genVAluOutput("hf809197eb9752f7ee5d4c306a740d8f0"), //vwadd.wv 454
          genVAluOutput("h1c71c66ef01232e60a3d6ff6dddddc58"), //vwadd.wv 454
          genVAluOutput("h8d15e1d08d15e1247ae18b587ae18a98"), //vwaddu.wv 415
          genVAluOutput("h68ad22ab68adddbc5678cc3356798730"), //vwaddu.wv 415
          genVAluOutput("h247bacad24bfac5912475635128b55f0"), //vwsub.wv 350
          genVAluOutput("h48d2596948d259d1369e02f1369e0268"), //vwsub.wv 350
          genVAluOutput("hb2a1261bb2a159cda06ccfa3a06d0368"), //vwsubu.wv 402
          genVAluOutput("h8e388b608e3803557c0434e87c03acf0"), //vwsubu.wv 402
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

  def vAluTest2(): Unit = {
    it should "pass the test: vz(s)ext, vmerge/vmv, vadc/vsbc" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vzext, vsext
          genVFuInput(SrcBundle("hbcdf00d2bcdf00c8aaaaaa5aaaaaaa50"), vzext_vf2.copy(vsew=3, vs1_imm=6, vl=4, uopIdx=1)), //vext.copy(u64, u32, u32, vl=4, uopIdx=1)),
          genVFuInput(SrcBundle("hbcdf00d2bcdf00c8aaaaaa5aaaaaaa50"), vzext_vf2.copy(vsew=3, vs1_imm=6, vl=4, uopIdx=0)), //vext.copy(u64, u32, u32, vl=4, uopIdx=0)),
          genVFuInput(SrcBundle("h4b17e4464b17e43838e38dce38e38dc0"), vzext_vf4.copy(vsew=2, vs1_imm=4, vl=16, uopIdx=2)), //vext.copy(u32, u8, u8, vl=16, uopIdx=2)),
          genVFuInput(SrcBundle("h4b17e4464b17e43838e38dce38e38dc0"), vzext_vf4.copy(vsew=2, vs1_imm=4, vl=16, uopIdx=1)), //vext.copy(u32, u8, u8, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("h4b17e4464b17e43838e38dce38e38dc0"), vzext_vf4.copy(vsew=2, vs1_imm=4, vl=16, uopIdx=3)), //vext.copy(u32, u8, u8, vl=16, uopIdx=3)),
          genVFuInput(SrcBundle("h4b17e4464b17e43838e38dce38e38dc0"), vzext_vf4.copy(vsew=2, vs1_imm=4, vl=16, uopIdx=0)), //vext.copy(u32, u8, u8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("he4b17e0fe4b17e08d27d2797d27d2790"), vsext_vf2.copy(vsew=2, vs1_imm=7, vl=8, uopIdx=0)), //vext.copy(s32, s16, s16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("he4b17e0fe4b17e08d27d2797d27d2790"), vsext_vf2.copy(vsew=2, vs1_imm=7, vl=8, uopIdx=1)), //vext.copy(s32, s16, s16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("hb17e4a7cb17e4a689f49f4049f49f3f0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=8, uopIdx=1)), //vext.copy(s64, s16, s16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("hb17e4a7cb17e4a689f49f4049f49f3f0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=8, uopIdx=0)), //vext.copy(s64, s16, s16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("hb17e4a7cb17e4a689f49f4049f49f3f0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=8, uopIdx=2)), //vext.copy(s64, s16, s16, vl=8, uopIdx=2)),
          genVFuInput(SrcBundle("hb17e4a7cb17e4a689f49f4049f49f3f0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=8, uopIdx=3)), //vext.copy(s64, s16, s16, vl=8, uopIdx=3)),
          genVFuInput(SrcBundle("h9be0233f9be0231889abccc789abcca0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=16, uopIdx=6)), //vext.copy(s64, s16, s16, vl=16, uopIdx=6)),
          genVFuInput(SrcBundle("h9be0233f9be0231889abccc789abcca0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=16, uopIdx=7)), //vext.copy(s64, s16, s16, vl=16, uopIdx=7)),
          genVFuInput(SrcBundle("h9be0233f9be0231889abccc789abcca0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=16, uopIdx=5)), //vext.copy(s64, s16, s16, vl=16, uopIdx=5)),
          genVFuInput(SrcBundle("h9be0233f9be0231889abccc789abcca0"), vsext_vf4.copy(vsew=3, vs1_imm=5, vl=16, uopIdx=4)), //vext.copy(s64, s16, s16, vl=16, uopIdx=4)),
          genVFuInput(SrcBundle("hc71c71b9c71c71b8b4e81b41b4e81b40"), vzext_vf8.copy(vsew=3, vs1_imm=2, vl=2, uopIdx=0)), //vext.copy(u64, u8, u8, vl=2, uopIdx=0)),
          genVFuInput(SrcBundle("h30eca83d30eca8381eb851c51eb851c0"), vsext_vf8.copy(vsew=3, vs1_imm=3, vl=2, uopIdx=0)), //vext.copy(s64, s8, s8, vl=2, uopIdx=0)),
          // vmerge
          genVFuInput(SrcBundle("h69d0360b69d035f8579bdf93579bdf80", "h4567891b45678908333332a333333290", mask="hfc962f3afc962f28ea61d8c2ea61d8b0"), 
                       vmerge.copy(vsew=0, vl=16, vm=false)), //vmerge.copy(u8, u8, u8, vl=16, vm=false)),
          genVFuInput(SrcBundle("h1a2b3b911a2b3b7807f6e51907f6e500", "hf5c28ea0f5c28e88e38e3828e38e3810", mask="hacf134c0acf134a89abcde489abcde30"), 
                       vmerge.copy(vsew=2, vl=4, vm=false)), //vmerge.copy(u32, u32, u32, vl=4, vm=false)),
          genVFuInput(SrcBundle("hce2e258a5117c9b22ac57f6b96e60d9f", "h2ca6886dc1bde386bc299f61f0fdb287", mask="hb73b8d5c4de20129c1887e21212af65d"), 
                       vmerge.copy(vsew=2, vl=4, vm=false)), //vmerge.copy(u32, u32, u32, vl=4, vm=false)),
          genVFuInput(SrcBundle("h1", "hca864116ca8640f8b851ea9eb851ea80"), 
                       vmv.copy(vsew=2, vl=4)), //vmv.copy(u32, u32, u32, vl=4)),
          // vadc vsbc
          genVFuInput(SrcBundle("hb05b050db05b04f89e26ae959e26ae80", "h8bf2581d8bf2580879be01a579be0190", mask="h4320fe3d4320fe2830eca7c530eca7b0"), 
                      vadc.copy(vsew=1, vl=8, vm=false)), //vadc.copy(s16, s16, s16, vl=8, vm=false)),
          genVFuInput(SrcBundle("hf258be53f258be38e02467dbe02467c0", "hcdf01163cdf01148bbbbbaebbbbbbad0", mask="h851eb783851eb76872ea610b72ea60f0"), 
                      vadc.copy(vsew=3, vl=2, vm=false)), //vadc.copy(s64, s64, s64, vl=2, vm=false)),
          genVFuInput(SrcBundle("hd82d824ad82d8238c5f92bd2c5f92bc0", "hb3c4d55ab3c4d548a1907ee2a1907ed0", mask="h6af37b7a6af37b6858bf250258bf24f0"), 
                      vsbc.copy(vsew=0, vl=16, vm=false)), //vsbc.copy(s8, s8, s8, vl=16, vm=false)),
          genVFuInput(SrcBundle("h1a2b3b911a2b3b7807f6e51907f6e500", "hf5c28ea0f5c28e88e38e3828e38e3810", mask="hacf134c0acf134a89abcde489abcde30"), 
                      vsbc.copy(vsew=2, vl=4, vm=false)), //vsbc.copy(s32, s32, s32, vl=4, vm=false)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vzext, vsext
          genVAluOutput("h00000000bcdf00d200000000bcdf00c8"), //vzext.vf2 239
          genVAluOutput("h00000000aaaaaa5a00000000aaaaaa50"), //vzext.vf2 239
          genVAluOutput("h0000004b00000017000000e400000038"), //vzext.vf4 275
          genVAluOutput("h00000038000000e30000008d000000ce"), //vzext.vf4 275
          genVAluOutput("h0000004b00000017000000e400000046"), //vzext.vf4 275
          genVAluOutput("h00000038000000e30000008d000000c0"), //vzext.vf4 275
          genVAluOutput("hffffd27d00002797ffffd27d00002790"), //vsext.vf2 191
          genVAluOutput("hffffe4b100007e0fffffe4b100007e08"), //vsext.vf2 191
          genVAluOutput("hffffffffffff9f49fffffffffffff404"), //vsext.vf4 335
          genVAluOutput("hffffffffffff9f49fffffffffffff3f0"), //vsext.vf4 335
          genVAluOutput("hffffffffffffb17e0000000000004a68"), //vsext.vf4 335
          genVAluOutput("hffffffffffffb17e0000000000004a7c"), //vsext.vf4 335
          genVAluOutput("hffffffffffff9be00000000000002318"), //vsext.vf4 431
          genVAluOutput("hffffffffffff9be0000000000000233f"), //vsext.vf4 431
          genVAluOutput("hffffffffffff89abffffffffffffccc7"), //vsext.vf4 431
          genVAluOutput("hffffffffffff89abffffffffffffcca0"), //vsext.vf4 431
          genVAluOutput("h000000000000001b0000000000000040"), //vzext.vf8 51
          genVAluOutput("h0000000000000051ffffffffffffffc0"), //vsext.vf8 155
          // vmerge
          genVAluOutput("h4567361b45d035f8339b32a3579bdf80"), //vmerge 441
          genVAluOutput("h1a2b3b911a2b3b7807f6e51907f6e500"), //vmerge 571
          genVAluOutput("h2ca6886dc1bde3862ac57f6bf0fdb287"), //vmerge 755
          genVAluOutput("hca864116ca8640f8b851ea9eb851ea80"), //vmv
          // vadc vsbc
          genVAluOutput("h3c4e5d2a3c4e5d0117e4b03a17e4b010"), //vadc 493
          genVAluOutput("hc048cfb7c048cf809be022c79be02290"), //vadc 636
          genVAluOutput("h2569acf02568adf02368acef2469adf0"), //vsbc 428
          genVAluOutput("h2468acf12468acf02468acf12468acf0"), //vsbc 428
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

  def vAluTest3(): Unit = {
    it should "pass the test: vnsrl(a), compare, vmadc/vmsbc" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vnsrl, vnsra
          genVFuInput(SrcBundle("h6d3a06586d3a06485b05afe05b05afd0", "h48d1596848d15958369d02f0369d02e0", "h2468ac782468ac6812345600123455f0"), 
                       vnsrl.copy(vsew=0, narrow=true, vl=16, uopIdx=0)), //vsrl.copy(u8, u16, u8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("h91a2b34891a2b3387f6e5cd07f6e5cc0", "h48d1596848d15958369d02f0369d02e0", "h2468ac782468ac68b4004e486c05c1d0"), 
                       vnsrl.copy(vsew=0, narrow=true, vl=16, uopIdx=1)), //vsrl.copy(u8, u16, u8, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("h69d0360b69d035f8579bdf93579bdf80", "h20fedc2b20fedc180eca85b30eca85a0", "hfc962f3afc962f2859e208accccc3290"), 
                       vnsrl.copy(vsew=1, narrow=true, vl=8, uopIdx=1)), //vsrl.copy(u16, u32, u16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h4567891b45678908333332a333333290", "h20fedc2b20fedc180eca85b30eca85a0", "hfc962f3afc962f28ea61d8c2ea61d8b0"), 
                       vnsrl.copy(vsew=1, narrow=true, vl=8, uopIdx=0)), //vsrl.copy(u16, u32, u16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("h8bf2581d8bf2580879be01a579be0190", "h4320fe3d4320fe2830eca7c530eca7b0", "h1eb8514d1eb851386b3c4d5800005555"), 
                       vnsra.copy(vsew=2, narrow=true, vl=4, uopIdx=1)), //vsra.copy(s32, s64, s32, vl=4, uopIdx=1)),
          genVFuInput(SrcBundle("h6789ab2d6789ab18555554b5555554a0", "h4320fe3d4320fe2830eca7c530eca7b0", "h1eb8514d1eb851380c83fad50c83fac0"), 
                       vnsra.copy(vsew=2, narrow=true, vl=4, uopIdx=0)), //vsra.copy(s32, s64, s32, vl=4, uopIdx=0)),
          genVFuInput(SrcBundle("h8f5c286a8f5c28587d27d1f27d27d1e0", "h6af37b7a6af37b6858bf250258bf24f0", "h468ace8a468ace783456781234567800"), 
                       vnsra.copy(vsew=1, narrow=true, vl=8, uopIdx=0)), //vsra.copy(s16, s32, s16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("hb3c4d55ab3c4d548a1907ee2a1907ed0", "h6af37b7a6af37b6858bf250258bf24f0", "h468ace8a468ace78ffff0a1600007d27"), 
                       vnsra.copy(vsew=1, narrow=true, vl=8, uopIdx=1)), //vsra.copy(s16, s32, s16, vl=8, uopIdx=1)),
          // // vec compare (& tail test)
          // genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), 
          //              vmseq.copy(vsew=0, narrow_to_1=true, vl=16, uopIdx=0)), //vmseq.copy(mask, s8, s8, vl=16, uopIdx=0)),
          // genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850", "h258bf1e7258bf1d813579b6f13579b60"), 
          //              vmseq.copy(vsew=0, narrow_to_1=true, vl=16, ta=false, uopIdx=0)), //vmseq.copy(mask, s8, s8, vl=16, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("ha740d995a740d978950c831d950c8300", "h5e6f7fb55e6f7f984c3b293d4c3b2920", "h159e25d5159e25b80369cf5d0369cf40"), 
          //   vmsne.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, uopIdx=0)), //vmsne.copy(mask, s16, s16, vl=16, vlmul=1, uopIdx=0)),
          // genVFuInput(SrcBundle("ha740d995a740d978950c831d950c8300", "h5e6f7fb55e6f7f984c3b293d4c3b2920", "h159e25d5159e25b80369cf5d0369cf40"), 
          //   vmsne.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, ta=false, uopIdx=0)), //vmsne.copy(mask, s16, s16, vl=16, vlmul=1, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("hcba98685cba98668b975300db9752ff0", "h82d82ca582d82c8870a3d62d70a3d610", "h159e25d5159e25b80369cf5d0369cfff"), 
          //   vmsne.copy(vsew=1, narrow_to_1=true, vl=14, vlmul=1, ta=false, uopIdx=1)), //vmsne.copy(mask, s16, s16, vl=14, vlmul=1, ta=false, uopIdx=1)),
          // genVFuInput(SrcBundle("h83fb712483fb70e871c71aac71c71a70", "hf258bd63f258bd28e02466ebe02466b0", "h60b609a360b609684e81b32b4e81b2f0"), 
          //   vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=0)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=0)),
          // genVFuInput(SrcBundle("ha8641e14a8641dd8962fc79c962fc760", "h16c16a5416c16a18048d13dc048d13a0", "hfffffffffffffffffffffffffffffaff"), 
          //   vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=1)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=1)),
          // genVFuInput(SrcBundle("hcccccb04cccccac8ba98748cba987450", "h3b2a17443b2a170828f5c0cc28f5c090", "hfffffffffffffffffffffffffffffa0f"), 
          //   vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=2)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=2)),
          // genVFuInput(SrcBundle("hf5c28bd0f5c28b58e38e3558e38e34e0", "hd27d244fd27d23d8c048cdd7c048cd60", "h666662ee6666627854320c7654320c01"), 
          //   vmslt.copy(vsew=3, narrow_to_1=true, vl=11, vlmul=3, ta=false, uopIdx=5)), //vmslt.copy(mask, s64, s64, vl=11, vlmul=3, ta=false, uopIdx=5)),
          // genVFuInput(SrcBundle("hf5c28bd0f5c28b58e38e3558e38e34e0", "hd27d244fd27d23d8c048cdd7c048cd60", "h666662ee6666627854320c7654320c01"), 
          //   vmslt.copy(vsew=3, narrow_to_1=true, vl=12, vlmul=3, ta=false, uopIdx=5)), //vmslt.copy(mask, s64, s64, vl=12, vlmul=3, ta=false, uopIdx=5)),
          // genVFuInput(SrcBundle("haaaaa9e2aaaaa9c89876536a98765350", "h61d9500261d94fe84fa4f98a4fa4f970", "h1907f6221907f60806d39faa06d39f90"), 
          //   vmsleu.copy(vsew=0, narrow_to_1=true, vl=18, vlmul=1, ta=false, uopIdx=0)), //vmsle.copy(mask, u8, u8, vl=18, vlmul=1, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("hcf1356d2cf1356b8bcdf005abcdf0040", "h8641fcf28641fcd8740da67a740da660", "h1907f6221907f60806d39faa06d34577"), 
          //   vmsleu.copy(vsew=0, narrow_to_1=true, vl=22, vlmul=1, ta=false, uopIdx=1)), //vmsle.copy(mask, u8, u8, vl=22, vlmul=1, ta=false, uopIdx=1)),
          // genVFuInput(SrcBundle("h6789aa3d6789aa08555553c555555390", "hd5e6f67cd5e6f648c3b2a004c3b29fd0", "hfb72e8dbfb72e8a8e93e9263e93e550f"), 
          //   vmsle.copy(vsew=1, narrow_to_1=true, vl=27, vlmul=2, ta=true, uopIdx=2)), //vmsle.copy(mask, s16, s16, vl=27, vlmul=2, ta=true, uopIdx=2)),
          // genVFuInput(SrcBundle("h8bf2572d8bf256f879be00b579be0080", "hfa4fa36cfa4fa338e81b4cf4e81b4cc0", "hfffffffffffffffffffffffff950550f"), 
          //   vmsle.copy(vsew=1, narrow_to_1=true, vl=27, vlmul=2, ta=true, uopIdx=3)), //vmsle.copy(mask, s16, s16, vl=27, vlmul=2, ta=true, uopIdx=3)),
          // genVFuInput(SrcBundle("h22222212222222100fedcb9a0fedcb98", rs1="h58bf257a58bf2578", old_vd="hd950c831d950c830c71c71b9c71c71b8"), 
          //   vmsgtu_vx.copy(vsew=3, narrow_to_1=true, vl=4, vlmul=1, ta=true, uopIdx=0)), //vmsgt.copy(mask, u64, u64, vl=4, vlmul=1, ta=true, uopIdx=0)),
          // genVFuInput(SrcBundle("h468acf02468acf003456788a34567888", rs1="h58bf257a58bf2578", old_vd="hfffffffffffffffffffffffffffffff8"), 
          //   vmsgtu_vx.copy(vsew=3, narrow_to_1=true, vl=4, vlmul=1, ta=true, uopIdx=1)), //vmsgt.copy(mask, u64, u64, vl=4, vlmul=1, ta=true, uopIdx=1)),
          // genVFuInput(SrcBundle("hfedcba90fedcbaa8eca86418eca86430", rs1="h0018001800180018", old_vd="h48d159e048d159f8369d0368369d0350"), 
          //   vmsgt_vx.copy(vsew=1, narrow_to_1=true, vl=19, vlmul=2, ta=false, uopIdx=1)), //vmsgt.copy(mask, s16, s16, vl=19, vlmul=2, ta=false, uopIdx=1)),
          // genVFuInput(SrcBundle("h23456781234567981111110911111120", rs1="h0018001800180018", old_vd="h48d159e048d159f8369d0368369d0550"), 
          //   vmsgt_vx.copy(vsew=1, narrow_to_1=true, vl=19, vlmul=2, ta=false, uopIdx=2)), //vmsgt.copy(mask, s16, s16, vl=19, vlmul=2, ta=false, uopIdx=2)),
          // //vmadc/vmsbc.vvm
          // genVFuInput(SrcBundle("hf6e5d40ff6e5d3f8e4b17d97e4b17d80", "hd27d271fd27d2708c048d0a7c048d090", mask="h89abcd3f89abcd28777776c7777776b0"), 
          //   vmadc_vvm.copy(vsew=0, narrow_to_1=true, vl=16, vlmul=0, vm=false, uopIdx=0)), //vmadc.copy(mask, s8, s8, vl=16, vlmul=0, vm=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h091a2a10091a29e8f6e5d397f6e5d370", "hc048d02fc048d008ae1479b7ae147990", "h7777764f7777762865431fd765431fb0", mask="h530ec95f530ec93840da72e740da72c0"), 
          //   vmadc_vvm.copy(vsew=0, narrow_to_1=true, vl=32, vlmul=0, vm=false, uopIdx=0)), //vmadc.copy(mask, s8, s8, vl=32, vlmul=0, vm=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h2d82d7002d82d6d81b4e80881b4e8060", "he4b17d1fe4b17cf8d27d26a7d27d2680", "hffffffffffffffffffffffff654300bb", mask="h530ec95f530ec93840da72e740da72c0"), 
          //   vmadc_vvm.copy(vsew=0, narrow_to_1=true, vl=32, vlmul=0, vm=false, uopIdx=1)), //vmadc.copy(mask, s8, s8, vl=32, vlmul=0, vm=false, uopIdx=1)),
          // genVFuInput(SrcBundle("h71c71a3471c719e85f92c3bc5f92c370", "he0246673e0246628cdf00ffbcdf00fb0", "hfffffffffffffffffffffffff37cf005", mask="he147abe2e147ab98cf13556acf135520"), 
          //   vmadc_vvm.copy(vsew=1, narrow_to_1=true, vl=32, vlmul=2, vm=false, uopIdx=2)), //vmadc.copy(mask, s16, s16, vl=32, vlmul=2, vm=false, uopIdx=2)),
          // genVFuInput(SrcBundle("h2ea61c6f2ea61c481c71c5f71c71c5d0", "he5d4c28ee5d4c268d3a06c16d3a06bf0", "hffffffffffffffffffffffff6666dd76", mask="h54320ece54320ea841fdb85641fdb830"), 
          //   vmsbc_vvm.copy(vsew=0, narrow_to_1=true, vl=32, vlmul=2, vm=false, uopIdx=1)), //vmsbc.copy(mask, s8, s8, vl=32, vlmul=2, vm=false, uopIdx=1)),
          // genVFuInput(SrcBundle("h71c71a3471c719e85f92c3bc5f92c370", "he0246673e0246628cdf00ffbcdf00fb0", "hffffffffffffffffffffffff1234ffaf", mask="he147abe2e147ab98cf13556acf135520"), 
          //   vmsbc_vvm.copy(vsew=1, narrow_to_1=true, vl=28, vlmul=2, vm=false, uopIdx=2)), //vmsbc.copy(mask, s16, s16, vl=28, vlmul=2, vm=false, uopIdx=2)),
          // //vmadc/vmsbc.vv
          // genVFuInput(SrcBundle("h7c048c0b7c048be869d0359369d03570", "h579bdf1b579bdef8456788a345678880"), 
          //   vmadc_vv.copy(vsew=0, narrow_to_1=true, vl=16, vlmul=0, uopIdx=0)), //vmadc.copy(mask, s8, s8, vl=16, vlmul=0, uopIdx=0)),
          // genVFuInput(SrcBundle("hedcba7a7edcba768db97512fdb9750f0", "ha4fa4dc7a4fa4d8892c5f74f92c5f710", "h8091a0d78091a0986e5d4a5f6e5d4a20"), 
          //   vmadc_vv.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, uopIdx=0)), //vmadc.copy(mask, s16, s16, vl=16, vlmul=1, uopIdx=0)),
          // genVFuInput(SrcBundle("h1234549812345458fffffe1ffffffde0", "hc962fab7c962fa78b72ea43fb72ea400", "hffffffffffffffffffffffffffff4aaf"), 
          //   vmadc_vv.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, uopIdx=1)), //vmadc.copy(mask, s16, s16, vl=16, vlmul=1, uopIdx=1)),
          // genVFuInput(SrcBundle("h59e269f959e269d847ae138147ae1360", "h3579bd093579bce82345669123456670"), 
          //   vmsbc_vv.copy(vsew=0, narrow_to_1=true, vl=16, vlmul=0, uopIdx=0)), //vmsbc.copy(mask, s8, s8, vl=16, vlmul=0, uopIdx=0)),
          // genVFuInput(SrcBundle("h7f6e5c587f6e5c386d3a05e06d3a05c0", "h5b05af685b05af4848d158f048d158d0"), 
          //   vmsbc_vv.copy(vsew=0, narrow_to_1=true, vl=16, vlmul=0, uopIdx=0)), //vmsbc.copy(mask, s8, s8, vl=16, vlmul=0, uopIdx=0)),
          // genVFuInput(SrcBundle("hedcba7a7edcba768db97512fdb9750f0", "ha4fa4dc7a4fa4d8892c5f74f92c5f710", "h8091a0d78091a0986e5d4a5f6e5d4a20"), 
          //   vmsbc_vv.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, uopIdx=0)), //vmsbc.copy(mask, s16, s16, vl=16, vlmul=1, uopIdx=0)),
          // genVFuInput(SrcBundle("h1234549812345458fffffe1ffffffde0", "hc962fab7c962fa78b72ea43fb72ea400", "hffffffffffffffffffffffffffff4a05"), 
          //   vmsbc_vv.copy(vsew=1, narrow_to_1=true, vl=16, vlmul=1, uopIdx=1)), //vmsbc.copy(mask, s16, s16, vl=16, vlmul=1, uopIdx=1)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vnsrl, vnsra
          genVAluOutput("h2468ac782468ac68b4004e486c05c1d0"), //vnsrl 350
          genVAluOutput("h91a448b37f683f5cb4004e486c05c1d0"), //vnsrl 350
          genVAluOutput("h00013a060001005759e208accccc3290"), //vnsrl 415
          genVAluOutput("hfc962f3afc962f2859e208accccc3290"), //vnsrl 415
          genVAluOutput("hfffffffc0079be016b3c4d5800005555"), //vnsra 454
          genVAluOutput("h1eb8514d1eb851386b3c4d5800005555"), //vnsra 454
          genVAluOutput("h468ace8a468ace78ffff0a1600007d27"), //vnsra 402
          genVAluOutput("hf678ffecf432907effff0a1600007d27"), //vnsra 402
          // // vec compare
          // genVAluOutput("hffffffffffffffffffffffffffff0000"), //vmseq 350
          // genVAluOutput("hffffffffffffffffffffffffffff0000"), //vmseq 350
          // genVAluOutput("hffffffffffffffffffffffffffffcfff"), //vmsne 610
          // genVAluOutput("hffffffffffffffffffffffffffffcfff"), //vmsne 610
          // genVAluOutput("hffffffffffffffffffffffffffffffff"), //vmsne 610
          // genVAluOutput("hfffffffffffffffffffffffffffffaff"), //vmsltu 883
          // genVAluOutput("hfffffffffffffffffffffffffffffa0f"), //vmsltu 883
          // genVAluOutput("hfffffffffffffffffffffffffffff80f"), //vmsltu 883
          // genVAluOutput("hfffffffffffffffffffffffffffff801"), //vmslt 1130
          // genVAluOutput("hfffffffffffffffffffffffffffff001"), //vmslt 1130
          // genVAluOutput("hffffffffffffffffffffffffffff4577"), //vmsleu 571
          // genVAluOutput("hfffffffffffffffffffffffffff34577"), //vmsleu 571
          // genVAluOutput("hfffffffffffffffffffffffff950550f"), //vmsle 831
          // genVAluOutput("hfffffffffffffffffffffffffd50550f"), //vmsle 831
          // genVAluOutput("hfffffffffffffffffffffffffffffff8"), //vmsgtu.vx 714
          // genVAluOutput("hfffffffffffffffffffffffffffffff0"), //vmsgtu.vx 714
          // genVAluOutput("hfffffffffffffffffffffffffffd0550"), //vmsgt.vx 818
          // genVAluOutput("hffffffffffffffffffffffffffff0550"), //vmsgt.vx 818
          // //vmadc/vmsbc.vvm
          // genVAluOutput("hffffffffffffffffffffffffffffcdbb"), //vmadc.vvm 587
          // genVAluOutput("hffffffffffffffffffffffff654300bb"), //vmadc.vvm 923
          // genVAluOutput("hffffffffffffffffffffffffef1000bb"), //vmadc.vvm 923
          // genVAluOutput("hfffffffffffffffffffffffff3aaf005"), //vmadc.vvm 1315
          // genVAluOutput("hffffffffffffffffffffffffffcddd76"), //vmsbc.vvm 909
          // genVAluOutput("hfffffffffffffffffffffffff2faffaf"), //vmsbc.vvm 1315
          // //vmadc/vmsbc.vv
          // genVAluOutput("hffffffffffffffffffffffffffff2354"), //vmadc.vv 1078
          // genVAluOutput("hffffffffffffffffffffffffffff4aaf"), //vmadc.vv 1793
          // genVAluOutput("hffffffffffffffffffffffffffff5faf"), //vmadc.vv 1793
          // genVAluOutput("hffffffffffffffffffffffffffff2333"), //vmsbc.vv 1013
          // genVAluOutput("hffffffffffffffffffffffffffff3377"), //vmsbc.vv 987
          // genVAluOutput("hffffffffffffffffffffffffffff4a05"), //vmsbc.vv 1793
          // genVAluOutput("hfffffffffffffffffffffffffffff005"), //vmsbc.vv 1793
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

  def vAluTest4(): Unit = {
    it should "pass the test: mask-reg logical, fixed-point" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // // mask-reg logical
          // genVFuInput(SrcBundle("h26af375626af3748147ae0de147ae0d0", "h02468a6602468a58f01233edf01233e0"), 
          //              vmand.copy(mask, mask, mask, vl=8, vlmul=7, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h159e25d5159e25b80369cf5d0369cf40", "hf13578e4f13578c8df01226cdf012250"), 
          //              vmnand.copy(mask, mask, mask, vl=16, vlmul=2, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("hba987504ba9874d8a8641e8ca8641e60", "h71c71b2471c71af85f92c4ac5f92c480"), 
          //              vmandn.copy(mask, mask, mask, vl=32, vlmul=2, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("hba987504ba9874d8a8641e8ca8641e60", "h71c71b2471c71af85f92c4ac5f92c480"), 
          //              vmandn.copy(mask, mask, mask, vl=27, vlmul=2, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h98765202987651b88641fb8a8641fb40", "h06d39e4206d39df8f49f47c9f49f4780"), 
          //              vmxor.copy(mask, mask, mask, vl=64, vlmul=2, ta=true, uopIdx=0)),
          // genVFuInput(SrcBundle("h530ec59f530ec4f840da6f2740da6e80", "h2fc95e1e2fc95d781d9507a61d950700"), 
          //              vmor.copy(mask, mask, mask, vl=46, vlmul=3, ta=true, uopIdx=0)),
          // genVFuInput(SrcBundle("h0eca826b0eca81e8fc962bf2fc962b70", "heb851ae9eb851a68d950c471d950c3f0"), 
          //              vmnor.copy(mask, mask, mask, vl=128, vlmul=3, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h907f6a19907f69887e4b13a17e4b1310", "h6d3a02986d3a02085b05ac205b05ab90"), 
          //              vmorn.copy(mask, mask, mask, vl=121, vlmul=0, ta=false, uopIdx=0)),
          // genVFuInput(SrcBundle("h80919d1780919c586e5d469f6e5d45e0", "h5d4c35965d4c34d84b17df1e4b17de60"), 
          //              vmxnor.copy(mask, mask, mask, vl=20, vlmul=0, ta=false, uopIdx=0)),
          // fixed-point
          genVFuInput(SrcBundle("h0c83fa5d0c83fa38fa4fa3e4fa4fa3c0", "he81b4d6ce81b4d48d5e6f6f4d5e6f6d0"), 
                      vaaddu.copy(vsew=0, vl=16, vlmul=0, uopIdx=0)), //vaadd.copy(u8, u8, u8, vl=16, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990", "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"), 
                      vaaddu.copy(vsew=0, vl=16, vlmul=0, vm=false, uopIdx=0)), //vaadd.copy(u8, u8, u8, vl=16, vlmul=0, vm=false, uopIdx=0)),
          genVFuInput(SrcBundle("hd70a3bebd70a3bb8c4d5e573c4d5e540", "hb2a18efbb2a18ec8a06d3883a06d3850"), 
                      vaaddu.copy(vsew=3, vl=1, vlmul=0, uopIdx=0)), //vaadd.copy(u64, u64, u64, vl=1, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hbcdeffe2bcdeffb8aaaaa96aaaaaa940", "h987652f2987652c88641fc7a8641fc50"), 
                      vaadd.copy(vsew=1, vl=8, vlmul=0, uopIdx=0)), //vaadd.copy(s16, s16, s16, vl=8, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hdb9751a7db975178c962fb2fc962fb00", "hb72ea4b7b72ea488a4fa4e3fa4fa4e10"), 
                      vaadd.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vaadd.copy(s32, s32, s32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h2b3c4c222b3c4bf81907f5aa1907f580", "h06d39f3206d39f08f49f48b9f49f4890"), 
                      vasubu.copy(vsew=1, vl=8, vlmul=0, uopIdx=0)), //vasub.copy(u16, u16, u16, vl=8, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hdb9751a7db975178c962fb2fc962fb00", "hb72ea4b7b72ea488a4fa4e3fa4fa4e10"), 
                      vasubu.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vasub.copy(u32, u32, u32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h0c83fa5d0c83fa38fa4fa3e4fa4fa3c0", "he81b4d6ce81b4d48d5e6f6f4d5e6f6d0"), 
                      vasub.copy(vsew=0, vl=16, vlmul=0, uopIdx=0)), //vasub.copy(s8, s8, s8, vl=16, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h8bf2572d8bf256f879be00b579be0080", "h6789aa3d6789aa08555553c555555390"), 
                      vasub.copy(vsew=3, vl=2, vlmul=0, uopIdx=0)), //vasub.copy(s64, s64, s64, vl=2, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("he93e92dbe93e92b8d70a3c63d70a3c40", "hc4d5e5ebc4d5e5c8b2a18f73b2a18f50"), 
                      vssrl.copy(vsew=0, vl=16, vlmul=0, uopIdx=0)), //vssrl.copy(s8, s8, s8, vl=16, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hbcdeffe2bcdeffb8aaaaa96aaaaaa940", "h987652f2987652c88641fc7a8641fc50"), 
                      vssrl.copy(vsew=1, vl=8, vlmul=0, uopIdx=0)), //vssrl.copy(s16, s16, s16, vl=8, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hdb9751a7db975178c962fb2fc962fb00", "hb72ea4b7b72ea488a4fa4e3fa4fa4e10"), 
                      vssra.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vssra.copy(s32, s32, s32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hfa4fa36cfa4fa338e81b4cf4e81b4cc0", "hd5e6f67cd5e6f648c3b2a004c3b29fd0"), 
                      vssra.copy(vsew=3, vl=2, vlmul=0, uopIdx=0)), //vssra.copy(s64, s64, s64, vl=2, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h962fc904962fc8f883fb728c83fb7280", "h71c71c1471c71c085f92c59c5f92c590"), 
                      vsaddu.copy(vsew=0, vl=16, vlmul=0, uopIdx=0)), //vsadd.copy(u8, u8, u8, vl=16, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hea61d93aea61d938d82d82c2d82d82c0", "hc5f92c4ac5f92c48b3c4d5d2b3c4d5d0", "ha1907f5aa1907f588f5c28e28f5c28e0"), 
                      vsadd.copy(vsew=0, vl=1, vlmul=0, ta=false, uopIdx=0)), //vsadd.copy(s8, s8, s8, vl=1, vlmul=0, ta=false, uopIdx=0)),
          genVFuInput(SrcBundle("h147adf76147adf38024688fe024688c0", "hf0123285f0123248dddddc0ddddddbd0"), 
                      vssubu.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vssub.copy(u32, u32, u32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("ha61d9336a61d92f893e93cbe93e93c80", "h81b4e64681b4e6086f808fce6f808f90"), 
                      vssubu.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vssub.copy(u32, u32, u32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("hc16c159ec16c1578af37bf26af37bf00", "h9d0368ae9d0368888acf12368acf1210"), 
                      vssub.copy(vsew=1, vl=8, vlmul=0, uopIdx=0)), //vssub.copy(s16, s16, s16, vl=8, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h6789a94d6789a8f8555552d555555280", "h4320fc5d4320fc0830eca5e530eca590", "h1eb84f6d1eb84f180c83f8f50c83f8a0"), 
                      vsadd.copy(vsew=3, vl=1, vlmul=0, ta=false, uopIdx=0)), //vsadd.copy(s64, s64, s64, vl=1, vlmul=0, ta=false, uopIdx=0)),
          genVFuInput(SrcBundle("h530ec95f530ec93840da72e740da72c0", "h2ea61c6f2ea61c481c71c5f71c71c5d0"), 
                      vsaddu.copy(vsew=1, vl=8, vlmul=0, uopIdx=0)), //vsadd.copy(u16, u16, u16, vl=8, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h0fedc9ba0fedc978fdb97341fdb97300", "heb851cc9eb851c88d950c651d950c610"), 
                      vssubu.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vssub.copy(u32, u32, u32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("ha61d9336a61d92f893e93cbe93e93c80", "h81b4e64681b4e6086f808fce6f808f90"), 
                      vssub.copy(vsew=2, vl=4, vlmul=0, uopIdx=0)), //vssub.copy(s32, s32, s32, vl=4, vlmul=0, uopIdx=0)),
          genVFuInput(SrcBundle("h091a2b00091a2af8f6e5d487f6e5d480", "he4b17e0fe4b17e08d27d2797d27d2790"), 
                      vaadd.copy(vsew=0, vl=16, vlmul=0, vxrm=1, uopIdx=0)),
          genVFuInput(SrcBundle("he93e92dbe93e92b8d70a3c63d70a3c40", "hc4d5e5ebc4d5e5c8b2a18f73b2a18f50"), 
                      vaaddu.copy(vsew=1, vl=8, vlmul=0, vxrm=2, uopIdx=0)), //vaadd.copy(u16, u16, u16, vl=8, vlmul=0, vxrm=2, uopIdx=0)),
          genVFuInput(SrcBundle("hc962fab7c962fa78b72ea43fb72ea400", "ha4fa4dc7a4fa4d8892c5f74f92c5f710"), 
                      vasub.copy(vsew=2, vl=4, vlmul=0, vxrm=3, uopIdx=0)), //vasub.copy(s32, s32, s32, vl=4, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("h2c5f92812c5f92781a2b3c091a2b3c00", "h07f6e59107f6e588f5c28f18f5c28f10"), 
                      vasubu.copy(vsew=0, vl=16, vlmul=0, vxrm=3, uopIdx=0)), //vasub.copy(u8, u8, u8, vl=16, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("h091a2b00091a2af8f6e5d487f6e5d480", "he4b17e0fe4b17e08d27d2797d27d2790"), 
                      vssrl.copy(vsew=0, vl=16, vlmul=0, vxrm=1, uopIdx=0)), //vssrl.copy(s8, s8, s8, vl=16, vlmul=0, vxrm=1, uopIdx=0)),
          genVFuInput(SrcBundle("h765430e0765430b8641fda68641fda40", "h51eb83f051eb83c83fb72d783fb72d50"), 
                      vssrl.copy(vsew=1, vl=8, vlmul=0, vxrm=2, uopIdx=0)), //vssrl.copy(s16, s16, s16, vl=8, vlmul=0, vxrm=2, uopIdx=0)),
          genVFuInput(SrcBundle("h567898bc567898784444424444444200", "h320febcc320feb881fdb95541fdb9510"), 
                      vssrl.copy(vsew=2, vl=4, vlmul=0, vxrm=3, uopIdx=0)), //vssrl.copy(s32, s32, s32, vl=4, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("ha4fa4cd7a4fa4c7892c5f65f92c5f600", "h80919fe780919f886e5d496f6e5d4910"), 
                      vssrl.copy(vsew=3, vl=2, vlmul=0, vxrm=3, uopIdx=0)), //vssrl.copy(s64, s64, s64, vl=2, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("hb9753085b9753078a740da0da740da00", "h950c8395950c838882d82d1d82d82d10"), 
                      vssra.copy(vsew=0, vl=16, vlmul=0, vxrm=3, uopIdx=0)), //vssra.copy(s8, s8, s8, vl=16, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("h7ae1469c7ae1467868acf02468acf000", "h567899ac567899884444433444444310"), 
                      vssra.copy(vsew=1, vl=8, vlmul=0, vxrm=3, uopIdx=0)), //vssra.copy(s16, s16, s16, vl=8, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("h7e4b15f97e4b15b86c16bf816c16bf40", "h59e2690959e268c847ae129147ae1250"), 
                      vssra.copy(vsew=2, vl=4, vlmul=0, vxrm=2, uopIdx=0)), //vssra.copy(s32, s32, s32, vl=4, vlmul=0, vxrm=2, uopIdx=0)),
          genVFuInput(SrcBundle("h3b2a16543b2a15f828f5bfdc28f5bf80", "h16c1696416c16908048d12ec048d1290"), 
                      vssra.copy(vsew=3, vl=2, vlmul=0, vxrm=1, uopIdx=0)), //vssra.copy(s64, s64, s64, vl=2, vlmul=0, vxrm=1, uopIdx=0)),
          // vnclip(u)
          genVFuInput(SrcBundle("hd70a382bd70a3778c4d5e1b3c4d5e100", "hb2a18b3bb2a18a88a06d34c3a06d3410", "h8e38de4b8e38dd987c0487d37c048720"), 
                      vnclipu.copy(vsew=1, narrow=true, vl=8, vlmul=0, vxrm=0, uopIdx=0)), //vssrl.copy(u16, u32, u16, vl=8, vlmul=0, vxrm=0, uopIdx=0)),
          genVFuInput(SrcBundle("hfb72e51bfb72e468e93e8ea3e93e8df0", "hb2a18b3bb2a18a88a06d34c3a06d3410", "h8e38de4b8e38dd98ffffffffffffc4d6"), 
                      vnclipu.copy(vsew=1, narrow=true, vl=8, vlmul=0, vxrm=0, uopIdx=1)), //vssrl.copy(u16, u32, u16, vl=8, vlmul=0, vxrm=0, uopIdx=1)),
          genVFuInput(SrcBundle("hb4e81709b4e81678a2b3c091a2b3c000", "h907f6a19907f69887e4b13a17e4b1310", "h6c16bd296c16bc9859e266b159e26620"), 
                      vnclipu.copy(vsew=0, narrow=true, vl=16, vlmul=0, vxrm=1, uopIdx=0)), //vssrl.copy(u8, u16, u8, vl=16, vlmul=0, vxrm=1, uopIdx=0)),
          genVFuInput(SrcBundle("hd950c3f9d950c368c71c6d81c71c6cf0", "h907f6a19907f69887e4b13a17e4b1310", "h6c16bd296c16bc980303ffff0318ffff"), 
                      vnclipu.copy(vsew=0, narrow=true, vl=16, vlmul=0, vxrm=1, uopIdx=1)), //vssrl.copy(u8, u16, u8, vl=16, vlmul=0, vxrm=1, uopIdx=1)),
          genVFuInput(SrcBundle("ha9875fc3a9875f089753094b97530890", "h851eb2d3851eb21872ea5c5b72ea5ba0", "h60b605e360b605284e81af6b4e81aeb0"), 
                      vnclip.copy(vsew=1, narrow=true, vl=8, vlmul=0, vxrm=3, uopIdx=0)), //vssra.copy(s16, s32, s16, vl=8, vlmul=0, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("hcdf00cb3cdf00bf8bbbbb63bbbbbb580", "h851eb2d3851eb21872ea5c5b72ea5ba0", "h60b605e360b605288000fff580008000"), 
                      vnclip.copy(vsew=1, narrow=true, vl=8, vlmul=0, vxrm=3, uopIdx=1)), //vssra.copy(s16, s32, s16, vl=8, vlmul=0, vxrm=3, uopIdx=1)),
          genVFuInput(SrcBundle("hd159ddf0d159dd58bf258778bf2586e0", "hacf13100acf130689abcda889abcd9f0", "h888884108888837876542d9876542d00"), 
                      vnclip.copy(vsew=0, narrow=true, vl=16, vlmul=0, vxrm=2, uopIdx=0)), //vssra.copy(s8, s16, s8, vl=16, vlmul=0, vxrm=2, uopIdx=0)),
          genVFuInput(SrcBundle("hf5c28ae0f5c28a48e38e3468e38e33d0", "hacf13100acf130689abcda889abcd9f0", "h8888841088888378f4fdf4ddeff8df80"), 
                      vnclip.copy(vsew=0, narrow=true, vl=16, vlmul=0, vxrm=2, uopIdx=1)), //vssra.copy(s8, s16, s8, vl=16, vlmul=0, vxrm=2, uopIdx=1)),

        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // // mask-reg logical
          // genVAluOutput("hffffffffffffffffffffffffffffffc0"), //vmand.mm 395
          // genVAluOutput("hfffffffffffffffffffffffffffffdbf"), //vmnand.mm 815
          // genVAluOutput("hffffffffffffffffffffffffa0641a60"), //vmandn.mm 1235
          // genVAluOutput("hfffffffffffffffffffffffff8641a60"), //vmandn.mm 1235
          // genVAluOutput("hffffffffffffffff72debc4372debcc0"), //vmxor.mm 1715
          // genVAluOutput("hffffffffffffffffffffefa75ddf6f80"), //vmor.mm 2387
          // genVAluOutput("h10306514103064170229100c0229140f"), //vmnor.mm 2207
          // genVAluOutput("hfeffff7f92fffdfffefb53fffefb577f"), //vmorn.mm 2279
          // genVAluOutput("hfffffffffffffffffffffffffff5647f"), //vmxnor.mm 2507
          // fixed-point
          genVAluOutput("h7a4fa4657a4fa440e89bcdece89bcdc8"), //vaaddu 844
          genVAluOutput("h9be02327d27da680c0ab50af89abcca0"), //vaaddu 909
          genVAluOutput("hffffffffffffffffb2a18efbb2a18ec8"), //vaaddu 1182
          genVAluOutput("haaaa296aaaaa29409876d2f29876d2c8"), //vaadd 974
          genVAluOutput("hc962fb2fc962fb00b72ea4b7b72ea488"), //vaadd 1091
          genVAluOutput("h1235d6781235d6789234567992345678"), //vasubu 961
          genVAluOutput("h12345678123456781234567812345678"), //vasubu 1091
          genVAluOutput("h12b4d7f912b4d7f81335d7f81335d7f8"), //vasub 844
          genVAluOutput("h92345678123456781234567812345678"), //vasub 1221
          genVAluOutput("h0f02051b0f0205b83605000c36050040"), //vssrl 818
          genVAluOutput("h02f33ff902f301005555002a5555a940"), //vssrl 974
          genVAluOutput("hffffffb7ffdb975100000000ffffc963"), //vssra 1091
          genVAluOutput("hfffa4fa36cfa4fa3ffffe81b4cf4e81b"), //vssra 1208
          genVAluOutput("hfff6e518fff6e4ffe2ffffffe2ffffff", vxsat=true), //vsaddu 307
          genVAluOutput("ha1907f5aa1907f588f5c28e28f5c2890", vxsat=false), //vsadd 69
          genVAluOutput("h00000000000000000000000000000000", vxsat=true), //vssubu 1525
          genVAluOutput("h2468acf02468acf02468acf02468acf0", vxsat=false), //vssubu 1539
          genVAluOutput("h2469acf02469acf02468acf02468acf0", vxsat=false), //vssub 951
          genVAluOutput("h1eb84f6d1eb84f187fffffffffffffff", vxsat=true), //vsadd 2099
          genVAluOutput("h81b4e5ce81b4e5805d4bffff5d4bffff", vxsat=true), //vsaddu 965
          genVAluOutput("h00000000000000002468acf02468acf0", vxsat=true), //vsaddu 1623
          genVAluOutput("h2468acf02468acf08000000080000000", vxsat=true), //vsadd 1539
          genVAluOutput("hf6e65408f6e65400e431fe8fe431fe88"), //vaadd 195
          genVAluOutput("hd709bc63d709bc40c4d565ebc4d565c8"), //vaaddu 881
          genVAluOutput("h12345678123456781234567812345678"), //vasub 1567
          genVAluOutput("h13b5d7f813b5d7f893b5d7f993b5d7f8"), //vasubu 223
          genVAluOutput("h010d0100010d01f83e0702013e070280"), //vssrl 195
          genVAluOutput("h000e30e0000e003000c800da00c8da40"), //vssrl 993
          genVAluOutput("h00056789005678990000044500004445"), //vssrl 1679
          genVAluOutput("h00a4fa4cd7a4fa4d000092c5f65f92c5"), //vssrl 2351
          genVAluOutput("hfd0706fdfd070678e940ff01e940ff00"), //vssra 335
          genVAluOutput("h007b0005007b0047068bff03068bf000"), //vssra 895
          genVAluOutput("h003f258a007e4b150000360b00006c16"), //vssra 1609
          genVAluOutput("h003b2a16543b2a16000028f5bfdc28f6"), //vssra 2267
          genVAluOutput("h8e38de4b8e38dd98ffffffffffffc4d6", vxsat=true), //vnclipu 4213
          genVAluOutput("hffff001fffffffffffffffffffffc4d6", vxsat=true), //vnclipu 4213
          genVAluOutput("h6c16bd296c16bc980303ffff0318ffff", vxsat=true), //vnclipu 3541
          genVAluOutput("hff023662ff01646d0303ffff0318ffff", vxsat=true), //vnclipu 3541
          genVAluOutput("h60b605e360b605288000fff580008000", vxsat=true), //vnclip 4367
          genVAluOutput("hfffff9bfffffffbb8000fff580008000", vxsat=false), //vnclip 4367
          genVAluOutput("h8888841088888378f4fdf4ddeff8df80", vxsat=true), //vnclip 3681
          genVAluOutput("hff808080fe7f8033f4fdf4ddeff8df80", vxsat=true), //vnclip 3681
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

  def vAluTest5(): Unit = {
    it should "pass the test: tail, prestart, mask" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vadd, vssra: tail, prestart, mask
          genVFuInput(SrcBundle("h2fc961de2fc961b81d950b661d950b40", "h0b60b4ee0b60b4c8f92c5e75f92c5e50",
                                 "he6f807fde6f807d8d4c3b185d4c3b160", "hc28f5b0dc28f5ae8b05b0495b05b0470"),
                      vadd.copy(vsew=0, vl=8, vm=false, ma=false, ta=false)), //vadd.copy(s8, s8, s8, vl=8, vm=false, ma=false, ta=false)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=11, vm=false, ma=false, ta=false)), //vadd.copy(s8, s8, s8, vl=11, vm=false, ma=false, ta=false)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=14, vm=false, ma=false, ta=false)), //vadd.copy(s8, s8, s8, vl=14, vm=false, ma=false, ta=false)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=14, vm=false, ma=true, ta=true)), //vadd.copy(s8, s8, s8, vl=14, vm=false, ma=true, ta=true)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=14, vm=false, ma=true, ta=true, vstart=5)), //vadd.copy(s8, s8, s8, vl=14, vm=false, ma=true, ta=true, vstart=5)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=14, vm=false, ma=true, ta=true, vstart=14)), //vadd.copy(s8, s8, s8, vl=14, vm=false, ma=true, ta=true, vstart=14)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=61, vm=false, ma=true, ta=true, vstart=34, uopIdx = 3)), //vadd.copy(s8, s8, s8, vl=61, vm=false, ma=true, ta=true, vstart=34, uopIdx = 3)),
          genVFuInput(SrcBundle("he4b17d1fe4b17cf8d27d26a7d27d2680", "hc048d02fc048d008ae1479b7ae147990",
                                 "h9be0233f9be0231889abccc789abcca0", "h7777764f7777762865431fd765431fb0"),
                      vadd.copy(vsew=0, vl=61, vm=false, ma=true, ta=true, vstart=57, uopIdx = 3)), //vadd.copy(s8, s8, s8, vl=61, vm=false, ma=true, ta=true, vstart=57, uopIdx = 3)),
          genVFuInput(SrcBundle("he0246763e0246738cdf010ebcdf010c0", "hbbbbba73bbbbba48a98763fba98763d0",
                                 "h97530d8397530d58851eb70b851eb6e0", "h72ea609372ea606860b60a1b60b609f0"),
                      vadd.copy(vsew=1, vl=4, vm=false, ma=false, ta=false, vstart=0, uopIdx = 0)), //vadd.copy(s16, s16, s16, vl=4, vm=false, ma=false, ta=false, vstart=0, uopIdx = 0)),
          genVFuInput(SrcBundle("h71c71b2471c71af85f92c4ac5f92c480", "h4d5e6e344d5e6e083b2a17bc3b2a1790",
                                 "h28f5c14428f5c11816c16acc16c16aa0", "h048d1454048d1428b058bddbf258bdb0"),
                      vadd.copy(vsew=1, vl=64, vm=false, ma=false, ta=false, vstart=56, uopIdx=7)), //vadd.copy(s16, s16, s16, vl=64, vm=false, ma=false, ta=false, vstart=56, uopIdx=7)),
          genVFuInput(SrcBundle("ha61d9336a61d92f893e93cbe93e93c80", "h81b4e64681b4e6086f808fce6f808f90",
                                 "h5d4c39565d4c39184b17e2de4b17e2a0", "h38e38c6638e38c2826af35ee26af35b0"),
                      vssra.copy(vsew=2, vl=27, vm=true, ma=false, ta=true, vstart=25, uopIdx=6)), //vssra.copy(s32, s32, s32, vl=27, vm=true, ma=false, ta=true, vstart=25, uopIdx=6)),
          genVFuInput(SrcBundle("ha61d9336a61d92f893e93cbe93e93c80", "h81b4e64681b4e6086f808fce6f808f90",
                                 "h5d4c39565d4c39184b17e2de4b17e2a0", "h38e38c6638e38c2826af35ee26af35b0"),
                      vssra.copy(vsew=2, vl=9, vm=false, ma=true, ta=true, vstart=4, uopIdx=1)), //vssra.copy(s32, s32, s32, vl=9, vm=false, ma=true, ta=true, vstart=4, uopIdx=1)),
          genVFuInput(SrcBundle("h8641fb128641fab8740da49a740da440", "h61d94e2261d94dc84fa4f7aa4fa4f750",
                                 "h3d70a1323d70a0d82b3c4aba2b3c4a60", "h1907f4421907f3e806d39dca06d39d70"),
                      vssra.copy(vsew=3, vl=11, vm=true, ma=true, ta=false, vstart=4, uopIdx=5, vxrm=2)), //vssra.copy(s64, s64, s64, vl=11, vm=true, ma=true, ta=false, vstart=4, uopIdx=5, vxrm=2)),
          genVFuInput(SrcBundle("h8641fb128641fab8740da49a740da440", "h61d94e2261d94dc84fa4f7aa4fa4f750",
                                 "h3d70a1323d70a0d82b3c4aba2b3c4a60", "h1907f4421907f3e806d39dca06d39d70"),
                      vssra.copy(vsew=3, vl=16, vm=false, ma=false, ta=false, vstart=4, uopIdx=7, vxrm=2)), //vssra.copy(s64, s64, s64, vl=16, vm=false, ma=false, ta=false, vstart=4, uopIdx=7, vxrm=2)),
          // widen
          genVFuInput(SrcBundle("h72ea609372ea606860b60a1b60b609f0", "h4e81b3a34e81b3783c4d5d2b3c4d5d00",
                                 "h05b059c305b05998f37c034af37c0320", "he147acd2e147aca8cf13565acf135630"),
                      vwaddu.copy(vsew=1, widen=true, vl=2, vm=true, ma=false, ta=false, vstart=0, uopIdx=0)), //vadd.copy(u32, u16, u16, vl=2, vm=true, ma=false, ta=false, vstart=0, uopIdx=0)),
          genVFuInput(SrcBundle("h72ea609372ea606860b60a1b60b609f0", "h4e81b3a34e81b3783c4d5d2b3c4d5d00",
                                 "h2a1906b32a19068817e4b03b17e4b010", "he147acd2e147aca8cf13565acf135630"),
                      vwaddu.copy(vsew=1, widen=true, vl=8, vm=false, ma=false, ta=false, vstart=0, uopIdx=1)), //vadd.copy(u32, u16, u16, vl=8, vm=false, ma=false, ta=false, vstart=0, uopIdx=1)),
          genVFuInput(SrcBundle("h9f49f38c9f49f3688d159d148d159cf0", "h7ae1469c7ae1467868acf02468acf000",
                                 "h320fecbc320fec981fdb96441fdb9620", "h0da73fcc0da73fa8fb72e953fb72e930"),
                      vwadd.copy(vsew=0, widen2=true, vl=54, vm=false, ma=false, ta=true, vstart=0, uopIdx=6)), //vadd.copy(s16, s16, s8, vl=54, vm=false, ma=false, ta=true, vstart=0, uopIdx=6)),
          genVFuInput(SrcBundle("hc3b2a07cc3b2a058b17e4a04b17e49e0", "h7ae1469c7ae1467868acf02468acf000",
                                 "h320fecbc320fec981fdb96441fdb9620", "h0da73fcc0da73fa8fb72e953fb72e930"),
                      vwadd.copy(vsew=0, widen2=true, vl=64, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)), //vadd.copy(s16, s16, s8, vl=64, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)),
          // // compare
          // genVFuInput(SrcBundle("hf6e5cb9ff6e5ca68e4b17527e4b173f0", "hd3a0641ed3a062e8c16c0da6c16c0c70",
          //                        "hb05afc9db05afb689e26a6259e26a4f0", "h8bf24fad8bf24e7879bdf93579bdf800"),
          //             vmslt.copy(vsew=3, narrow_to_1=true, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=0)), //vmslt.copy(mask, s64, s64, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=0)),
          // genVFuInput(SrcBundle("h641fd270641fd13851eb7bf851eb7ac0", "h40da6aef40da69b82ea614772ea61340",
          //                        "hffffffffffffffffffffffffffffa4f0", "h8bf24fad8bf24e7879bdf93579bdf800"),
          //             vmslt.copy(vsew=3, narrow_to_1=true, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=3)), //vmslt.copy(mask, s64, s64, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=3)),
          // genVFuInput(SrcBundle("hf5c28630f5c284f8e38e2fb8e38e2e80", "hd27d1eafd27d1d78c048c837c048c700",
          //                        "hffffffffffffffffffffffffffff84f0", "h8bf24fad8bf24e7879bdf93579bdf800"),
          //             vmslt.copy(vsew=3, narrow_to_1=true, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=7)), //vmslt.copy(mask, s64, s64, vl=16, vm=false, ma=false, ta=false, vstart=0, uopIdx=7)),
          // genVFuInput(SrcBundle("h16c1687416c167f8048d11fc048d1180", "h851eb4b3851eb43872ea5e3b72ea5dc0",
          //                        "hf37c00f2f37c0078e147aa7ae147aa00", "hcf135402cf135388bcdefd8abcdefd10"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=0)), //vmslt.copy(mask, s16, s16, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=0)),
          // genVFuInput(SrcBundle("h3b2a15643b2a14e828f5beec28f5be70", "ha98761a3a987612897530b2b97530ab0",
          //                        "hffffffffffffffffffffffffe147aa00", "hcf135402cf135388bcdefd8abcdefd10"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=1)), //vmslt.copy(mask, s16, s16, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=1)),
          // genVFuInput(SrcBundle("h5f92c2545f92c1d84d5e6bdc4d5e6b60", "hcdf00e93cdf00e18bbbbb81bbbbbb7a0",
          //                        "hffffffffffffffffffffffffe1475700", "hcf135402cf135388bcdefd8abcdefd10"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=2)), //vmslt.copy(mask, s16, s16, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=2)),
          // genVFuInput(SrcBundle("h83fb6f4483fb6ec871c718cc71c71850", "hf258bb83f258bb08e024650be0246490",
          //                        "hffffffffffffffffffffffffe1515700", "hcf135402cf135388bcdefd8abcdefd10"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=3)), //vmslt.copy(mask, s16, s16, vl=32, vm=false, ma=false, ta=true, vstart=0, uopIdx=3)),
          // genVFuInput(SrcBundle("h16c1687416c167f8048d11fc048d1180", "h851eb4b3851eb43872ea5e3b72ea5dc0",
          //                        "hf37c00f2f37c0078e147aa7ae141aa06", "hcf135402cf135388bcdefd8abcdefd5f"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=19, vm=false, ma=true, ta=false, vstart=4, uopIdx=0)), //vmslt.copy(mask, s16, s16, vl=19, vm=false, ma=true, ta=false, vstart=4, uopIdx=0)),
          // genVFuInput(SrcBundle("h3b2a15643b2a14e828f5beec28f5be70", "ha98761a3a987612897530b2b97530ab0",
          //                        "hfffffffffffffffffffffffffff9aaa6", "hcf135402cf135388bcdefd8abcde695f"),
          //             vmslt.copy(vsew=1, narrow_to_1=true, vl=19, vm=false, ma=true, ta=false, vstart=4, uopIdx=1)), //vmslt.copy(mask, s16, s16, vl=19, vm=false, ma=true, ta=false, vstart=4, uopIdx=1)),
          // narrow
          genVFuInput(SrcBundle("h0b60ad6e0b60ac48f92c56f5f92c55d0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454320f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=7, vm=false, ma=false, ta=true, vstart=1, uopIdx=0)), //vssra.copy(s16, s32, s16, vl=7, vm=false, ma=false, ta=true, vstart=1, uopIdx=0)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789005b6789a210", "h55554cbd55554b984320f6454320f565"),
                      vnclip.copy(vsew=1, narrow=true, vl=7, vm=false, ma=false, ta=true, vstart=1, uopIdx=1)), //vssra.copy(s16, s32, s16, vl=7, vm=false, ma=false, ta=true, vstart=1, uopIdx=1)),
          genVFuInput(SrcBundle("h0b60ad6e0b60ac48f92c56f5f92c55d0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454320f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=7, vm=false, ma=false, ta=true, vstart=5, uopIdx=0)), //vssra.copy(s16, s32, s16, vl=7, vm=false, ma=false, ta=true, vstart=5, uopIdx=0)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "hfffff9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454320f565"),
                      vnclip.copy(vsew=1, narrow=true, vl=7, vm=false, ma=false, ta=true, vstart=5, uopIdx=1)), //vssra.copy(s16, s32, s16, vl=7, vm=false, ma=false, ta=true, vstart=5, uopIdx=1)),
          genVFuInput(SrcBundle("h0b60ad6e0b60ac48f92c56f5f92c55d0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454325f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=23, vm=false, ma=false, ta=true, vstart=21, uopIdx=4)), //vssra.copy(s16, s32, s16, vl=23, vm=false, ma=false, ta=true, vstart=21, uopIdx=4)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "hfffff9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454365f565"),
                      vnclip.copy(vsew=1, narrow=true, vl=23, vm=false, ma=false, ta=true, vstart=21, uopIdx=5)), //vssra.copy(s16, s32, s16, vl=23, vm=false, ma=false, ta=true, vstart=21, uopIdx=5)),
          genVFuInput(SrcBundle("h0b60ad6e0b60ac48f92c56f5f92c55d0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f6454f20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=6)), //vssra.copy(s16, s32, s16, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=6)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8880003005bfffef92c", "h55554cbd55554b984320f645af20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)), //vssra.copy(s16, s32, s16, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8880003005bfffef92c", "h55554cbd55554b984320f645ef20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)), //vssra.copy(s16, s32, s16, vl=32, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8880003005bfffef92c", "h55554cbd55554b984320f645ef20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=30, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)), //vssra.copy(s16, s32, s16, vl=30, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8880003005bfffef92c", "h55554cbd55554b984320f645ff20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=30, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)), //vssra.copy(s16, s32, s16, vl=30, vm=false, ma=true, ta=true, vstart=0, uopIdx=7)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "h79bdf9ad79bdf8886789a3356789a210", "h55554cbd55554b984320f645ff20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=30, vm=false, ma=true, ta=true, vstart=29, uopIdx=6)), //vssra.copy(s16, s32, s16, vl=30, vm=false, ma=true, ta=true, vstart=29, uopIdx=6)),
          genVFuInput(SrcBundle("h2fc95a5e2fc959381d9503e61d9502c0", "hc28f538dc28f5268b05afd15b05afbf0",
                                 "hffffffff79bdf8886789a3356789a210", "h55554cbd55554b984320f645ff20f525"),
                      vnclip.copy(vsew=1, narrow=true, vl=30, vm=false, ma=true, ta=true, vstart=29, uopIdx=7)), //vssra.copy(s16, s32, s16, vl=30, vm=false, ma=true, ta=true, vstart=29, uopIdx=7)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vadd, vssra: tail, prestart, mask
          genVAluOutput("he6f807fde6f807d8d4c169dbd4c3b160"), //vadd 870
          genVAluOutput("h9be0233f9bf94c0080ab9f5e89abcca0"), //vadd 909
          genVAluOutput("h9be0234ea4f94c0080ab9f5e89abcca0"), //vadd 909
          genVAluOutput("hffffff4ea4f94c0080ff9f5effffffff"), //vadd 909
          genVAluOutput("hffffff4ea4f94c0080ff9fc789abcca0"), //vadd 909
          genVAluOutput("h9be0233f9be0231889abccc789abcca0"), //vadd 909
          genVAluOutput("hfffffffffff9ff00ff91ffffffff9f10"), //vadd 909
          genVAluOutput("hfffffffffff9ff1889abccc789abcca0"), //vadd 909
          genVAluOutput("h97530d8397530d58851eb70b851eb6e0"), //vadd 1000
          genVAluOutput("hbf25c144bf25890016c16acc16c16aa0"), //vadd 1013 mask change
          genVAluOutput("hffffffffffa61d93fffe4fa54b17e2a0"), //vssra 1539
          genVAluOutput("hfe98764dfffffffffffe4fa5ffff93e9"), //vssra 1539
          genVAluOutput("h3d70a1323d70a0d80000740da49a740d"), //vssra 2225
          genVAluOutput("hff8641fb128641fa2b3c4aba2b3c4a60"), //vssra 2225
          // widen
          genVAluOutput("h05b059c305b0599800009d03000066f0"), //vwaddu 948
          genVAluOutput("h2a1906b32a1906880000c16b000113e0"), //vwaddu 948
          genVAluOutput("hffffffff9f39f38c1fdb96448d059620"), //vwadd.wv 818
          genVAluOutput("hc42ca05dc3f89ff4b1f8ffffb1c44a58"), //vwadd.wv 818
          // compare
          // genVAluOutput("hffffffffffffffffffffffffffffa4f0"), //vmslt 2859
          // genVAluOutput("hffffffffffffffffffffffffffffa4f0"), //vmslt 2859
          // genVAluOutput("hffffffffffffffffffffffffffff04f0"), //vmslt 2859
          // genVAluOutput("hffffffffffffffffffffffffe147aa00"), //vmslt 2014
          // genVAluOutput("hffffffffffffffffffffffffe1475700"), //vmslt 2014
          // genVAluOutput("hffffffffffffffffffffffffe1515700"), //vmslt 2014
          // genVAluOutput("hffffffffffffffffffffffffe5515700"), //vmslt 2014
          // genVAluOutput("hfffffffffffffffffffffffffff9aaa6"), //vmslt 2014 some change
          // genVAluOutput("hfffffffffffffffffffffffffff9d7a6"), //vmslt 2014 some change
          // narrow
          genVAluOutput("hfffff9ad79bdf8886789005b6789a210"), //vnclip.wv 5683 some change
          genVAluOutput("hffff7fff3b2af8886789005b6789a210", vxsat=true), //vnclip.wv 5683 some change
          genVAluOutput("hfffff9ad79bdf8886789a3356789a210"), //vnclip.wv 5683 some change
          genVAluOutput("hffff7fff3b2af8886789a3356789a210", vxsat=true), //vnclip.wv 5683 some change
          genVAluOutput("hfffff9ad79bdf8886789a3356789a210"), //vnclip.wv 5683 some change
          genVAluOutput("hffff7fff3b2af8886789a3356789a210", vxsat=true), //vnclip.wv 5683 some change
          genVAluOutput("hfffff9adffffffff0003005bfffef92c", vxsat=false), //vnclip.wv 5683 some change
          genVAluOutput("h5f93ffff3b2affff0003005bfffef92c", vxsat=false), //vnclip.wv 5683 some change
          genVAluOutput("h5f937fff3b2affff0003005bfffef92c", vxsat=true), //vnclip.wv 5683 some change
          genVAluOutput("hffffffff3b2affff0003005bfffef92c", vxsat=false), //vnclip.wv 5683 some change
          genVAluOutput("hffffffff3b2a7fff0003005bfffef92c", vxsat=true), //vnclip.wv 5683 some change
          genVAluOutput("hffffffff79bdf8886789a3356789a210", vxsat=false), //vnclip.wv 5683 some change
          genVAluOutput("hffffffff3b2af8886789a3356789a210", vxsat=false), //vnclip.wv 5683 some change
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

  def vAluTest6(): Unit = {
    it should "pass the test: lmul < 1" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vsub: lmul < 1
          genVFuInput(SrcBundle("h3579bdf93579bdf82345678123456780", "h1111110911111108fedcba90fedcba90",
                                 "heca86418eca86418da740da0da740da0"),
                      vsub.copy(vsew=0, vl=2, ta=false, vlmul=5)), //vsub.copy(s8, s8, s8, vl=2, ta=false, vlmul=5)),
          genVFuInput(SrcBundle("h048d1544048d1538f258becbf258bec0", "he0246853e0246848cdf011dbcdf011d0",
                                 "hbbbbbb63bbbbbb58a98764eba98764e0"),
                      vsub.copy(vsew=1, vl=1, ta=false, vlmul=6)), //vsub.copy(s16, s16, s16, vl=1, ta=false, vlmul=6)),
          genVFuInput(SrcBundle("h962fc904962fc8f883fb728c83fb7280", "h71c71c1471c71c085f92c59c5f92c590",
                                 "h4d5e6f244d5e6f183b2a18ac3b2a18a0"),
                      vsub.copy(vsew=1, vl=2, ta=false, vlmul=6)), //vsub.copy(s16, s16, s16, vl=2, ta=false, vlmul=6)),
          genVFuInput(SrcBundle("hca864116ca8640f8b851ea9eb851ea80", "ha61d9426a61d940893e93dae93e93d90",
                                 "h81b4e73681b4e7186f8090be6f8090a0"),
                      vsub.copy(vsew=2, vl=2, ta=false, vlmul=7)), //vsub.copy(s32, s32, s32, vl=2, ta=false, vlmul=7)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vsub: lmul < 1
          genVAluOutput("heca86418eca86418da740da0da74adf0"), //vsub 25
          genVAluOutput("hbbbbbb63bbbbbb58a98764eba987acf0"), //vsub 272
          genVAluOutput("h4d5e6f244d5e6f183b2a18ac2469acf0"), //vsub 285
          genVAluOutput("h81b4e73681b4e7182468acf02468acf0"), //vsub 701
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

  def vAluTest7(): Unit = {
    it should "pass the test: vmv.x.s(s.x), vfmv.f.s(s.f), vmv<nr>r" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // 16.1 16.2
          genVFuInput(SrcBundle("hb851ea9eb851ea82", "h0"), 
                      vmvxs.copy(vsew=1, vl=4)),
          genVFuInput(SrcBundle("hb851ea9eb8515a82", "h0"), 
                      vmvxs.copy(vsew=1, vl=7)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea80"), 
                      vmvsx.copy(vsew=2, vl=4)), //vmvsx.copy(u32, u32, u32, vl=4)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea80", old_vd="h1122"), 
                      vmvsx.copy(vsew=2, vl=4, vstart=5)), //vmvsx.copy(u32, u32, u32, vl=4, vstart=5)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea80", old_vd="h1122"), 
                      vmvsx.copy(vsew=2, vl=0)), //vmvsx.copy(u32, u32, u32, vl=0)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea80", old_vd="h5566"), 
                      vmvsx.copy(vsew=2, vl=3, vstart=1)), //vmvsx.copy(u32, u32, u32, vl=3, vstart=1)),
          genVFuInput(SrcBundle("hb851ea9eb851ea82", "h0"), 
                      vfmvfs.copy(vsew=2, vl=5)),
          genVFuInput(SrcBundle("hb851ea9eb851ea82", "h0"), 
                      vfmvfs.copy(vsew=3, vl=5)),
          genVFuInput(SrcBundle("hb851ea9eb851ea82", "h0", old_vd="h1122"), 
                      vfmvfs.copy(vsew=2, vl=0)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea82", old_vd="h1122"), 
                      vfmvsf.copy(vsew=2, vl=2)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea82", old_vd="h1122"), 
                      vfmvsf.copy(vsew=2, vl=0)),
          genVFuInput(SrcBundle("h0", rs1="hb851ea9eb851ea82", old_vd="h1122"), 
                      vfmvsf.copy(vsew=3, vl=8)),
          // 16.6
          genVFuInput(SrcBundle("h3579bdf93579bdf82345678123456780", "h1"), 
                      vmvnrr.copy(vsew=1, vs1_imm=1, uopIdx=1, vl=4, vstart=4)),
          genVFuInput(SrcBundle("h3579bdf93579bdf82345678123456780", "h1", old_vd="h1122"), 
                      vmvnrr.copy(vsew=1, vs1_imm=1, uopIdx=1, vl=4, vstart=16)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // 16.1 16.2
          genVAluOutput("hffffffffffffea82"), //vmvxs handmade
          genVAluOutput("h5a82"), //vmvxs handmade
          genVAluOutput("hffffffffffffffffffffffffb851ea80"), //vmvsx handmade
          genVAluOutput("h1122"), //vmvsx handmade
          genVAluOutput("h1122"), //vmvsx handmade
          genVAluOutput("hffffffffffffffffffffffff00005566"), //vmvsx handmade
          genVAluOutput("hb851ea82"), //vfmvfs handmade
          genVAluOutput("hb851ea9eb851ea82"), //vfmvfs handmade
          genVAluOutput("hb851ea82"), //vfmvfs handmade
          genVAluOutput("hffffffffffffffffffffffffb851ea82"), //vfmvsf handmade
          genVAluOutput("h1122"), //vfmvsf handmade
          genVAluOutput("hffffffffffffffffb851ea9eb851ea82"), //vfmvsf handmade
          genVAluOutput("h3579bdf93579bdf82345678123456780"), //vmv<nr>r handmade
          genVAluOutput("h1122"), //vmv<nr>r handmade
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

  def vAluTest8(): Unit = {
    it should "pass the test: compare, vmadc/vmsbc" in {
      test(new VAluWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessAlu.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vec compare (& tail test)
          genVFuInput(SrcBundle("h6e5d4bc76e5d4bb85c28f54f5c28f540", "h49f49ed749f49ec837c0485f37c04850"), 
                       vmseq.copy(vsew=0, narrow_to_1=true, vl=16, uopIdx=0, uopEnd=true)), //vmseq.copy(mask, s8, s8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("h83fb712483fb70e871c71aac71c71a70", "hf258bd63f258bd28e02466ebe02466b0", "h60b609a360b609684e81b32b4e81b2f0"), 
            vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=0)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=0)),
          genVFuInput(SrcBundle("ha8641e14a8641dd8962fc79c962fc760", "h16c16a5416c16a18048d13dc048d13a0", "hfffffffffffffffffffffffffffffaff"), 
            vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=1)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=1)),
          genVFuInput(SrcBundle("hcccccb04cccccac8ba98748cba987450", "h3b2a17443b2a170828f5c0cc28f5c090", "hfffffffffffffffffffffffffffffa0f"), 
            vmsltu.copy(vsew=2, narrow_to_1=true, vl=11, vlmul=2, ta=true, uopIdx=2, uopEnd=true)), //vmslt.copy(mask, u32, u32, vl=11, vlmul=2, ta=true, uopIdx=2)),
          genVFuInput(SrcBundle("haaaaa9e2aaaaa9c89876536a98765350", "h61d9500261d94fe84fa4f98a4fa4f970", "h1907f6221907f60806d39faa06d39f90"), 
            vmsleu.copy(vsew=0, narrow_to_1=true, vl=22, vlmul=1, ta=false, uopIdx=0)), //vmsle.copy(mask, u8, u8, vl=18, vlmul=1, ta=false, uopIdx=0)),
          genVFuInput(SrcBundle("hcf1356d2cf1356b8bcdf005abcdf0040", "h8641fcf28641fcd8740da67a740da660", "h1907f6221907f60806d39faa06d34577"), 
            vmsleu.copy(vsew=0, narrow_to_1=true, vl=22, vlmul=1, ta=false, uopIdx=1, uopEnd=true)), //vmsle.copy(mask, u8, u8, vl=22, vlmul=1, ta=false, uopIdx=1)),
          // vmadc
          genVFuInput(SrcBundle("hffffffffffffffffffffffffffffffff", rs1="h0", mask="h00eb3d30f587dbacd8856e1aa5936ff1", old_vd="h00ff55aa75cd39e1f05a7a1e0fdca593"), 
            vmadc_vxm.copy(vsew=2, narrow_to_1=true, vl=2, vlmul=7, ta=false, vm=false, uopIdx=0, uopEnd=true, vstart=1)), //vmsle.copy(mask, u8, u8, vl=22, vlmul=1, ta=false, uopIdx=1)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vec compare
          genVAluOutput("hffffffffffffffffffffffffffff0000"), //vmseq 350
          genVAluOutput("hfffffffffffffffffffffffffffff80f"), //vmsltu 883
          genVAluOutput("hfffffffffffffffffffffffffff34577"), //vmsleu 571
          // vmadc
          genVAluOutput("hfffffffffffffffffffffffffffffffd"),
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

  def vAluTest10(): Unit = {
    it should "pass the test: vmv.x.s(s.x)" in {
      test(new SmartVectorWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessSmart.test_init(dut)

        val Smartvmvxs = VInfoBundle()

        //----- Input gen -----
        val inputSeq = Seq(
          // 16.1 16.2
          
          //vmv.vi
          genRVUissue("b01011110000000001011000101010111".U, Smartvmvxs),
          //genRVUissue("b01011110000000010011001101010111".U, Smartvmvxs),
          //vmv.vv
          //genRVUissue("b01011110000000110100000011010111".U, Smartvmvxs),

          
          //vadd.v.v has no dependency with vmv.v.v
          //genRVUissue("b00000010101001111000011101010111".U, Smartvmvxs),

          //vadd.v.v has dependency with vmv.v.v
          //genRVUissue("b00000010001000110000011101010111".U, Smartvmvxs),

          //vsub.v.v
          //genRVUissue("b00001010001000110000011101010111".U, Smartvmvxs),

          //vmul.v.v
          //genRVUissue("b10010110001000110010011111010111".U, Smartvmvxs),
          
          
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // 16.1 16.2
          genRVUTestResult("h08080808"), //vmvxs handmade
          //genVAluOutput("h5a82"), //vmvxs handmade
        )

        fork{
          dut.io.in.enqueueSeq(inputSeq)
        }.fork {
          // dut.io.out.expectDequeueSeq(outputSeq)
        }.join()
        dut.clock.step(10)
        dut.clock.setTimeout(1)
      }
    }
  }

}

class SmartSpec extends AnyFlatSpec with ChiselScalatestTester with SBundleGenHelper with SmartBehavior {
  behavior of "ALU test"
  //it should behave like vAluTest0()  // add/sub, and/or/xor, sll/srl/sra
  //it should behave like vAluTest1()  // min/max(u), vwadd/vwsub(u)
  //it should behave like vAluTest2()  // vz(s)ext, vmerge, vadc/vsbc, vmv
  //it should behave like vAluTest3()  // vnsrl(a), compare, vmadc/vmsbc
  //it should behave like vAluTest4()  // mask-reg logical, fixed-point
  //it should behave like vAluTest5()  // mask/tail/prestart
  //it should behave like vAluTest6()  // lmul < 1
  //it should behave like vAluTest7()  // 16.1 16.2 16.6
  //it should behave like vAluTest8()  // New compare/vmadc test for nanhu_v3
  it should behave like vAluTest10()
}