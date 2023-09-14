package darecreek.vfutest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util._
import chiseltest.WriteVcdAnnotation
import darecreek.exu.vfu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._

/** @note VMac has the same IO ports as VAlu */
class VMacWrapper extends Module {
  implicit val p = Parameters.empty.alterPartial({case VFuParamsKey => VFuParameters()
                                                  case XSCoreParamsKey => XSCoreParameters()})
  
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VFuInput))
    val out = Decoupled(new VAluOutput)
  })

  val vMac = Module(new VMac)
  vMac.io.in.bits := io.in.bits
  io.out.bits := vMac.io.out.bits

  vMac.io.in.valid := io.in.valid
  io.out.valid := vMac.io.out.valid
  io.in.ready := io.out.ready
}

trait VMacBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  val vmul = CtrlBundle(VMUL_VV)
  val vmulh = CtrlBundle(VMULH_VV)
  val vmulhu = CtrlBundle(VMULHU_VV)
  val vmulhsu = CtrlBundle(VMULHSU_VV)
  val vwmul = CtrlBundle(VWMUL_VV)
  val vwmulu = CtrlBundle(VWMULU_VV)
  val vwmulsu = CtrlBundle(VWMULSU_VV)
  val vmacc = CtrlBundle(VMACC_VV)
  val vnmsac = CtrlBundle(VNMSAC_VV)
  val vmadd = CtrlBundle(VMADD_VV)
  val vnmsub = CtrlBundle(VNMSUB_VV)
  val vwmaccu = CtrlBundle(VWMACCU_VV)
  val vwmacc = CtrlBundle(VWMACC_VV)
  val vwmaccsu = CtrlBundle(VWMACCSU_VV)
  val vwmaccus = CtrlBundle(VWMACCUS_VX)
  val vsmul = CtrlBundle(VSMUL_VV)

  
  def vMacTest0(): Unit = {
    it should "pass the basic multiply" in {
      test(new VMacWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessMac.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // vmul
          genVFuInput(SrcBundle("ha99702acb5fc066e1ebbc1a3e5ab2901", "ha29bce578540b0a4f6d1c874207311be"),
                      vmul.copy(vsew=0, vl=16)), //vmul.copy(s8, s8, s8)),
          genVFuInput(SrcBundle("hb6ccea5811a2dff1332ece3b3e4c8ac2", "h87ced08010a9ca844b980f150cb769fd", "h1e8c733e62ae5191c3f7569d8508e89e"),
                      vmul.copy(vsew=0, vl=8, ta=false)), //vmul.copy(s8, s8, s8, vl=8, ta=false)),
          genVFuInput(SrcBundle("hb6ccea5811a2dff1332ece3b3e4c8ac2", "h87ced08010a9ca844b980f150cb769fd", "h1e8c733e62ae5191c3f7569d8508e89e"),
                      vmul.copy(vsew=0, vl=8)), //vmul.copy(s8, s8, s8, vl=8)),
          genVFuInput(SrcBundle("hdfe7ce8be2dc8a46ef01fd4dd36ab541", "ha0e8b8b086d5ec44472d250ef7a4143a"),
                      vmul.copy(vsew=1, vl=8)), //vmul.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("hd224ee34c8f84e4d140b02cc244b9ab5", "h98cc1bb8c3ae0d7326b742721ce36889", "h565e84cd903ed97b5fc46ddd2b781c69", "h48ae0702228282939ea9461c94d08831"),
                      vmul.copy(vsew=1, vl=8, vm=false)), //vmul.copy(s16, s16, s16, vl=8, vm=false)),
          genVFuInput(SrcBundle("hd224ee34c8f84e4d140b02cc244b9ab5", "h98cc1bb8c3ae0d7326b742721ce36889", "h565e84cd903ed97b5fc46ddd2b781c69", "h48ae0702228282939ea9461c94d08831"),
                      vmul.copy(vsew=1, vl=8, vm=false, vstart=5)), //vmul.copy(s16, s16, s16, vl=8, vm=false, vstart=5)),
          genVFuInput(SrcBundle("hd224ee34c8f84e4d140b02cc244b9ab5", "h98cc1bb8c3ae0d7326b742721ce36889", "h565e84cd903ed97b5fc46ddd2b781c69", "h48ae0702228282939ea9461c94d08831"),
                      vmul.copy(vsew=1, vl=8, vm=false, vstart=22)), //vmul.copy(s16, s16, s16, vl=8, vm=false, vstart=22)),
          // vmulh/vmulhu/vmulhsu
          genVFuInput(SrcBundle("h0a6cc8cba9ee1beee18c9c9f8f7135ff", "hf1e36e523b9aeb0f474656f2130bce82", "h0ea95595340690ac4c0417b7b14f53bc", "h32caeb54925a9a5f3b37bffa60ef23ee"),
                      vmulh.copy(vsew=2, vl=4, vm=false)), //vmulh.copy(s32, s32, s32, vl=4, vm=false)),
          genVFuInput(SrcBundle("hbdded0a4eacea2a62d6ba241111133b6", "h058a93adbb20daa41ceff52f6da44e51", "h0ea95595340690ac4c0417b7b14f53bc", "h32caeb54925a9a5f3b37bffa60ef23ee"),
                      vmulh.copy(vsew=3, vl=2)), //vmulh.copy(s64, s64, s64, vl=2)),
          genVFuInput(SrcBundle("hf6e61c133176a3301f900fe7e81c42a4", "he1b3ea73fb71e6f831dce7158ebfa011", "hdf2b0b9e3017c138372d29c056c7aa68", "h22dacf6bc8f13e557baae3912b40b2c8"),
                      vmulhu.copy(vsew=0, vl=14, vm=false, ma=true, vstart=2)), //vmulh.copy(u8, u8, u8, vl=14, vm=false, ma=true, vstart=2)),
          genVFuInput(SrcBundle("h31e91983fba8aa5b66f940cbde1ec2a3", "hcd519f57e36f7008004dd97ae1247e6a", "h0ea95595340690ac4c0417b7b14f53bc", "h32caeb54925a9a5f3b37bffa60ef23ee"),
                      vmulhu.copy(vsew=3, vl=2)), //vmulh.copy(u64, u64, u64, vl=2)),
          genVFuInput(SrcBundle("h50c07225c971dd003986e80a6eb8f4e0", "hbc32b85c6392ec234b7d8caf37c17e53", "hf038cee74a32b1132de33cfd821f685b", "hd6f321f416e19957cc337b61d81a7999"),
                      vmulhsu.copy(vsew=1, vl=29, vm=false, ta=false, vstart=25, uopIdx=3)), //vmulh.copy(s16, s16, u16, vl=29, vm=false, ta=false, vstart=25, uopIdx=3)),
          genVFuInput(SrcBundle("h1f1c3162091158f6c76d72d6c06b1bbb", "h0881ef5fbaf21932aca060bf82b10c58", "hf038cee74a32b1132de33cfd821f685b", "hd6f321f416e19957cc337b61d81a7999"),
                      vmulhsu.copy(vsew=0, vl=16)), //vmulh.copy(s8, s8, u8, vl=16)),
          // widen
          genVFuInput(SrcBundle("h7506671a882c38f56a9105cfa711362b", "h7171f89f06d39116418b9177515a36e5"),
                      vwmul.copy(vsew=2, widen=true, uopIdx=0)), //vmul.copy(s64, s32, s32, uopIdx=0)),
          genVFuInput(SrcBundle("h7506671a882c38f56a9105cfa711362b", "h7171f89f06d39116418b9177515a36e5"),
                      vwmul.copy(vsew=2, widen=true, uopIdx=1)), //vmul.copy(s64, s32, s32, uopIdx=1)),
          genVFuInput(SrcBundle("h670c16f883fd77a5bc8e322a5747daad", "hacb375ee75ba26596d6165daf8c43647"),
                      vwmulu.copy(vsew=0, widen=true, vl=16, uopIdx=0)), //vmul.copy(u16, u8, u8, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("h670c16f883fd77a5bc8e322a5747daad", "hacb375ee75ba26596d6165daf8c43647"),
                      vwmulu.copy(vsew=0, widen=true, vl=16, uopIdx=1)), //vmul.copy(u16, u8, u8, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("h0acc7b707568048a099adb0df95b39ae", "hc8e85b1c4d3d94ad9748042b8f1ab24a", "hae407980a1268ad6b8123ece61e2ec51", "h40f2452290756e10c7b375d8167db678"),
                      vwmulu.copy(vsew=0, widen=true, vl=16, uopIdx=0, vm=false, ta=false)), //vmul.copy(u16, u8, u8, vl=16, uopIdx=0, vm=false, ta=false)),
          genVFuInput(SrcBundle("h0acc7b707568048a099adb0df95b39ae", "hc8e85b1c4d3d94ad9748042b8f1ab24a", "h0d0f18bb772a6bdb6a46a0c37e120044", "h40f2452290756e10c7b375d8167db678"),
                      vwmulu.copy(vsew=0, widen=true, vl=16, uopIdx=1, vm=false, ta=false)), //vmul.copy(u16, u8, u8, vl=16, uopIdx=1, vm=false, ta=false)),
          genVFuInput(SrcBundle("hc49b283cd998002b9c1488cb90f69f4c", "h390d6ca162a0761d51ed74ccfb6c3ce7"),
                      vwmulsu.copy(vsew=1, widen=true, vl=16, uopIdx=0)), //vmul.copy(s32, s16, u16, vl=16, uopIdx=0)),
          genVFuInput(SrcBundle("hc49b283cd998002b9c1488cb90f69f4c", "h390d6ca162a0761d51ed74ccfb6c3ce7"),
                      vwmulsu.copy(vsew=1, widen=true, vl=16, uopIdx=1)), //vmul.copy(s32, s16, u16, vl=16, uopIdx=1)),
          genVFuInput(SrcBundle("hb53bb440761bb0d86d60578b8a2f41a4", "hc0e85eaf3b27c0b6d8d7a6a645dca86a", "h8690131317af49f2df3628389b0700cc", "h3afc9370814160044070a85057a911a7"),
                      vwmulsu.copy(vsew=1, widen=true, vl=7, uopIdx=0, vm=false, ta=false, vstart=1)), //vmul.copy(u32, s16, u16, vl=7, uopIdx=0, vm=false, ta=false, vstart=1)),
          genVFuInput(SrcBundle("hb53bb440761bb0d86d60578b8a2f41a4", "hc0e85eaf3b27c0b6d8d7a6a645dca86a", "h9187c29d03a9d508655deeeb4524d9b9", "h3afc9370814160044070a85057a911a7"),
                      vwmulsu.copy(vsew=1, widen=true, vl=7, uopIdx=1, vm=false, ta=false, vstart=1)), //vmul.copy(u32, s16, u16, vl=7, uopIdx=1, vm=false, ta=false, vstart=1)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // vmul
          genVAluOutput("hf26d9c7409002078d4abc8dca0d1b9be"), // vmul 1091
          genVAluOutput("h1e8c733e62ae5191f15012d7e8549aba"), // vmul 1049
          genVAluOutput("hfffffffffffffffff15012d7e8549aba"), // vmul 1049
          genVAluOutput("h4958e790e90c42984a2dfb36b5e824ba"), // vmul 1245
          genVAluOutput("h565e84cd809015975fc46ddd2b7852dd"), // vmul 1315
          genVAluOutput("h565e84cd8090d97b5fc46ddd2b781c69"), // vmul 1315
          genVAluOutput("h565e84cd903ed97b5fc46ddd2b781c69"), // vmul 1315
          // vmulh/vmulhu/vmulhsu
          genVAluOutput("hff6ce330ebf5caa5f785a18bb14f53bc"), // vmulh 2141
          genVAluOutput("hfe918e09c28794a0052258bc051bdaed"), // vmulh 1567
          genVAluOutput("hffff1908ffff92ff057bffff80ffaa68"), // vmulhu 1063
          genVAluOutput("h28079340acb0c17a001f507332973f8a"), // vmulhu 1567
          genVAluOutput("hf038cee74a32dfb710f63cfd821f685b"), // vmulhsu 2687
          genVAluOutput("h000e2d24061008fed9442ae0df4901e8"), // vmulhsu 1091
          // widen
          genVAluOutput("h1b48ebc56418f239e3bd1b1eb9f08677"), // vwmul 1399
          genVAluOutput("h33dbecfcdf553926fcce01df6ffeaa0e"), // vwmul 1399
          genVAluOutput("h500c35ce13ba23c45448365c2dfc2ffb"), // vwmulu 1091
          genVAluOutput("h453408640a0ee6903bdfb7d211aa395d"), // vwmulu 1091
          genVAluOutput("hae402b50036c022f8b173ece61e2ec51"), // vwmulu 1161
          genVAluOutput("h07d018bb2bb90c406a4618c802500044"), // vwmulu 1161
          genVAluOutput("he005d284c99cfdc492f259c8e8fe8d94"), // vwmulsu 1245
          genVAluOutput("hf2c37edf11129dbcf1342f000013d6df"), // vwmulsu 1245
          genVAluOutput("h8690131338fce622dfd96b649b0700cc"), // vwmulsu 1329
          genVAluOutput("h9187c29d03a9d5081b4a371d4524d9b9"), // vwmulsu 1329
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

  def vMacTest1(): Unit = {
    it should "pass the macc/madd, fixed-point" in {
      test(new VMacWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        TestHarnessMac.test_init(dut)

        //----- Input gen -----
        val inputSeq = Seq(
          // 11.13 vmacc, ...
          genVFuInput(SrcBundle("h7a4334867ced31bbe5cb33ee399c1115", "h0a1ebd943a0d3a2a0a006fa5c61b1db3", "he8c8539798746c1f3b2ab61001ae0f7c"),
                      vmacc.copy(vsew=1, vl=8)), //vmacc.copy(s16, s16, s16, vl=8)),
          genVFuInput(SrcBundle("h7bcc2c42ce8be5465dfb087a7399bbd8", "hc59febb6d12ba8da300427bfdc4c5707", "ha6b8d9e5f6aa32dfd90b8aa1db644f66"),
                      vmacc.copy(vsew=3, vl=8)), //vmacc.copy(s64, s64, s64, vl=8)),
          genVFuInput(SrcBundle("ha6ebda784168cd76af5c54421055884b", "h7092289c8d2c5a333916f5d221b8f6e2", "h517b1be0a23b5ff7ab0de19841294c17", "hdf6b8da5c18b705ad3f38ac62021d5a5"),
                      vmacc.copy(vsew=1, vl=6, vstart=1, vm=false, ta=true)), //vmacc.copy(s16, s16, s16, vl=6, vstart=1, vm=false, ta=true)),
          genVFuInput(SrcBundle("h2330b5edb7c6b4acf4cb6f75115d7f2d", "hee3c56069c5985e6186ddf69d26981c6", "h1443ca33940e6979a8367181ab1eb67c"),
                      vnmsac.copy(vsew=0)), //vnmsac.copy(s8, s8, s8)),
          genVFuInput(SrcBundle("h6a9542a271f7d8781f4fe502e25dff72", "h824a6b880a347247e4d8a077b482c8ff", "h3a7018baf93635f3bde8b5d321cfb60d", "hdb92a06ebb12475b5614ce82f8de4610"),
                      vnmsac.copy(vsew=0, vl=8, vm=false, ta=false)), //vnmsac.copy(s8, s8, s8, vl=8, vm=false, ta=false)),
          genVFuInput(SrcBundle("h322dd3732c9f2524aea99c89c35eca89", "h4ce5d0dfe38b9a9f93d7fd75e3c51f88", "h4541400537a0b174650cb6cb35fbc1ce", "h70403810f77fea75441612b7223ad31b"),
                      vnmsac.copy(vsew=2, vl=16, uopIdx=2)), //vnmsac.copy(s32, s32, s32, vl=16, uopIdx=2)),
          genVFuInput(SrcBundle("h35f383f5de3aa9be2581f197160841e8", "h397c4a87431448de5b8172a6f924e028", "hcb0e731241c03becf18e381f04b04e4a", "h78dac7e195c8180935f87ea138589a35"),
                      vmadd.copy(vsew=1, vl=8, vm=false, ta=false )), //vmadd.copy(s16, s16, s16, vl=8, vm=false, ta=false )),
          genVFuInput(SrcBundle("h6605ab95dfe9c7d7e9c4d6392c91e3fc", "h876858858eb4ea21579b3b7f93d217b0", "h9c3b2995ce579904d0644125d7b570f5", "hb29b0586c7b24a856052de79ee31db1e"),
                      vmadd.copy(vsew=3, vl=2)), //vmadd.copy(s64, s64, s64, vl=2)),
          genVFuInput(SrcBundle("hc479b82e1a8c40440ed2d8df0c32df40", "hf234025cccaf5b33b2062960963f34a8", "hd6dda0b26ba4a5d10a9e2749eadd36e7", "hdd0c3cf8efb8af11e3f5a7c5044c533e"),
                      vnmsub.copy(vsew=0, vl=16, vm=false, ta=false)), //vnmsub.copy(s8, s8, s8, vl=16, vm=false, ta=false)),
          genVFuInput(SrcBundle("h93857ffe6d4a1ebe155eaf7c852a2ffd", "h7132fa74d7ae59f9b30457337eceb470", "hb82219c3546b711f0adf48621807ebd9", "h52e6c1664262134453709b773396d56f"),
                      vnmsub.copy(vsew=2, vl=32)), //vnmsub.copy(s32, s32, s32, vl=32)),
          // 11.14 vwmaccu, ...
          genVFuInput(SrcBundle("hcb097cbd0ae9e323ba616caefefcfba5", "hf203cb1f52797a244e6df2336e4a158e", "hc0eb6d6bb199d9528626be69b0e33ac0", "h95b46c9dab109f383ade4e01d64ae284"),
                      vwmaccu.copy(vsew=1, widen=true, vl=8)), //vmacc.copy(u32, u16, u16, vl=8)),
          genVFuInput(SrcBundle("hcb097cbd0ae9e323ba616caefefcfba5", "hf203cb1f52797a244e6df2336e4a158e", "h0bac343b49fbe070bc837a274dfe8fad", "h95b46c9dab109f383ade4e01d64ae284"),
                      vwmaccu.copy(vsew=1, widen=true, vl=8, uopIdx=1)), //vmacc.copy(u32, u16, u16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h22892d91142feb24bfc629cde5b7039e", "he079fdca97df1fff5966f474d00e82a5", "hcdb62e2b63bb0d6df6841e97c078bf9b", "h2748306bf965b71e5473f7ccca6b3ee9"),
                      vwmacc.copy(vsew=0, widen=true, vl=16, vm=false, ta=false, uopIdx=0)), //vwmacc.copy(s16, s8, s8, vl=16, vm=false, ta=false, uopIdx=0)),
          genVFuInput(SrcBundle("h22892d91142feb24bfc629cde5b7039e", "he079fdca97df1fff5966f474d00e82a5", "h803c6e6867e54485d2901e4257cdd0e2", "h2748306bf965b71e5473f7ccca6b3ee9"),
                      vwmacc.copy(vsew=0, widen=true, vl=16, vm=false, ta=false, uopIdx=1)), //vmacc.copy(s16, s8, s8, vl=16, vm=false, ta=false, uopIdx=1)),
          genVFuInput(SrcBundle("he32ab2a10ab5db6bc8dcd334c5bb8545", "h4c67d9f4f3a17e5bbdcf816b1cf7101a", "h46c25d9266656e095cd00e3e69497407", "h4d3731e5a47ac3cce8ccab1808292971"),
                      vwmaccsu.copy(vsew=1, widen=true, vl=8, uopIdx=0)), //vmacc.copy(s32, u16, s16, vl=8, uopIdx=0)),
          genVFuInput(SrcBundle("he32ab2a10ab5db6bc8dcd334c5bb8545", "h4c67d9f4f3a17e5bbdcf816b1cf7101a", "hd860ad068164b7803578b0a02c795157", "h4d3731e5a47ac3cce8ccab1808292971"),
                      vwmaccsu.copy(vsew=1, widen=true, vl=8, uopIdx=1)), //vmacc.copy(s32, u16, s16, vl=8, uopIdx=1)),
          genVFuInput(SrcBundle("h7bdff7b574e1c868bef8b57ef96cc960", rs1="h1212121212121212", old_vd="hdc5b1249e3f23cecf6550aa851cdef15", mask="h6d5ad696376b5cbba9675ddcdfb67b7f"),
                      vwmaccus.copy(vsew=0, widen=true, vl=16, vm=false, ta=false, uopIdx=0)), //vmacc.copy(s16, s8, u8, vl=16, vm=false, ta=false, uopIdx=0)),
          genVFuInput(SrcBundle("h7bdff7b574e1c868bef8b57ef96cc960", rs1="h1212121212121212", old_vd="h30df60b0251ef61ccdc93fe959bbbd60", mask="h6d5ad696376b5cbba9675ddcdfb67b7f"),
                      vwmaccus.copy(vsew=0, widen=true, vl=16, vm=false, ta=false, uopIdx=1)), //vmacc.copy(s16, s8, u8, vl=16, vm=false, ta=false, uopIdx=1)),
          // vsmul
          genVFuInput(SrcBundle("h8d761fc68afdbd08222b837d9cbfc066", "h6dd479e194eba80f0cf7ed586ce10e9d", "hbc6913a4392324175c94fd1f93170019", "he1ee2fb95096b8c97a6ad0b0c11ce5f1"),
                      vsmul.copy(vsew=2, vl=2, ta=false, vlmul=7, vxrm=0)), //vsmul.copy(s32, s32, s32, vl=2, ta=false, vlmul=7, vxrm=0)),
          genVFuInput(SrcBundle("h791a297724a32ae31ab5d8295fac8216", "h17b39c64af7a70f1ed94b3cb087c5a41", "h7874ec2e595608e90bdf03a748924295"),
                      vsmul.copy(vsew=0, vl=16, vxrm=1)), //vsmul.copy(s8, s8, s8, vl=16, vxrm=1)),
          genVFuInput(SrcBundle("h47e51219c39602abc5e6b089e8e08b20", "h176293fee6276406ea89532518c7fd7d", "ha340963e941e8e9591336c77d9691503"),
                      vsmul.copy(vsew=0, vl=16, vxrm=2)), //vsmul.copy(s8, s8, s8, vl=16, vxrm=2)),
          genVFuInput(SrcBundle("h540b1fe8fc7e6fc35b5f49a1027c11f7", "h9b5192736d6c4fe4be45a1ff9b4c4a0b", "hf1a106126e33623eb9ee8c048260fbc6"),
                      vsmul.copy(vsew=0, vl=16, vxrm=3)), //vsmul.copy(s8, s8, s8, vl=16, vxrm=3)),
          genVFuInput(SrcBundle("h0c2b49818dc1b9e4e700c3508a5a9f69", "h8a94d35972841fb81656a0205e179319", "h4dc36a2d1e9967a8b6d2dedd2c3ad39f", "h0c642070f3e354d84e75bf2efdeddaa6"),
                      vsmul.copy(vsew=1, vl=32, vm=false, ta=false, vxrm=3, uopIdx=2)), //vsmul.copy(s16, s16, s16, vl=32, vm=false, ta=false, vxrm=3, uopIdx=2)),
          genVFuInput(SrcBundle("h80000000000000008000000000000000", "h80000000000000008000000000000000"),
                      vsmul.copy(vsew=3, vl=2, vxrm=3, uopIdx=0)), //vsmul.copy(s64, s64, s64, vl=2, vxrm=3, uopIdx=0)),
          genVFuInput(SrcBundle("h80808080808080808080808080808080", "h80808080808080808080808080808080"),
                      vsmul.copy(vsew=0, vl=16, vxrm=1, uopIdx=0)), //vsmul.copy(s8, s8, s8, vl=16, vxrm=1, uopIdx=0)),
          genVFuInput(SrcBundle("h00000000000000008000000000000000", "h00000000000000008000000000000000"),
                      vsmul.copy(vsew=1, vl=8, vxrm=0, uopIdx=0)), //vsmul.copy(s16, s16, s16, vl=8, vxrm=0, uopIdx=0)),
          genVFuInput(SrcBundle("h00000000000080000000000000000000", "h00000000000080000000000000000000"),
                      vsmul.copy(vsew=1, vl=8, vxrm=0, uopIdx=0)), //vsmul.copy(s16, s16, s16, vl=8, vxrm=0, uopIdx=0)),
        )

        //----- Output expectation -----
        val outputSeq = Seq(
          // 11.13 vmacc, ...
          genVAluOutput("hdaa29f0fa27df2cd292a6076bd22622b"), // vmacc 1231
          genVAluOutput("h90e18bc75103607b952de07da78eda4e"), // vmacc 1595
          genVAluOutput("hffffffff281b5ff7ab0d29bc41294c17"), // vmacc 1343
          genVAluOutput("h8a03fca51038e5f1c8c7c084b9f9b7ae"), // vnmsac 1077
          genVAluOutput("h3a7018baf93635f3bde8b5e521cfb60d"), // vnmsac 1133
          genVAluOutput("h09a99ed8ade4f818cfc5c72e9aac9206"), // vnmsac 2757
          genVAluOutput("hcb0e7312413a0066f18e23b104b03d78"), // vmadd 1329
          genVAluOutput("h892e820d59e2295bd7d6a9dd1e6f8f6c"), // vmadd 1595
          genVAluOutput("hd695a0366ba499a10a9e997ff0cfe7e7"), // vnmsub 1147
          genVAluOutput("hbbd365a20a63509761c1f5f6d9406d0d"), // vnmsub 3415
          // 11.14 vwmaccu, ...
          genVAluOutput("hfa0456b8186bfbfcf400bb41c6135946"), // vwmaccu 1259
          genVAluOutput("hcb9d1756acf4da53c0074448ba5d2e99"), // vwmaccu 1259
          genVAluOutput("hb71d170f61cf0d6dfb941e97c078e271"), // vwmacc 1175
          genVAluOutput("h803c6e68675e5befca5c18335542d0e2"), // vwmacc 1175
          genVAluOutput("h12d33376fdf6e8c5732f49ab71ab4d09"), // vwmaccsu 1259
          genVAluOutput("h1c2c8aec66d871f434f43b7598c5fa60"), // vwmaccsu 1259
          genVAluOutput("hdc5b11b9deac45c8f5d712404deff5d5"), // vwmaccus 1091
          genVAluOutput("h30df5e5e247cf0d6d5f13fe955cbc4b0"), // vwmaccus 1091
          // vsmul
          genVAluOutput("hbc6913a439232417037643a8ab934811"), // vsmul 3485
          genVAluOutput("h16f0e05de9a72503fc3f18ef06afa70b"), // vsmul 4227
          genVAluOutput("h0cebf0ff0cdf01fc0a18ccddfb0e021f"), // vsmul 4241
          genVAluOutput("hbd07e5ebfd6b450dd133c901ff4909ff"), // vsmul 4255
          genVAluOutput("hf4d7e65b99cb67a8fba32d752c3a522d"), // vsmul 10639
          genVAluOutput("h7fffffffffffffff7fffffffffffffff", vxsat=true), // handmade
          genVAluOutput("h7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f", vxsat=true), // handmade
          genVAluOutput("h00000000000000007fff000000000000", vxsat=true), // handmade
          genVAluOutput("h0000000000007fff0000000000000000", vxsat=true), // handmade
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

class VMacSpec extends AnyFlatSpec with ChiselScalatestTester with BundleGenHelper with VMacBehavior {
  behavior of "Mac test"
  it should behave like vMacTest0()
  it should behave like vMacTest1()
}