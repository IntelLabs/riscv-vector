
package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{FunSuite, ParallelTestExecution}
import chisel3._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
/*import yunsuan.vector._
import yunsuan.vector.alu._
import yunsuan.vectorAutotest._*/
import chiseltest.WriteVcdAnnotation
// import yunsuan.vectorAutotest.dataType._
// import yunsuan.vector.alu.VAluOpcode._
// import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.io.Source
import java.io.FileWriter
import java.time.{LocalDate, LocalDateTime}
import java.nio.file.{Paths, Files}
import java.time.format.DateTimeFormatter
import java.io._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._
import ExecutionContext.Implicits.global
import scala.collection.concurrent.{Map => ConCurMap}
import scala.collection.mutable.ListBuffer
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap



object ctrlBundles {
  // Vector Load/Store instructions
  val vle8_v = CtrlBundle(VLE8_V)
  val vle16_v = CtrlBundle(VLE16_V)
  val vle32_v = CtrlBundle(VLE32_V)
  val vle64_v = CtrlBundle(VLE64_V)
  val vse8_v = CtrlBundle(VSE8_V)
  val vse16_v = CtrlBundle(VSE16_V)
  val vse32_v = CtrlBundle(VSE32_V)
  val vse64_v = CtrlBundle(VSE64_V)
  val vlm_v = CtrlBundle(VLM_V)
  val vsm_v = CtrlBundle(VSM_V)
  val vlse8_v = CtrlBundle(VLSE8_V)
  val vlse16_v = CtrlBundle(VLSE16_V)
  val vlse32_v = CtrlBundle(VLSE32_V)
  val vlse64_v = CtrlBundle(VLSE64_V)
  val vsse8_v = CtrlBundle(VSSE8_V)
  val vsse16_v = CtrlBundle(VSSE16_V)
  val vsse32_v = CtrlBundle(VSSE32_V)
  val vsse64_v = CtrlBundle(VSSE64_V)
  val vluxei8_v = CtrlBundle(VLUXEI8_V)
  val vluxei16_v = CtrlBundle(VLUXEI16_V)
  val vluxei32_v = CtrlBundle(VLUXEI32_V)
  val vluxei64_v = CtrlBundle(VLUXEI64_V)
  val vloxei8_v = CtrlBundle(VLOXEI8_V)
  val vloxei16_v = CtrlBundle(VLOXEI16_V)
  val vloxei32_v = CtrlBundle(VLOXEI32_V)
  val vloxei64_v = CtrlBundle(VLOXEI64_V)
  val vsuxei8_v = CtrlBundle(VSUXEI8_V)
  val vsuxei16_v = CtrlBundle(VSUXEI16_V)
  val vsuxei32_v = CtrlBundle(VSUXEI32_V)
  val vsuxei64_v = CtrlBundle(VSUXEI64_V)
  val vsoxei8_v = CtrlBundle(VSOXEI8_V)
  val vsoxei16_v = CtrlBundle(VSOXEI16_V)
  val vsoxei32_v = CtrlBundle(VSOXEI32_V)
  val vsoxei64_v = CtrlBundle(VSOXEI64_V)
  val vle8ff_v = CtrlBundle(VLE8FF_V)
  val vle16ff_v = CtrlBundle(VLE16FF_V)
  val vle32ff_v = CtrlBundle(VLE32FF_V)
  val vle64ff_v = CtrlBundle(VLE64FF_V)
  val vl1re8_v = CtrlBundle(VL1RE8_V)
  val vl1re16_v = CtrlBundle(VL1RE16_V)
  val vl1re32_v = CtrlBundle(VL1RE32_V)
  val vl1re64_v = CtrlBundle(VL1RE64_V)
  val vl2re8_v = CtrlBundle(VL2RE8_V)
  val vl2re16_v = CtrlBundle(VL2RE16_V)
  val vl2re32_v = CtrlBundle(VL2RE32_V)
  val vl2re64_v = CtrlBundle(VL2RE64_V)
  val vl4re8_v = CtrlBundle(VL4RE8_V)
  val vl4re16_v = CtrlBundle(VL4RE16_V)
  val vl4re32_v = CtrlBundle(VL4RE32_V)
  val vl4re64_v = CtrlBundle(VL4RE64_V)
  val vl8re8_v = CtrlBundle(VL8RE8_V)
  val vl8re16_v = CtrlBundle(VL8RE16_V)
  val vl8re32_v = CtrlBundle(VL8RE32_V)
  val vl8re64_v = CtrlBundle(VL8RE64_V)
  val vs1r_v = CtrlBundle(VS1R_V)
  val vs2r_v = CtrlBundle(VS2R_V)
  val vs4r_v = CtrlBundle(VS4R_V)
  val vs8r_v = CtrlBundle(VS8R_V)

  // Vector Integer Arithmetic Instructions
  val vadd_vv = CtrlBundle(VADD_VV)
  val vadd_vx = CtrlBundle(VADD_VX)
  val vadd_vi = CtrlBundle(VADD_VI)
  val vsub_vv = CtrlBundle(VSUB_VV)
  val vsub_vx = CtrlBundle(VSUB_VX)
  val vrsub_vx = CtrlBundle(VRSUB_VX)
  val vrsub_vi = CtrlBundle(VRSUB_VI)
  val vwaddu_vv = CtrlBundle(VWADDU_VV)
  val vwaddu_vx = CtrlBundle(VWADDU_VX)
  val vwsubu_vv = CtrlBundle(VWSUBU_VV)
  val vwsubu_vx = CtrlBundle(VWSUBU_VX)
  val vwadd_vv = CtrlBundle(VWADD_VV)
  val vwadd_vx = CtrlBundle(VWADD_VX)
  val vwsub_vv = CtrlBundle(VWSUB_VV)
  val vwsub_vx = CtrlBundle(VWSUB_VX)
  val vwaddu_wv = CtrlBundle(VWADDU_WV)
  val vwaddu_wx = CtrlBundle(VWADDU_WX)
  val vwsubu_wv = CtrlBundle(VWSUBU_WV)
  val vwsubu_wx = CtrlBundle(VWSUBU_WX)
  val vwadd_wv = CtrlBundle(VWADD_WV)
  val vwadd_wx = CtrlBundle(VWADD_WX)
  val vwsub_wv = CtrlBundle(VWSUB_WV)
  val vwsub_wx = CtrlBundle(VWSUB_WX)
  val vzext_vf2 = CtrlBundle(VZEXT_VF2)
  val vsext_vf2 = CtrlBundle(VSEXT_VF2)
  val vzext_vf4 = CtrlBundle(VZEXT_VF4)
  val vsext_vf4 = CtrlBundle(VSEXT_VF4)
  val vzext_vf8 = CtrlBundle(VZEXT_VF8)
  val vsext_vf8 = CtrlBundle(VSEXT_VF8)
  val vadc_vvm = CtrlBundle(VADC_VVM)
  val vadc_vxm = CtrlBundle(VADC_VXM)
  val vadc_vim = CtrlBundle(VADC_VIM)
  val vmadc_vvm = CtrlBundle(VMADC_VVM)
  val vmadc_vxm = CtrlBundle(VMADC_VXM)
  val vmadc_vim = CtrlBundle(VMADC_VIM)
  val vmadc_vv = CtrlBundle(VMADC_VV)
  val vmadc_vx = CtrlBundle(VMADC_VX)
  val vmadc_vi = CtrlBundle(VMADC_VI)
  val vsbc_vvm = CtrlBundle(VSBC_VVM)
  val vsbc_vxm = CtrlBundle(VSBC_VXM)
  val vmsbc_vvm = CtrlBundle(VMSBC_VVM)
  val vmsbc_vxm = CtrlBundle(VMSBC_VXM)
  val vmsbc_vv = CtrlBundle(VMSBC_VV)
  val vmsbc_vx = CtrlBundle(VMSBC_VX)
  val vand_vv = CtrlBundle(VAND_VV)
  val vand_vx = CtrlBundle(VAND_VX)
  val vand_vi = CtrlBundle(VAND_VI)
  val vor_vv = CtrlBundle(VOR_VV)
  val vor_vx = CtrlBundle(VOR_VX)
  val vor_vi = CtrlBundle(VOR_VI)
  val vxor_vv = CtrlBundle(VXOR_VV)
  val vxor_vx = CtrlBundle(VXOR_VX)
  val vxor_vi = CtrlBundle(VXOR_VI)
  val vsll_vv = CtrlBundle(VSLL_VV)
  val vsll_vx = CtrlBundle(VSLL_VX)
  val vsll_vi = CtrlBundle(VSLL_VI)
  val vsrl_vv = CtrlBundle(VSRL_VV)
  val vsrl_vx = CtrlBundle(VSRL_VX)
  val vsrl_vi = CtrlBundle(VSRL_VI)
  val vsra_vv = CtrlBundle(VSRA_VV)
  val vsra_vx = CtrlBundle(VSRA_VX)
  val vsra_vi = CtrlBundle(VSRA_VI)
  val vnsrl_wv = CtrlBundle(VNSRL_WV)
  val vnsrl_wx = CtrlBundle(VNSRL_WX)
  val vnsrl_wi = CtrlBundle(VNSRL_WI)
  val vnsra_wv = CtrlBundle(VNSRA_WV)
  val vnsra_wx = CtrlBundle(VNSRA_WX)
  val vnsra_wi = CtrlBundle(VNSRA_WI)
  val vmseq_vv = CtrlBundle(VMSEQ_VV)
  val vmseq_vx = CtrlBundle(VMSEQ_VX)
  val vmseq_vi = CtrlBundle(VMSEQ_VI)
  val vmsne_vv = CtrlBundle(VMSNE_VV)
  val vmsne_vx = CtrlBundle(VMSNE_VX)
  val vmsne_vi = CtrlBundle(VMSNE_VI)
  val vmsltu_vv = CtrlBundle(VMSLTU_VV)
  val vmsltu_vx = CtrlBundle(VMSLTU_VX)
  val vmslt_vv = CtrlBundle(VMSLT_VV)
  val vmslt_vx = CtrlBundle(VMSLT_VX)
  val vmsleu_vv = CtrlBundle(VMSLEU_VV)
  val vmsleu_vx = CtrlBundle(VMSLEU_VX)
  val vmsleu_vi = CtrlBundle(VMSLEU_VI)
  val vmsle_vv = CtrlBundle(VMSLE_VV)
  val vmsle_vx = CtrlBundle(VMSLE_VX)
  val vmsle_vi = CtrlBundle(VMSLE_VI)
  val vmsgtu_vx = CtrlBundle(VMSGTU_VX)
  val vmsgtu_vi = CtrlBundle(VMSGTU_VI)
  val vmsgt_vx = CtrlBundle(VMSGT_VX)
  val vmsgt_vi = CtrlBundle(VMSGT_VI)
  val vminu_vv = CtrlBundle(VMINU_VV)
  val vminu_vx = CtrlBundle(VMINU_VX)
  val vmin_vv = CtrlBundle(VMIN_VV)
  val vmin_vx = CtrlBundle(VMIN_VX)
  val vmaxu_vv = CtrlBundle(VMAXU_VV)
  val vmaxu_vx = CtrlBundle(VMAXU_VX)
  val vmax_vv = CtrlBundle(VMAX_VV)
  val vmax_vx = CtrlBundle(VMAX_VX)

  val vmul_vv = CtrlBundle(VMUL_VV)
  val vmul_vx = CtrlBundle(VMUL_VX)
  val vmulh_vv = CtrlBundle(VMULH_VV)
  val vmulh_vx = CtrlBundle(VMULH_VX)
  val vmulhu_vv = CtrlBundle(VMULHU_VV)
  val vmulhu_vx = CtrlBundle(VMULHU_VX)
  val vmulhsu_vv = CtrlBundle(VMULHSU_VV)
  val vmulhsu_vx = CtrlBundle(VMULHSU_VX)
  val vwmul_vv = CtrlBundle(VWMUL_VV)
  val vwmul_vx = CtrlBundle(VWMUL_VX)
  val vwmulu_vv = CtrlBundle(VWMULU_VV)
  val vwmulu_vx = CtrlBundle(VWMULU_VX)
  val vwmulsu_vv = CtrlBundle(VWMULSU_VV)
  val vwmulsu_vx = CtrlBundle(VWMULSU_VX)
  val vmacc_vv = CtrlBundle(VMACC_VV)
  val vmacc_vx = CtrlBundle(VMACC_VX)
  val vnmsac_vv = CtrlBundle(VNMSAC_VV)
  val vnmsac_vx = CtrlBundle(VNMSAC_VX)
  val vmadd_vv = CtrlBundle(VMADD_VV)
  val vmadd_vx = CtrlBundle(VMADD_VX)
  val vnmsub_vv = CtrlBundle(VNMSUB_VV)
  val vnmsub_vx = CtrlBundle(VNMSUB_VX)
  val vwmaccu_vv = CtrlBundle(VWMACCU_VV)
  val vwmaccu_vx = CtrlBundle(VWMACCU_VX)
  val vwmacc_vv = CtrlBundle(VWMACC_VV)
  val vwmacc_vx = CtrlBundle(VWMACC_VX)
  val vwmaccsu_vv = CtrlBundle(VWMACCSU_VV)
  val vwmaccsu_vx = CtrlBundle(VWMACCSU_VX)
  val vwmaccus_vx = CtrlBundle(VWMACCUS_VX)
  val vmerge_vvm = CtrlBundle(VMERGE_VVM)
  val vmerge_vxm = CtrlBundle(VMERGE_VXM)
  val vmerge_vim = CtrlBundle(VMERGE_VIM)
  val vmv_v_v = CtrlBundle(VMV_V_V)
  val vmv_v_x = CtrlBundle(VMV_V_X)
  val vmv_v_i = CtrlBundle(VMV_V_I)

  // Vector Fixed-Point instructions
  val vsaddu_vv = CtrlBundle(VSADDU_VV)
  val vsaddu_vx = CtrlBundle(VSADDU_VX)
  val vsaddu_vi = CtrlBundle(VSADDU_VI)
  val vsadd_vv = CtrlBundle(VSADD_VV)
  val vsadd_vx = CtrlBundle(VSADD_VX)
  val vsadd_vi = CtrlBundle(VSADD_VI)
  val vssubu_vv = CtrlBundle(VSSUBU_VV)
  val vssubu_vx = CtrlBundle(VSSUBU_VX)
  val vssub_vv = CtrlBundle(VSSUB_VV)
  val vssub_vx = CtrlBundle(VSSUB_VX)
  val vaaddu_vv = CtrlBundle(VAADDU_VV)
  val vaaddu_vx = CtrlBundle(VAADDU_VX)
  val vaadd_vv = CtrlBundle(VAADD_VV)
  val vaadd_vx = CtrlBundle(VAADD_VX)
  val vasubu_vv = CtrlBundle(VASUBU_VV)
  val vasubu_vx = CtrlBundle(VASUBU_VX)
  val vasub_vv = CtrlBundle(VASUB_VV)
  val vasub_vx = CtrlBundle(VASUB_VX)
  val vsmul_vv = CtrlBundle(VSMUL_VV)
  val vsmul_vx = CtrlBundle(VSMUL_VX)
  val vssrl_vv = CtrlBundle(VSSRL_VV)
  val vssrl_vx = CtrlBundle(VSSRL_VX)
  val vssrl_vi = CtrlBundle(VSSRL_VI)
  val vssra_vv = CtrlBundle(VSSRA_VV)
  val vssra_vx = CtrlBundle(VSSRA_VX)
  val vssra_vi = CtrlBundle(VSSRA_VI)
  val vnclipu_wv = CtrlBundle(VNCLIPU_WV)
  val vnclipu_wx = CtrlBundle(VNCLIPU_WX)
  val vnclipu_wi = CtrlBundle(VNCLIPU_WI)
  val vnclip_wv = CtrlBundle(VNCLIP_WV)
  val vnclip_wx = CtrlBundle(VNCLIP_WX)
  val vnclip_wi = CtrlBundle(VNCLIP_WI)

  // Vector Floating-point instructions
  val vfadd_vv = CtrlBundle(VFADD_VV)
  val vfadd_vf = CtrlBundle(VFADD_VF)
  val vfsub_vv = CtrlBundle(VFSUB_VV)
  val vfsub_vf = CtrlBundle(VFSUB_VF)
  val vfrsub_vf = CtrlBundle(VFRSUB_VF)
  val vfwadd_vv = CtrlBundle(VFWADD_VV)
  val vfwadd_vf = CtrlBundle(VFWADD_VF)
  val vfwsub_vv = CtrlBundle(VFWSUB_VV)
  val vfwsub_vf = CtrlBundle(VFWSUB_VF)
  val vfwadd_wv = CtrlBundle(VFWADD_WV)
  val vfwadd_wf = CtrlBundle(VFWADD_WF)
  val vfwsub_wv = CtrlBundle(VFWSUB_WV)
  val vfwsub_wf = CtrlBundle(VFWSUB_WF)
  val vfmul_vv = CtrlBundle(VFMUL_VV)
  val vfmul_vf = CtrlBundle(VFMUL_VF)
  val vfdiv_vv = CtrlBundle(VFDIV_VV)
  val vfdiv_vf = CtrlBundle(VFDIV_VF)
  val vfrdiv_vf = CtrlBundle(VFRDIV_VF)
  val vfwmul_vv = CtrlBundle(VFWMUL_VV)
  val vfwmul_vf = CtrlBundle(VFWMUL_VF)
  val vfmacc_vv = CtrlBundle(VFMACC_VV)
  val vfmacc_vf = CtrlBundle(VFMACC_VF)
  val vfnmacc_vv = CtrlBundle(VFNMACC_VV)
  val vfnmacc_vf = CtrlBundle(VFNMACC_VF)
  val vfmsac_vv = CtrlBundle(VFMSAC_VV)
  val vfmsac_vf = CtrlBundle(VFMSAC_VF)
  val vfnmsac_vv = CtrlBundle(VFNMSAC_VV)
  val vfnmsac_vf = CtrlBundle(VFNMSAC_VF)
  val vfmadd_vv = CtrlBundle(VFMADD_VV)
  val vfmadd_vf = CtrlBundle(VFMADD_VF)
  val vfnmadd_vv = CtrlBundle(VFNMADD_VV)
  val vfnmadd_vf = CtrlBundle(VFNMADD_VF)
  val vfmsub_vv = CtrlBundle(VFMSUB_VV)
  val vfmsub_vf = CtrlBundle(VFMSUB_VF)
  val vfnmsub_vv = CtrlBundle(VFNMSUB_VV)
  val vfnmsub_vf = CtrlBundle(VFNMSUB_VF)
  val vfwmacc_vv = CtrlBundle(VFWMACC_VV)
  val vfwmacc_vf = CtrlBundle(VFWMACC_VF)
  val vfwnmacc_vv = CtrlBundle(VFWNMACC_VV)
  val vfwnmacc_vf = CtrlBundle(VFWNMACC_VF)
  val vfwmsac_vv = CtrlBundle(VFWMSAC_VV)
  val vfwmsac_vf = CtrlBundle(VFWMSAC_VF)
  val vfwnmsac_vv = CtrlBundle(VFWNMSAC_VV)
  val vfwnmsac_vf = CtrlBundle(VFWNMSAC_VF)
  val vfsqrt_v = CtrlBundle(VFSQRT_V)
  val vfrsqrt7_v = CtrlBundle(VFRSQRT7_V)
  val vfrec7_v = CtrlBundle(VFREC7_V)
  val vfmin_vv = CtrlBundle(VFMIN_VV)
  val vfmin_vf = CtrlBundle(VFMIN_VF)
  val vfmax_vv = CtrlBundle(VFMAX_VV)
  val vfmax_vf = CtrlBundle(VFMAX_VF)
  val vfsgnj_vv = CtrlBundle(VFSGNJ_VV)
  val vfsgnj_vf = CtrlBundle(VFSGNJ_VF)
  val vfsgnjn_vv = CtrlBundle(VFSGNJN_VV)
  val vfsgnjn_vf = CtrlBundle(VFSGNJN_VF)
  val vfsgnjx_vv = CtrlBundle(VFSGNJX_VV)
  val vfsgnjx_vf = CtrlBundle(VFSGNJX_VF)
  val vmfeq_vv = CtrlBundle(VMFEQ_VV)
  val vmfeq_vf = CtrlBundle(VMFEQ_VF)
  val vmfne_vv = CtrlBundle(VMFNE_VV)
  val vmfne_vf = CtrlBundle(VMFNE_VF)
  val vmflt_vv = CtrlBundle(VMFLT_VV)
  val vmflt_vf = CtrlBundle(VMFLT_VF)
  val vmfle_vv = CtrlBundle(VMFLE_VV)
  val vmfle_vf = CtrlBundle(VMFLE_VF)
  val vmfgt_vf = CtrlBundle(VMFGT_VF)
  val vmfge_vf = CtrlBundle(VMFGE_VF)
  val vfclass_v = CtrlBundle(VFCLASS_V)
  val vfmerge_vfm = CtrlBundle(VFMERGE_VFM)
  val vfmv_v_f = CtrlBundle(VFMV_V_F)
  val vfcvt_xu_f_v = CtrlBundle(VFCVT_XU_F_V)
  val vfcvt_x_f_v = CtrlBundle(VFCVT_X_F_V)
  val vfcvt_rtz_xu_f_v = CtrlBundle(VFCVT_RTZ_XU_F_V)
  val vfcvt_rtz_x_f_v = CtrlBundle(VFCVT_RTZ_X_F_V)
  val vfcvt_f_xu_v = CtrlBundle(VFCVT_F_XU_V)
  val vfcvt_f_x_v = CtrlBundle(VFCVT_F_X_V)
  val vfwcvt_xu_f_v = CtrlBundle(VFWCVT_XU_F_V)
  val vfwcvt_x_f_v = CtrlBundle(VFWCVT_X_F_V)
  val vfwcvt_rtz_xu_f_v = CtrlBundle(VFWCVT_RTZ_XU_F_V)
  val vfwcvt_rtz_x_f_v = CtrlBundle(VFWCVT_RTZ_X_F_V)
  val vfwcvt_f_xu_v = CtrlBundle(VFWCVT_F_XU_V)
  val vfwcvt_f_x_v = CtrlBundle(VFWCVT_F_X_V)
  val vfwcvt_f_f_v = CtrlBundle(VFWCVT_F_F_V)
  val vfncvt_xu_f_w = CtrlBundle(VFNCVT_XU_F_W)
  val vfncvt_x_f_w = CtrlBundle(VFNCVT_X_F_W)
  val vfncvt_rtz_xu_f_w = CtrlBundle(VFNCVT_RTZ_XU_F_W)
  val vfncvt_rtz_x_f_w = CtrlBundle(VFNCVT_RTZ_X_F_W)
  val vfncvt_f_xu_w = CtrlBundle(VFNCVT_F_XU_W)
  val vfncvt_f_x_w = CtrlBundle(VFNCVT_F_X_W)
  val vfncvt_f_f_w = CtrlBundle(VFNCVT_F_F_W)
  val vfncvt_rod_f_f_w = CtrlBundle(VFNCVT_ROD_F_F_W)

  // Vector reduction instructions
  val vredsum_vs = CtrlBundle(VREDSUM_VS)
  val vredmax_vs = CtrlBundle(VREDMAX_VS)
  val vredmaxu_vs = CtrlBundle(VREDMAXU_VS)
  val vredmin_vs = CtrlBundle(VREDMIN_VS)
  val vredminu_vs = CtrlBundle(VREDMINU_VS)
  val vredand_vs = CtrlBundle(VREDAND_VS)
  val vredor_vs = CtrlBundle(VREDOR_VS)
  val vredxor_vs = CtrlBundle(VREDXOR_VS)
  val vwredsumu_vs = CtrlBundle(VWREDSUMU_VS)
  val vwredsum_vs = CtrlBundle(VWREDSUM_VS)
  val vfredosum_vs = CtrlBundle(VFREDOSUM_VS)
  val vfredusum_vs = CtrlBundle(VFREDUSUM_VS)
  val vfredmin_vs = CtrlBundle(VFREDMIN_VS)
  val vfredmax_vs = CtrlBundle(VFREDMAX_VS)
  val vfwredusum_vs = CtrlBundle(VFWREDUSUM_VS)
  val vfwredosum_vs = CtrlBundle(VFWREDOSUM_VS)

  // Vector mask instructions
  val vmand_mm = CtrlBundle(VMAND_MM)
  val vmnand_mm = CtrlBundle(VMNAND_MM)
  val vmandn_mm = CtrlBundle(VMANDN_MM)
  val vmxor_mm = CtrlBundle(VMXOR_MM)
  val vmor_mm = CtrlBundle(VMOR_MM)
  val vmnor_mm = CtrlBundle(VMNOR_MM)
  val vmorn_mm = CtrlBundle(VMORN_MM)
  val vmxnor_mm = CtrlBundle(VMXNOR_MM)
  val vcpop_m = CtrlBundle(VCPOP_M)
  val vfirst_m = CtrlBundle(VFIRST_M)
  val vmsbf_m = CtrlBundle(VMSBF_M)
  val vmsif_m = CtrlBundle(VMSIF_M)
  val vmsof_m = CtrlBundle(VMSOF_M)
  val viota_m = CtrlBundle(VIOTA_M)
  val vid_v = CtrlBundle(VID_V)

  // Vector Permutation instructions
  val vmv_x_s = CtrlBundle(VMV_X_S)
  val vmv_s_x = CtrlBundle(VMV_S_X)
  val vfmv_f_s = CtrlBundle(VFMV_F_S)
  val vfmv_s_f = CtrlBundle(VFMV_S_F)
  val vslideup_vx = CtrlBundle(VSLIDEUP_VX)
  val vslideup_vi = CtrlBundle(VSLIDEUP_VI)
  val vslidedown_vx = CtrlBundle(VSLIDEDOWN_VX)
  val vslidedown_vi = CtrlBundle(VSLIDEDOWN_VI)
  val vslide1up_vx = CtrlBundle(VSLIDE1UP_VX)
  val vfslide1up_vf = CtrlBundle(VFSLIDE1UP_VF)
  val vslide1down_vx = CtrlBundle(VSLIDE1DOWN_VX)
  val vfslide1down_vf = CtrlBundle(VFSLIDE1DOWN_VF)
  val vrgather_vv = CtrlBundle(VRGATHER_VV)
  val vrgatherei16_vv = CtrlBundle(VRGATHEREI16_VV)
  val vrgather_vx = CtrlBundle(VRGATHER_VX)
  val vrgather_vi = CtrlBundle(VRGATHER_VI)
  val vcompress_vm = CtrlBundle(VCOMPRESS_VM)
  val vmv1r_v = CtrlBundle(VMV1R_V)
  val vmv2r_v = CtrlBundle(VMV2R_V)
  val vmv4r_v = CtrlBundle(VMV4R_V)
  val vmv8r_v = CtrlBundle(VMV8R_V)

  val vdivu_vv = CtrlBundle(VDIVU_VV)
  val vdivu_vx = CtrlBundle(VDIVU_VX)
  val vdiv_vv = CtrlBundle(VDIV_VV)
  val vdiv_vx = CtrlBundle(VDIV_VX)
  val vremu_vv = CtrlBundle(VREMU_VV)
  val vremu_vx = CtrlBundle(VREMU_VX)
  val vrem_vv = CtrlBundle(VREM_VV)
  val vrem_vx = CtrlBundle(VREM_VX)
}

object UtilFuncs {
  //string format convert
  def stringconvert(str:String) = {
    val stringpart = str.trim.split("0x")
    "h"+stringpart(1)
  }

  def removespace(str:String) = {
    val stringtemp = str.split(" ")
    var stringout = ""
    for(i <- 0 until stringtemp.length){
      stringout = stringout + stringtemp(i)
    }
    stringout
  }

  def multilmuldatahandle(str:String) = {
    val stringtemp = str.split(" ")
    var stringout = stringtemp
    for(i <- 0 until stringtemp.length/2){
      stringout(i) = "h" + stringtemp(i*2) + stringtemp(i*2+1)
    }
    val temp = stringout(0).split("0x")
    stringout(0) = "h" + temp(1)
    stringout
  }

  def lmulconvert(lmul:String) = {
    var lmulreg = 0
    if(lmul=="0.125000")
       lmulreg = 5
    if(lmul=="0.250000")
       lmulreg = 6
    if(lmul=="0.500000")
       lmulreg = 7
    if(lmul=="1.000000")
       lmulreg = 0
    if(lmul=="2.000000")
       lmulreg = 1
    if(lmul=="4.000000")
       lmulreg = 2
    if(lmul=="8.000000")
       lmulreg = 3
    lmulreg
  }

  /*def srctypeconvert(sew:String,sing:String) = {
    var ty = 15
    if((sing=="s") && (sew=="8")){
      ty = 4
    }
    if((sing=="s") && (sew=="16")){
      ty = 5
    }
    if((sing=="s") && (sew=="32")){
      ty = 6
    }
    if((sing=="s") && (sew=="64")){
      ty = 7
    }
    if((sing=="u") && (sew=="64")){
      ty = 3
    }
    if((sing=="u") && (sew=="32")){
      ty = 2
    }
    if((sing=="u") && (sew=="16")){
      ty = 1
    }
    if((sing=="u") && (sew=="8")){
      ty = 0
    }
    if((sing=="f") && (sew=="16")){
      ty = 9
    }
    if((sing=="f") && (sew=="32")){
      ty = 10
    }
    if((sing=="f") && (sew=="64")){
      ty = 11
    }
    ty
  }*/

  def vsewconvert(sew:String) : Int = {
    sew match {
      case "8" => return 0
      case "16" => return 1
      case "32" => return 2
      case "64" => return 3
    }
  }

  def vlremaincal(vl:String,num:Int,sew:Int) = {
    var vlremain = vl.toInt
    //println("vl",vlremain)
    vlremain = vlremain - (128/sew)*num
    if(vlremain < 0)
      vlremain = 0
    vlremain
  }

  /*def fsmsewconvert(sew:String) = {
    var ty = 0
    if(sew == "8")
      ty = 0
    if(sew == "16")
      ty = 1
    if(sew == "32")
      ty = 2
    if(sew == "64")
      ty = 3
    ty
  }*/
}

object ReadTxt {
  //parsing a txt file
  def readFromTxtByline(file:String) = {
    import scala.io.Source
    val source = Source.fromFile(file,"UTF-8")
    val lines = source.getLines().toArray
    source.close()
    lines
  }

  def hasVstart(lines : Array[String]) : Boolean = {
    var i : Int = 0
    for (line <- lines) {
      if (i >= 15) return false
      if (line.contains("VSTART")) {
        return true
      }
      i += 1
    }
    return false
  }

  def getEachInputNLines(lines : Array[String]) : Int = {
    val INVALID = -1
    var i : Int = 0
    for (line <- lines) {
      if (i >= 30) return INVALID
      if (line.contains("------")) {
        return i + 1
      }
      i += 1
    }
    return INVALID
  }

  def getNEachAssignedLines(n_lines : Int, self_idx : Int, n : Int, 
                      each_input_n_lines : Int) : Int = {
    var n_inputs = n_lines / each_input_n_lines
    var each_n_inputs = n_inputs / n
    if(n_inputs % n > 0) {
      each_n_inputs += 1
    }
    return each_n_inputs * each_input_n_lines
  }

  def KeyFileUtil(array:Array[String]) = {
    var keyMapList = Map[String, String]()
    var keyMapList2 = Map[Int,Map[String,String]]()
    var number = 0
    for (i <- 0 until array.length) {
      val lineArray = array(i).trim.split("=")
      if(lineArray.size==2){
        keyMapList = keyMapList ++ Map(lineArray(0).trim -> lineArray(1))
      }else{
        number = number + 1 
      }
      keyMapList2 = keyMapList2 ++ Map(number -> keyMapList)
      if(lineArray(0).equals("--------------------------test_cnt")) {
        number = number + 1
      }
    }
    keyMapList2
  }
}

trait VAluBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  def vIntTestAll(sim:Map[Int,Map[String,String]],ctrl:TestCtrlBundleBase,s:String, tb:TestBehavior, j:Int = -1): Unit = {
    var testName = "pass the test: " + tb.getInstid() + " lmul ls 1"
    if (j != -1) testName += s" datasplit $j"
    it should s"$testName" in {
      test(tb.getDut()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        tb.test_init(dut)
        val nameList=sim.map(_._1)
        println(s"Starting test for ${tb.getInstid()}, lmul <= 1")
        println("test input counts",nameList.max+1)
        var i = 0
        do{
          val vflmul = sim(i).get("vflmul").get
          if((vflmul != "2.000000") && (vflmul != "4.000000") && (vflmul) != "8.000000"){
            println("lmul <= 1, id: ", i)
            tb.testSingle(sim(i), ctrl, s, dut)
          }
          i = i + 1
        }while(i <= nameList.max)

        println(s"${tb.getInstid()}, lmul <= 1 tests are done.")
        tb.setLmulLsOneDone()
        Dump.recordDone(s"${tb.getInstid()}, lmul <= 1")
      }
    }
  }

  def vsalu(sim:Map[Int,Map[String,String]], ctrl:TestCtrlBundleBase, s:String, tb:TestBehavior, j:Int = -1): Unit = {
    var testName = "pass the test: " + tb.getInstid() + " lmul gt 1"
    if (j != -1) testName += s" datasplit $j"
    it should s"$testName" in {
      test(tb.getDut()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        if (tb.getLmulLsOneDone()) {
          // assume lmul <= 1 tests are done before
          tb.test_init(dut)
        
          val nameList=sim.map(_._1)
          println(s"Starting test for ${tb.getInstid()}, lmul > 1")
          println("test counts:",nameList.max)
          for(i <- 0 until nameList.max+1){
            val vflmul = sim(i).get("vflmul").get
            if((vflmul == "2.000000") || (vflmul == "4.000000") || (vflmul) == "8.000000"){
              println("lmul > 1, id: ", i)
              tb.testMultiple(sim(i), ctrl, s, dut)
            }
          }
          println(s"${tb.getInstid()}, lmul > 1 tests are done.")
          Dump.recordDone(s"${tb.getInstid()}, lmul > 1")
        } else {
          println(s"lmulLsOneDone has not been set. Is lmul <= 1 tests failed for ${tb.getInstid()}?")
        }
      }
    }
  }

}

object Logger {
  def printvds(dut_vd : String, expected_vd : String) : Unit = {
    println("dut:\t\t" + dut_vd + "\nexpected: \t" + expected_vd)
  }
}

object Dump {
  val DUMP = true // switch

  val name = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd_kk_mm"))
  // var fileDire = "./src/test/scala/log/"
  var fileDire = "./src/test/scala/auto/IncorrectInputs/"
  var fileName = fileDire + s"${name}.txt"
  var incorrectDataFilePath = fileDire + s"${name}_data"
  Files.createDirectories(Paths.get(incorrectDataFilePath));
  var incorrectInstFilename = ""
  var doneInstFilename = ""

  def updateIncorrDataDire(dire : String) = {
    fileDire = dire
    fileName = fileDire + s"${name}.txt"
    incorrectDataFilePath = fileDire + s"${name}_data"
    Files.createDirectories(Paths.get(incorrectDataFilePath));
  }

  def hasInst(file: String, instid: String): Boolean = {
    val source = Source.fromFile(file)
    try {
      source.getLines().exists(_.trim == instid)
    } finally {
      source.close()
    }
  }

  def dump(map : Map[String,String], instid : String, dut_out : String = "", golden_vd : String = "", any_str : String = "", fault_wb : String = "") {
    if (!DUMP) return
    // val currentDate = LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd"))
    val currentDatetime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd_kk_mm"))

    val file = new File(fileName)
    file.createNewFile()
    var writer = new FileWriter(file, true)
    writer.write("##############################################################################################\n")
    writer.write(s"$instid ($any_str, $currentDatetime)\n")
    writer.write("=========================================================================\n")
    map.foreach {
        case (key, value) => writer.write(s"$key=$value\n")
    }
    writer.write("=========================================================================\n")
    writer.write(s"Incorrect input:\n")
    println("fault_wb", fault_wb)
    if (!fault_wb.equals("")) {
      writer.write(s"faulty wb index: ${fault_wb}\n")
    }
    writer.write(s"dut: \t\t\t${dut_out}\n")
    writer.write(s"expected: \t\t${golden_vd}\n")
    writer.write(s"##############################################################################################\n")
    writer.close()

    val incorrectDataFile = new File(s"${incorrectDataFilePath}/${instid.replaceAll("_", ".")}.data")
    incorrectDataFile.createNewFile()
    writer = new FileWriter(incorrectDataFile, true)
    var sepkey : String = ""
    var sepValue : String = ""
    map.foreach {
        case (key, value) => {
          if (!key.contains("------")) {
            writer.write(s"$key=$value\n")
          } else {
            sepkey = key
            sepValue = value
          }
        }
    }
    if (sepkey.length != 0) {
      writer.write(s"$sepkey=$sepValue\n")
    } else {
      writer.write("---------------------------------------------------\n")
    }
    writer.close()

    recordIncorrectInst(instid)
  }

  def recordIncorrectInst(instid : String) {
    if(!incorrectInstFilename.equals("")) {
      val file1 = new File(incorrectInstFilename)
      file1.createNewFile()

      val instFailedBefore = hasInst(incorrectInstFilename, instid)

      val writer1 = new FileWriter(file1, true)
      if(!instFailedBefore) {
        writer1.write(s"$instid\n")
      }
      writer1.close()
    }
  }

  def recordDone(str : String) {
    if (!DUMP) return
    if(!doneInstFilename.equals("")) {
      val file1 = new File(doneInstFilename)
      file1.createNewFile()

      val writer1 = new FileWriter(file1, true)
      writer1.write(s"$str\n")
      writer1.close()
    }
  }
}

object TestResults {

  case class InstTestRes(
    inst : String,
    failed : Boolean = false,
    fault_dut_out : String = "",
    fault_golden_out : String = "",
    fault_fsm_wb_idx : String = "",
  )

  var results: ConCurMap[String, InstTestRes] = new ConcurrentHashMap().asScala

  def initResults(insts : Array[String]) : Unit = {
    for(inst <- insts) {
      results(inst) = InstTestRes(inst)
    }
  }

  def addResult(testRes : InstTestRes) : Unit = {
    results(testRes.inst) = testRes
  }

  def checkResult(inst : String) : Boolean = {
    return results(inst).failed
  }

  def print() : Unit = {
    println("TestResults: ")
    results.foreach {
        case (key, value) => println(s"$key=${value.failed}, ${value.fault_dut_out}")
    }
  }
}

object Datapath {
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_23/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_29/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_30/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_1/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_4/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_6/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/10_10/unittest/"
  val testdataRoot = "/home/maoting/nanhu/testdata/10_13/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/debug/"
  //val testdataRoot = "C:\\kou\\XS_Vector_Unit\\src\\test\\scala\\unittest\\"
  // val testdataRoot = "/home/kou/unittest/"
  // val testdataRoot = "/home/maoting/xs-unittest/testdata/"
}


class VAluSpec extends AnyFlatSpec with ChiselScalatestTester
  with BundleGenHelper with VAluBehavior {
  behavior of "Int fixP test"

  println(s"============\nTEST STARTED\n============")
  if(Dump.DUMP) println(s"DUMP is turned on. Incorrect inputs will be saved to ${Dump.fileName}")

  var tbs : Seq[TestBehavior] = Seq(

    // new VredsumvsTestBehavior,
    new VfaddvvTestBehavior,

    // new VslideupvxFSMTestBehavior,
    // new VfaddvvTestBehavior,
    // new VnclipwiTestBehavior,
    // new VnclipwxTestBehavior,
    // new VaddvxTestBehavior,
    // new VaddviTestBehavior,

    // new VmadcvvTestBehavior,

    // new VaddvvTestBehavior,
    //  new Vmv8rTestBehavior,
    // new VaddvvTestBehavior,
    // new VminuvvTestBehavior,
    // new VmaxuvvTestBehavior

    // new VmadcvxmTestBehavior,

    // new VmvxsTestBehavior,
    // new VfmvfsTestBehavior,

    // new VfncvtxufwTestBehavior,

    //   // FSM ==================================
    // new VslideupvxFSMTestBehavior,
    //   new VslidedownvxFSMTestBehavior,
    //   new Vslide1upvxFSMTestBehavior,
    //   new Vslide1downvxFSMTestBehavior,
    //   new VrgathervvFSMTestBehavior,
    //   new VrgathervxFSMTestBehavior,
    //  new Vrgatherei16vvFSMTestBehavior,
    //   new VcompressvmFSMTestBehavior,
    // new VslideupviFSMTestBehavior,
    // new VslidedownviFSMTestBehavior,
    // new VrgatherviFSMTestBehavior,
    // new Vfslide1upvfFSMTestBehavior,
    // new Vfslide1downvfFSMTestBehavior,

    //   // Reduction ===============================
    //   new VredsumvsTestBehavior,
    //   new VredmaxuvsTestBehavior,
    //   new VredmaxvsTestBehavior,
    //   new VredminuvsTestBehavior,
    //   new VredminvsTestBehavior,
    //   new VredandvsTestBehavior,
    //   new VredorvsTestBehavior,
    //   new VredxorvsTestBehavior,
    //   new VwredsumuvsTestBehavior,
    //   new VwredsumvsTestBehavior,

    //   // Mask ==================================
    //   new VmsbfmTestBehavior,
    //   new VmsifmTestBehavior,
    //   new VmsofmTestBehavior,

    //   new VmandmmTestBehavior,
    //   new VmnandmmTestBehavior,
    //   new VmandnmmTestBehavior,
    //   new VmxormmTestBehavior,
    //   new VmormmTestBehavior,
    //   new VmnormmTestBehavior,
    //   new VmornmmTestBehavior,
    //   new VmxnormmTestBehavior,

    //   new VfirstmTestBehavior,

    //   new VcpopmTestBehavior,
    //   new VidvTestBehavior,
    //   new ViotamTestBehavior,
    // new VfmvsfTestBehavior,
    // new VmadcvxmTestBehavior,

    // FP ===============================
    // new VfredosumvsTestBehavior, // vfredosum.vs
    // new VfredusumvsTestBehavior, // vfredusum.vs
    // new VfredmaxvsTestBehavior, // vfredmax.vs
    // new VfredminvsTestBehavior, // vfredmin.vs

    // new VfwredosumvsTestBehavior, // vfwredosum.vs
    // new VfwredusumvsTestBehavior, // vfwredusum.vs

    // new VfncvtxufwTestBehavior, // vfncvt.xu.f.w
    // new VfncvtxfwTestBehavior, // vfncvt.x.f.w
    // new VfncvtrtzxufwTestBehavior, // vfncvt.rtz.xu.f.w
    // new VfncvtrtzxfwTestBehavior, // vfncvt.rtz.x.f.w
    // new VfncvtfxuwTestBehavior, // vfncvt.f.xu.w
    // new VfncvtfxwTestBehavior, // vfncvt.f.x.w

    // new VfncvtffwTestBehavior, // vfncvt.f.f.w
    // new VfncvtrodffwTestBehavior, // vfncvt.rod.f.f.w

    // new VfaddvvTestBehavior, // vfadd.vv
    // new VfsubvvTestBehavior, // vfsub.vv

    // new VfmulvvTestBehavior, // vfmul.vv
    // new VfmaccvvTestBehavior, // vfmacc.vv
    // new VfnmaccvvTestBehavior, // vfnmacc.vv
    // new VfmsacvvTestBehavior, // vfmsac.vv
    // new VfnmsacvvTestBehavior, // vfnmsac.vv
    // new VfmaddvvTestBehavior, // vfmadd.vv
    // new VfnmaddvvTestBehavior, // vfnmadd.vv
    // new VfmsubvvTestBehavior, // vfmsub.vv
    // new VfnmsubvvTestBehavior, // vfnmsub.vv

    // new VfminvvTestBehavior, // vfmin.vv
    // new VfmaxvvTestBehavior, // vfmax.vv

    // new VmfeqvvTestBehavior, // vmfeq.vv
    // new VmfnevvTestBehavior, // vmfne.vv
    // new VmfltvvTestBehavior, // vmflt.vv
    // new VmflevvTestBehavior, // vmfle.vv

    // new VfsgnjvvTestBehavior, // vfsgnj.vv
    // new VfsgnjnvvTestBehavior, // vfsgnjn.vv
    // new VfsgnjxvvTestBehavior, // vfsgnjx.vv

    // new VfaddvfTestBehavior, // vfadd.vf
    // new VfsubvfTestBehavior, // vfsub.vf

    // new VfmulvfTestBehavior, // vfmul.vf
    // new VfmaccvfTestBehavior, // vfmacc.vf
    // new VfnmaccvfTestBehavior, // vfnmacc.vf
    // new VfmsacvfTestBehavior, // vfmsac.vf
    // new VfnmsacvfTestBehavior, // vfnmsac.vf
    // new VfmaddvfTestBehavior, // vfmadd.vf
    // new VfnmaddvfTestBehavior, // vfnmadd.vf
    // new VfmsubvfTestBehavior, // vfmsub.vf
    // new VfnmsubvfTestBehavior, // vfnmsub.vf

    // new VfminvfTestBehavior, // vfmin.vf
    // new VfmaxvfTestBehavior, // vfmax.vf

    // new VmfeqvfTestBehavior, // vmfeq.vf
    // new VmfnevfTestBehavior, // vmfne.vf
    // new VmfltvfTestBehavior, // vmflt.vf
    // new VmflevfTestBehavior, // vmfle.vf

    // new VfsgnjvfTestBehavior, // vfsgnj.vf
    // new VfsgnjnvfTestBehavior, // vfsgnjn.vf
    // new VfsgnjxvfTestBehavior, // vfsgnjx.vf

    // new VfrsubvfTestBehavior, // vfrsub.vf
    // new VmfgtvfTestBehavior, // vmfgt.vf
    // new VmfgevfTestBehavior, // vmfge.vf
    // new VfclassvTestBehavior, // vfclass.v
    // new VfmergevfmTestBehavior, // vfmerge.vfm
    // new VfmvTestBehavior, // vfmv.v.f
    // new Vfrsqrt7vTestBehavior, // vfrsqrt7.v
    // new Vfrec7vTestBehavior, // vfrec7.v


    // new VfcvtxufvTestBehavior, // vfcvt.xu.f.v
    // new VfcvtxfvTestBehavior, // vfcvt.x.f.v
    // new VfcvtrtzxufvTestBehavior, // vfcvt.rtz.xu.f.v
    // new VfcvtrtzxfvTestBehavior, // vfcvt.rtz.x.f.v
    // new VfcvtfxuvTestBehavior, // vfcvt.f.xu.v
    // new VfcvtfxvTestBehavior, // vfcvt.f.x.v

    // new VfwaddwvTestBehavior, // vfwadd.wv
    // new VfwsubwvTestBehavior, // vfwsub.wv
    // new VfwaddwfTestBehavior, // vfwadd.wf
    // new VfwsubwfTestBehavior, // vfwsub.wf

    // new VfwaddvvTestBehavior, // vfwadd.vv
    // new VfwsubvvTestBehavior, // vfwsub.vv
    // new VfwmaccvvTestBehavior, // vfwmacc.vv
    // new VfwnmaccvvTestBehavior, // vfwnmacc.vv
    // new VfwmsacvvTestBehavior, // vfwmsac.vv
    // new VfwnmsacvvTestBehavior, // vfwnmsac.vv
    // new VfwmulvvTestBehavior, // vfwmul.vv

    // new VfwaddvfTestBehavior, // vfwadd.vf
    // new VfwsubvfTestBehavior, // vfwsub.vf
    // new VfwmaccvfTestBehavior, // vfwmacc.vf
    // new VfwnmaccvfTestBehavior, // vfwnmacc.vf
    // new VfwmsacvfTestBehavior, // vfwmsac.vf
    // new VfwnmsacvfTestBehavior, // vfwnmsac.vf
    // new VfwmulvfTestBehavior, // vfwmul.vf

    // new VfwcvtffvTestBehavior, // vfwcvt.f.f.v
    // new VfwcvtxufvTestBehavior, // vfwcvt.xu.f.v
    // new VfwcvtxfvTestBehavior, // vfwcvt.x.f.v
    // new VfwcvtrtzxufvTestBehavior, // vfwcvt.rtz.xu.f.v
    // new VfwcvtrtzxfvTestBehavior, // vfwcvt.rtz.x.f.v
    // new VfwcvtfxuvTestBehavior, // vfwcvt.f.xu.v
    // new VfwcvtfxvTestBehavior, // vfwcvt.f.x.v

    // Div ========================================
    // new VdivvvTestBehavior,
    // new VdivuvvTestBehavior,
    // new VdivvxTestBehavior,
    // new VdivuvxTestBehavior,

    // new VremvvTestBehavior,
    // new VremuvvTestBehavior,
    // new VremvxTestBehavior,
    // new VremuvxTestBehavior,

    // new VfdivvvTestBehavior,
    // new VfdivvfTestBehavior,

    // new VfrdivvfTestBehavior,

    // new VfsqrtvTestBehavior,
  )

  var testInsts : Array[String] = Array()

  val param = sys.props.getOrElse("insfile", "")
  val incorrectInstFile = sys.props.getOrElse("incorrInsts", "")
  val doneInstFile = sys.props.getOrElse("doneInsts", "")
  val incorrectInputFile = sys.props.getOrElse("incorrInput", "")

  val incorrectDataDireFile = sys.props.getOrElse("incorrDataDire", "")

  val dataSplitIx = sys.props.getOrElse("dataSplitIx", "0").toInt
  val dataSplitN = sys.props.getOrElse("dataSplitN", "1").toInt
  val dataSplitInst = sys.props.getOrElse("dataSplitInst", "")


  if(!incorrectInputFile.equals("")) Dump.fileName = incorrectInputFile
  if(!incorrectInstFile.equals("")) Dump.incorrectInstFilename = incorrectInstFile
  if(!doneInstFile.equals("")) Dump.doneInstFilename = doneInstFile
  if(!incorrectDataDireFile.equals("")) Dump.updateIncorrDataDire(incorrectDataDireFile)

  val dataSplitMode : Boolean = !dataSplitInst.equals("")

  if (!dataSplitMode) {
    // normal, no data splitting
    if (param.equals("")) {
      println("WARNING: No file specified.. Using TestBehaviors specified in VIntFixpAluSpec.scala")
      for (tb <- tbs) {
        testInsts = testInsts :+ tb.getInstid()
      }
    } else {
      tbs = Seq()
      println("Loading instructions from file: ", param)
      testInsts = ReadTxt.readFromTxtByline(param).distinct
      for (inst <- testInsts) {
        if (!inst.equals("")) {
          println(s".. Adding $inst")
          tbs = tbs :+ TBMap.tbMap(inst)()
        }
      }
    }
  } else {
    // single inst, data splitting
    tbs = Seq(TBMap.tbMap(dataSplitInst)())
    println(s"Testing $dataSplitInst with Data Split .. $dataSplitIx / $dataSplitN")
  }

  // TestResults.initResults(testInsts)
  // TestResults.print()

  val futures = new ListBuffer[Future[Unit]]
  
  for(i <- 0 until tbs.length) {
    // params
    val tb = tbs(i)

    // test code
    // tb.changeSwitch()
    val test_file = tb.getTestfilePath()
    val inst = tb.getCtrlBundle()
    val sign = tb.getSign()

    if (Files.exists(Paths.get(test_file))) {
      val key = ReadTxt.readFromTxtByline(test_file)
      val hasvstart1 = ReadTxt.hasVstart(key)
      println(s"hasVstart $hasvstart1")
      var each_input_n_lines = ReadTxt.getEachInputNLines(key)
      println("each_input_n_lines", each_input_n_lines)

      var dataN = 1
      var j = 0
      if (dataSplitMode) {
        dataN = dataSplitN
        j = dataSplitIx
      }

      val each_asisgned_lines = ReadTxt.getNEachAssignedLines(key.length, j, dataN, each_input_n_lines)
      val startingIndex = j * each_asisgned_lines
      if (startingIndex < key.length) {
        println(s"Data Split $j / $dataN: $startingIndex + $each_asisgned_lines, total ${key.length}")
        // val testFuture : Future[Unit] = Future 
        
        val keymap = ReadTxt.KeyFileUtil(key.slice(startingIndex, startingIndex + each_asisgned_lines))
        // println(s"$j Future is looking at ${tb.getInstid()}, $startingIndex + $each_asisgned_lines")
        var reportIx = -1
        if (dataSplitMode) {
          reportIx = j
        }
        it should behave like vIntTestAll(keymap, inst, sign, tb, reportIx)
        it should behave like vsalu(keymap, inst, sign, tb, reportIx)
        // println("wtf??????????")
      }
    } else {
      println(s"Data file does not exist for instruction: ${tb.getInstid()} , skipping")
      Dump.recordIncorrectInst(tb.getInstid())
    }
  }
}

