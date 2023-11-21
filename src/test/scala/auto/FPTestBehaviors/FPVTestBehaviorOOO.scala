package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.util.control.Breaks._

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.VInstructions._


// VN ============================================
class VfncvtxufwTestBehaviorOOO extends VfTestBehavior("vfncvt.xu.f.w.data", ctrlBundles.vfncvt_xu_f_w, "u", "vfncvt_xu_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x10), ooo=true) {}
class VfncvtxfwTestBehaviorOOO extends VfTestBehavior("vfncvt.x.f.w.data", ctrlBundles.vfncvt_x_f_w, "u", "vfncvt_x_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x11), ooo=true) {}
class VfncvtfxuwTestBehaviorOOO extends VfTestBehavior("vfncvt.f.xu.w.data", ctrlBundles.vfncvt_f_xu_w, "u", "vfncvt_f_xu_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x12), ooo=true) {}
class VfncvtfxwTestBehaviorOOO extends VfTestBehavior("vfncvt.f.x.w.data", ctrlBundles.vfncvt_f_x_w, "u", "vfncvt_f_x_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x13), ooo=true) {}
class VfncvtffwTestBehaviorOOO extends VfTestBehavior("vfncvt.f.f.w.data", ctrlBundles.vfncvt_f_f_w, "u", "vfncvt_f_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x14), ooo=true) {}
class VfncvtrodffwTestBehaviorOOO extends VfTestBehavior("vfncvt.rod.f.f.w.data", ctrlBundles.vfncvt_rod_f_f_w, "u", "vfncvt_rod_f_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x15), ooo=true) {}
class VfncvtrtzxufwTestBehaviorOOO extends VfTestBehavior("vfncvt.rtz.xu.f.w.data", ctrlBundles.vfncvt_rtz_xu_f_w, "u", "vfncvt_rtz_xu_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x16), ooo=true) {}
class VfncvtrtzxfwTestBehaviorOOO extends VfTestBehavior("vfncvt.rtz.x.f.w.data", ctrlBundles.vfncvt_rtz_x_f_w, "u", "vfncvt_rtz_x_f_w_ooo", () => new NarrowFPResult(), vn=true, vs1encoding=Some(0x17), ooo=true) {}

// Normal ============================================
// vv ================================================================================================================
class VfaddvvTestBehaviorOOO extends VfTestBehavior("vfadd.vv.data", ctrlBundles.vfadd_vv, "-", "vfadd_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsubvvTestBehaviorOOO extends VfTestBehavior("vfsub.vv.data", ctrlBundles.vfsub_vv, "-", "vfsub_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfmulvvTestBehaviorOOO extends VfTestBehavior("vfmul.vv.data", ctrlBundles.vfmul_vv, "-", "vfmul_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaccvvTestBehaviorOOO extends VfTestBehavior("vfmacc.vv.data", ctrlBundles.vfmacc_vv, "-", "vfmacc_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmaccvvTestBehaviorOOO extends VfTestBehavior("vfnmacc.vv.data", ctrlBundles.vfnmacc_vv, "-", "vfnmacc_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmsacvvTestBehaviorOOO extends VfTestBehavior("vfmsac.vv.data", ctrlBundles.vfmsac_vv, "-", "vfmsac_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmsacvvTestBehaviorOOO extends VfTestBehavior("vfnmsac.vv.data", ctrlBundles.vfnmsac_vv, "-", "vfnmsac_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaddvvTestBehaviorOOO extends VfTestBehavior("vfmadd.vv.data", ctrlBundles.vfmadd_vv, "-", "vfmadd_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmaddvvTestBehaviorOOO extends VfTestBehavior("vfnmadd.vv.data", ctrlBundles.vfnmadd_vv, "-", "vfnmadd_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmsubvvTestBehaviorOOO extends VfTestBehavior("vfmsub.vv.data", ctrlBundles.vfmsub_vv, "-", "vfmsub_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmsubvvTestBehaviorOOO extends VfTestBehavior("vfnmsub.vv.data", ctrlBundles.vfnmsub_vv, "-", "vfnmsub_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfminvvTestBehaviorOOO extends VfTestBehavior("vfmin.vv.data", ctrlBundles.vfmin_vv, "-", "vfmin_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaxvvTestBehaviorOOO extends VfTestBehavior("vfmax.vv.data", ctrlBundles.vfmax_vv, "-", "vfmax_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfsgnjvvTestBehaviorOOO extends VfTestBehavior("vfsgnj.vv.data", ctrlBundles.vfsgnj_vv, "s", "vfsgnj_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsgnjnvvTestBehaviorOOO extends VfTestBehavior("vfsgnjn.vv.data", ctrlBundles.vfsgnjn_vv, "s", "vfsgnjn_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsgnjxvvTestBehaviorOOO extends VfTestBehavior("vfsgnjx.vv.data", ctrlBundles.vfsgnjx_vv, "s", "vfsgnjx_vv_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

// vf ================================================================================================================
class VfaddvfTestBehaviorOOO extends VfTestBehavior("vfadd.vf.data", ctrlBundles.vfadd_vf, "-", "vfadd_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsubvfTestBehaviorOOO extends VfTestBehavior("vfsub.vf.data", ctrlBundles.vfsub_vf, "-", "vfsub_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfmulvfTestBehaviorOOO extends VfTestBehavior("vfmul.vf.data", ctrlBundles.vfmul_vf, "-", "vfmul_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaccvfTestBehaviorOOO extends VfTestBehavior("vfmacc.vf.data", ctrlBundles.vfmacc_vf, "-", "vfmacc_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmaccvfTestBehaviorOOO extends VfTestBehavior("vfnmacc.vf.data", ctrlBundles.vfnmacc_vf, "-", "vfnmacc_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmsacvfTestBehaviorOOO extends VfTestBehavior("vfmsac.vf.data", ctrlBundles.vfmsac_vf, "-", "vfmsac_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmsacvfTestBehaviorOOO extends VfTestBehavior("vfnmsac.vf.data", ctrlBundles.vfnmsac_vf, "-", "vfnmsac_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaddvfTestBehaviorOOO extends VfTestBehavior("vfmadd.vf.data", ctrlBundles.vfmadd_vf, "-", "vfmadd_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmaddvfTestBehaviorOOO extends VfTestBehavior("vfnmadd.vf.data", ctrlBundles.vfnmadd_vf, "-", "vfnmadd_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmsubvfTestBehaviorOOO extends VfTestBehavior("vfmsub.vf.data", ctrlBundles.vfmsub_vf, "-", "vfmsub_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfnmsubvfTestBehaviorOOO extends VfTestBehavior("vfnmsub.vf.data", ctrlBundles.vfnmsub_vf, "-", "vfnmsub_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfminvfTestBehaviorOOO extends VfTestBehavior("vfmin.vf.data", ctrlBundles.vfmin_vf, "-", "vfmin_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmaxvfTestBehaviorOOO extends VfTestBehavior("vfmax.vf.data", ctrlBundles.vfmax_vf, "-", "vfmax_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class VfsgnjvfTestBehaviorOOO extends VfTestBehavior("vfsgnj.vf.data", ctrlBundles.vfsgnj_vf, "s", "vfsgnj_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsgnjnvfTestBehaviorOOO extends VfTestBehavior("vfsgnjn.vf.data", ctrlBundles.vfsgnjn_vf, "s", "vfsgnjn_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfsgnjxvfTestBehaviorOOO extends VfTestBehavior("vfsgnjx.vf.data", ctrlBundles.vfsgnjx_vf, "s", "vfsgnjx_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

// others ================================================================================================================
class VfrsubvfTestBehaviorOOO extends VfTestBehavior("vfrsub.vf.data", ctrlBundles.vfrsub_vf, "-", "vfrsub_vf_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmergevfmTestBehaviorOOO extends VfTestBehavior("vfmerge.vfm.data", ctrlBundles.vfmerge_vfm, "-", "vfmerge_vfm_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}
class VfmvTestBehaviorOOO extends VfTestBehavior("vfmv.v.f.data", ctrlBundles.vfmv_v_f, "-", "vfmv_v_f_ooo", () => new NormalFPResult(), normal=true, ooo=true) {}

class Vfrsqrt7vTestBehaviorOOO extends VfTestBehavior("vfrsqrt7.v.data", ctrlBundles.vfrsqrt7_v, "-", "vfrsqrt7_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x04), ooo=true) {}
class Vfrec7vTestBehaviorOOO extends VfTestBehavior("vfrec7.v.data", ctrlBundles.vfrec7_v, "-", "vfrec7_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x05), ooo=true) {}
class VfclassvTestBehaviorOOO extends VfTestBehavior("vfclass.v.data", ctrlBundles.vfclass_v, "-", "vfclass_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x10), ooo=true) {}


class VfcvtxufvTestBehaviorOOO extends VfTestBehavior("vfcvt.xu.f.v.data", ctrlBundles.vfcvt_xu_f_v, "-", "vfcvt_xu_f_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x00), ooo=true) {}
class VfcvtxfvTestBehaviorOOO extends VfTestBehavior("vfcvt.x.f.v.data", ctrlBundles.vfcvt_x_f_v, "-", "vfcvt_x_f_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x01), ooo=true) {}
class VfcvtfxuvTestBehaviorOOO extends VfTestBehavior("vfcvt.f.xu.v.data", ctrlBundles.vfcvt_f_xu_v, "-", "vfcvt_f_xu_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x02), ooo=true) {}
class VfcvtfxvTestBehaviorOOO extends VfTestBehavior("vfcvt.f.x.v.data", ctrlBundles.vfcvt_f_x_v, "-", "vfcvt_f_x_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x03), ooo=true) {}
class VfcvtrtzxufvTestBehaviorOOO extends VfTestBehavior("vfcvt.rtz.xu.f.v.data", ctrlBundles.vfcvt_rtz_xu_f_v, "-", "vfcvt_rtz_xu_f_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x06), ooo=true) {}
class VfcvtrtzxfvTestBehaviorOOO extends VfTestBehavior("vfcvt.rtz.x.f.v.data", ctrlBundles.vfcvt_rtz_x_f_v, "-", "vfcvt_rtz_x_f_v_ooo", () => new NormalFPResult(), normal=true, vs1encoding=Some(0x07), ooo=true) {}

// vw ====================================================================
class VfwaddwvTestBehaviorOOO extends VfTestBehavior("vfwadd.wv.data", ctrlBundles.vfwadd_wv, "s", "vfwadd_wv_ooo", () => new NormalFPResult(), vw=true, vfwvv=false, ooo=true) {}
class VfwsubwvTestBehaviorOOO extends VfTestBehavior("vfwsub.wv.data", ctrlBundles.vfwsub_wv, "s", "vfwsub_wv_ooo", () => new NormalFPResult(), vw=true, vfwvv=false, ooo=true) {}
class VfwaddwfTestBehaviorOOO extends VfTestBehavior("vfwadd.wf.data", ctrlBundles.vfwadd_wf, "s", "vfwadd_wf_ooo", () => new NormalFPResult(), vw=true, vfwvv=false, ooo=true) {}
class VfwsubwfTestBehaviorOOO extends VfTestBehavior("vfwsub.wf.data", ctrlBundles.vfwsub_wf, "s", "vfwsub_wf_ooo", () => new NormalFPResult(), vw=true, vfwvv=false, ooo=true) {}

class VfwaddvvTestBehaviorOOO extends VfTestBehavior("vfwadd.vv.data", ctrlBundles.vfwadd_vv, "s", "vfwadd_vv_ooo", () => new NormalFPResult(), vw=true, vfwvv=true, ooo=true) {}
class VfwsubvvTestBehaviorOOO extends VfTestBehavior("vfwsub.vv.data", ctrlBundles.vfwsub_vv, "s", "vfwsub_vv_ooo", () => new NormalFPResult(), vw=true, vfwvv=true, ooo=true) {}

class VfwaddvfTestBehaviorOOO extends VfTestBehavior("vfwadd.vf.data", ctrlBundles.vfwadd_vf, "s", "vfwadd_vf_ooo", () => new NormalFPResult(), vw=true, vfwvv=true, ooo=true) {}
class VfwsubvfTestBehaviorOOO extends VfTestBehavior("vfwsub.vf.data", ctrlBundles.vfwsub_vf, "s", "vfwsub_vf_ooo", () => new NormalFPResult(), vw=true, vfwvv=true, ooo=true) {}

class VfwmaccvvTestBehaviorOOO extends VfTestBehavior("vfwmacc.vv.data", ctrlBundles.vfwmacc_vv, "s", "vfwmacc_vv_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwnmaccvvTestBehaviorOOO extends VfTestBehavior("vfwnmacc.vv.data", ctrlBundles.vfwnmacc_vv, "s", "vfwnmacc_vv_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwmsacvvTestBehaviorOOO extends VfTestBehavior("vfwmsac.vv.data", ctrlBundles.vfwmsac_vv, "s", "vfwmsac_vv_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwnmsacvvTestBehaviorOOO extends VfTestBehavior("vfwnmsac.vv.data", ctrlBundles.vfwnmsac_vv, "s", "vfwnmsac_vv_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwmulvvTestBehaviorOOO extends VfTestBehavior("vfwmul.vv.data", ctrlBundles.vfwmul_vv, "s", "vfwmul_vv_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwmaccvfTestBehaviorOOO extends VfTestBehavior("vfwmacc.vf.data", ctrlBundles.vfwmacc_vf, "s", "vfwmacc_vf_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwnmaccvfTestBehaviorOOO extends VfTestBehavior("vfwnmacc.vf.data", ctrlBundles.vfwnmacc_vf, "s", "vfwnmacc_vf_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwmsacvfTestBehaviorOOO extends VfTestBehavior("vfwmsac.vf.data", ctrlBundles.vfwmsac_vf, "s", "vfwmsac_vf_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwnmsacvfTestBehaviorOOO extends VfTestBehavior("vfwnmsac.vf.data", ctrlBundles.vfwnmsac_vf, "s", "vfwnmsac_vf_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}
class VfwmulvfTestBehaviorOOO extends VfTestBehavior("vfwmul.vf.data", ctrlBundles.vfwmul_vf, "s", "vfwmul_vf_ooo", () => new NormalFPResult(), vw=true, vfwmul_like=true, vfwvv=true, ooo=true) {}

class VfwcvtxufvTestBehaviorOOO extends VfTestBehavior("vfwcvt.xu.f.v.data", ctrlBundles.vfwcvt_xu_f_v, "s", "vfwcvt_xu_f_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x08), vfwvv=true, ooo=true) {}
class VfwcvtxfvTestBehaviorOOO extends VfTestBehavior("vfwcvt.x.f.v.data", ctrlBundles.vfwcvt_x_f_v, "s", "vfwcvt_x_f_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x09), vfwvv=true, ooo=true) {}
class VfwcvtfxuvTestBehaviorOOO extends VfTestBehavior("vfwcvt.f.xu.v.data", ctrlBundles.vfwcvt_f_xu_v, "s", "vfwcvt_f_xu_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x0A), vfwvv=true, ooo=true) {}
class VfwcvtfxvTestBehaviorOOO extends VfTestBehavior("vfwcvt.f.x.v.data", ctrlBundles.vfwcvt_f_x_v, "s", "vfwcvt_f_x_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x0B), vfwvv=true, ooo=true) {}
class VfwcvtffvTestBehaviorOOO extends VfTestBehavior("vfwcvt.f.f.v.data", ctrlBundles.vfwcvt_f_f_v, "s", "vfwcvt_f_f_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x0C), vfwvv=true, ooo=true) {}
class VfwcvtrtzxufvTestBehaviorOOO extends VfTestBehavior("vfwcvt.rtz.xu.f.v.data", ctrlBundles.vfwcvt_rtz_xu_f_v, "s", "vfwcvt_rtz_xu_f_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x0E), vfwvv=true, ooo=true) {}
class VfwcvtrtzxfvTestBehaviorOOO extends VfTestBehavior("vfwcvt.rtz.x.f.v.data", ctrlBundles.vfwcvt_rtz_x_f_v, "s", "vfwcvt_rtz_x_f_v_ooo", () => new NormalFPResult(), vw=true, vs1encoding=Some(0x0F), vfwvv=true, ooo=true) {}
