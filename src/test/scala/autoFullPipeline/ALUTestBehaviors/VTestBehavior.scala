package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters

class VaddvvTestBehavior extends VTestBehavior("vadd.vv.data", ctrlBundles.vadd_vv, "u", "vadd_vv") {}
class VsubvvTestBehavior extends VTestBehavior("vsub.vv.data", ctrlBundles.vsub_vv, "u", "vsub_vv") {}
class VandvvTestBehavior extends VTestBehavior("vand.vv.data", ctrlBundles.vand_vv, "u", "vand_vv") {}
class VorvvTestBehavior extends VTestBehavior("vor.vv.data", ctrlBundles.vor_vv, "u", "vor_vv") {}
class VxorvvTestBehavior extends VTestBehavior("vxor.vv.data", ctrlBundles.vxor_vv, "u", "vxor_vv") {}
class VsllvvTestBehavior extends VTestBehavior("vsll.vv.data", ctrlBundles.vsll_vv, "u", "vsll_vv") {}
class VsrlvvTestBehavior extends VTestBehavior("vsrl.vv.data", ctrlBundles.vsrl_vv, "u", "vsrl_vv") {}
class VsravvTestBehavior extends VTestBehavior("vsra.vv.data", ctrlBundles.vsra_vv, "u", "vsra_vv") {}
class VmaxvvTestBehavior extends VTestBehavior("vmax.vv.data", ctrlBundles.vmax_vv, "s", "vmax_vv") {}
class VmaxuvvTestBehavior extends VTestBehavior("vmaxu.vv.data", ctrlBundles.vmaxu_vv, "u", "vmaxu_vv") {}
class VminvvTestBehavior extends VTestBehavior("vmin.vv.data", ctrlBundles.vmin_vv, "s", "vmin_vv") {}
class VminuvvTestBehavior extends VTestBehavior("vminu.vv.data", ctrlBundles.vminu_vv, "u", "vminu_vv") {}

class VmergevvmTestBehavior extends VTestBehavior("vmerge.vvm.data", ctrlBundles.vmerge_vvm, "u", "vmerge_vvm") {}
class VmvvvTestBehavior extends VTestBehavior("vmv.v.v.data", ctrlBundles.vmv_v_v, "u", "vmv_v_v") {}

class VadcvvmTestBehavior extends VTestBehavior("vadc.vvm.data", ctrlBundles.vadc_vvm, "u", "vadc_vvm", _vm = Option(false)) {}
class VsbcvvmTestBehavior extends VTestBehavior("vsbc.vvm.data", ctrlBundles.vsbc_vvm, "u", "vsbc_vvm", _vm = Option(false)) {}

class VssubvvTestBehavior extends VTestBehavior("vssub.vv.data", ctrlBundles.vssub_vv, "s", "vssub_vv") {}
class VssubuvvTestBehavior extends VTestBehavior("vssubu.vv.data", ctrlBundles.vssubu_vv, "u", "vssubu_vv") {}
class VsaddvvTestBehavior extends VTestBehavior("vsadd.vv.data", ctrlBundles.vsadd_vv, "s", "vsadd_vv") {}
class VsadduvvTestBehavior extends VTestBehavior("vsaddu.vv.data", ctrlBundles.vsaddu_vv, "u", "vsaddu_vv") {}

class VssrlvvTestBehavior extends VTestBehavior("vssrl.vv.data", ctrlBundles.vssrl_vv, "u", "vssrl_vv") {}
class VssravvTestBehavior extends VTestBehavior("vssra.vv.data", ctrlBundles.vssra_vv, "u", "vssra_vv") {}

class VaaddvvTestBehavior extends VTestBehavior("vaadd.vv.data", ctrlBundles.vaadd_vv, "s", "vaadd_vv") {}
class VaadduvvTestBehavior extends VTestBehavior("vaaddu.vv.data", ctrlBundles.vaaddu_vv, "u", "vaaddu_vv") {}
class VasubvvTestBehavior extends VTestBehavior("vasub.vv.data", ctrlBundles.vasub_vv, "s", "vasub_vv") {}
class VasubuvvTestBehavior extends VTestBehavior("vasubu.vv.data", ctrlBundles.vasubu_vv, "u", "vasubu_vv") {}

class VaddvxTestBehavior extends VTestBehavior("vadd.vx.data", ctrlBundles.vadd_vx, "u", "vadd_vx") {}
class VsubvxTestBehavior extends VTestBehavior("vsub.vx.data", ctrlBundles.vsub_vx, "u", "vsub_vx") {}
class VandvxTestBehavior extends VTestBehavior("vand.vx.data", ctrlBundles.vand_vx, "u", "vand_vx") {}
class VorvxTestBehavior extends VTestBehavior("vor.vx.data", ctrlBundles.vor_vx, "u", "vor_vx") {}
class VxorvxTestBehavior extends VTestBehavior("vxor.vx.data", ctrlBundles.vxor_vx, "u", "vxor_vx") {}
class VsllvxTestBehavior extends VTestBehavior("vsll.vx.data", ctrlBundles.vsll_vx, "u", "vsll_vx") {}
class VsrlvxTestBehavior extends VTestBehavior("vsrl.vx.data", ctrlBundles.vsrl_vx, "u", "vsrl_vx") {}
class VsravxTestBehavior extends VTestBehavior("vsra.vx.data", ctrlBundles.vsra_vx, "u", "vsra_vx") {}
class VmaxvxTestBehavior extends VTestBehavior("vmax.vx.data", ctrlBundles.vmax_vx, "s", "vmax_vx") {}
class VmaxuvxTestBehavior extends VTestBehavior("vmaxu.vx.data", ctrlBundles.vmaxu_vx, "u", "vmaxu_vx") {}
class VminvxTestBehavior extends VTestBehavior("vmin.vx.data", ctrlBundles.vmin_vx, "s", "vmin_vx") {}
class VminuvxTestBehavior extends VTestBehavior("vminu.vx.data", ctrlBundles.vminu_vx, "u", "vminu_vx") {}

class VmergevxmTestBehavior extends VTestBehavior("vmerge.vxm.data", ctrlBundles.vmerge_vxm, "u", "vmerge_vxm") {}

class VadcvxmTestBehavior extends VTestBehavior("vadc.vxm.data", ctrlBundles.vadc_vxm, "u", "vadc_vxm", _vm = Option(false)) {}
class VsbcvxmTestBehavior extends VTestBehavior("vsbc.vxm.data", ctrlBundles.vsbc_vxm, "u", "vsbc_vxm", _vm = Option(false)) {}

class VssubvxTestBehavior extends VTestBehavior("vssub.vx.data", ctrlBundles.vssub_vx, "s", "vssub_vx") {}
class VssubuvxTestBehavior extends VTestBehavior("vssubu.vx.data", ctrlBundles.vssubu_vx, "u", "vssubu_vx") {}
class VsaddvxTestBehavior extends VTestBehavior("vsadd.vx.data", ctrlBundles.vsadd_vx, "s", "vsadd_vx") {}
class VsadduvxTestBehavior extends VTestBehavior("vsaddu.vx.data", ctrlBundles.vsaddu_vx, "u", "vsaddu_vx") {}

class VssrlvxTestBehavior extends VTestBehavior("vssrl.vx.data", ctrlBundles.vssrl_vx, "u", "vssrl_vx") {}
class VssravxTestBehavior extends VTestBehavior("vssra.vx.data", ctrlBundles.vssra_vx, "u", "vssra_vx") {}

class VaaddvxTestBehavior extends VTestBehavior("vaadd.vx.data", ctrlBundles.vaadd_vx, "s", "vaadd_vx") {}
class VaadduvxTestBehavior extends VTestBehavior("vaaddu.vx.data", ctrlBundles.vaaddu_vx, "u", "vaaddu_vx") {}
class VasubvxTestBehavior extends VTestBehavior("vasub.vx.data", ctrlBundles.vasub_vx, "s", "vasub_vx") {}
class VasubuvxTestBehavior extends VTestBehavior("vasubu.vx.data", ctrlBundles.vasubu_vx, "u", "vasubu_vx") {}

class VaddviTestBehavior extends VTestBehavior("vadd.vi.data", ctrlBundles.vadd_vi, "u", "vadd_vi") {}
class VandviTestBehavior extends VTestBehavior("vand.vi.data", ctrlBundles.vand_vi, "u", "vand_vi") {}
class VorviTestBehavior extends VTestBehavior("vor.vi.data", ctrlBundles.vor_vi, "u", "vor_vi") {}
class VxorviTestBehavior extends VTestBehavior("vxor.vi.data", ctrlBundles.vxor_vi, "u", "vxor_vi") {}
class VsaddviTestBehavior extends VTestBehavior("vsadd.vi.data", ctrlBundles.vsadd_vi, "s", "vsadd_vi") {}
class VsllviTestBehavior extends VTestBehavior("vsll.vi.data", ctrlBundles.vsll_vi, "u", "vsll_vi") {}
class VsrlviTestBehavior extends VTestBehavior("vsrl.vi.data", ctrlBundles.vsrl_vi, "u", "vsrl_vi") {}
class VsraviTestBehavior extends VTestBehavior("vsra.vi.data", ctrlBundles.vsra_vi, "u", "vsra_vi") {}
class VssrlviTestBehavior extends VTestBehavior("vssrl.vi.data", ctrlBundles.vssrl_vi, "u", "vssrl_vi") {}
class VssraviTestBehavior extends VTestBehavior("vssra.vi.data", ctrlBundles.vssra_vi, "u", "vssra_vi") {}
class VsadduviTestBehavior extends VTestBehavior("vsaddu.vi.data", ctrlBundles.vsaddu_vi, "u", "vsaddu_vi") {}

class VrsubvxTestBehavior extends VTestBehavior("vrsub.vx.data", ctrlBundles.vrsub_vx, "s", "vrsub_vx") {}
class VrsubviTestBehavior extends VTestBehavior("vrsub.vi.data", ctrlBundles.vrsub_vi, "s", "vrsub_vi") {}

// class VmvsxTestBehavior extends VTestBehavior("vmv.s.x.data", ctrlBundles.vmv_s_x, "u", "vmv_s_x") {}
class VmvvxTestBehavior extends VTestBehavior("vmv.v.x.data", ctrlBundles.vmv_v_x, "u", "vmv_v_x") {}
class VmvxsTestBehavior extends VTestBehavior("vmv.x.s.data", ctrlBundles.vmv_x_s, "u", "vmv_x_s") {}

class VmvviTestBehavior extends VTestBehavior("vmv.v.i.data", ctrlBundles.vmv_v_i, "u", "vmv_v_i") {}

class VfmvfsTestBehavior extends VTestBehavior("vfmv.f.s.data", ctrlBundles.vfmv_f_s, "-", "vfmv_f_s") {}
// class VfmvsfTestBehavior extends VTestBehavior("vfmv.s.f.data", ctrlBundles.vfmv_s_f, "-", "vfmv_s_f") {}

class VmergevimTestBehavior extends VTestBehavior("vmerge.vim.data", ctrlBundles.vmerge_vim, "u", "vmerge_vim") {}

class VadcvimTestBehavior extends VTestBehavior("vadc.vim.data", ctrlBundles.vadc_vim, "u", "vadc_vim", _vm = Option(false)) {}

class VTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String,
    _vm : Option[Boolean] = None) extends TestBehavior(fn, cb, s, instid) {

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        var vx = simi.get("RS1") != None || simi.get("FS1") != None
        var vv = simi.get("VS1") != None
        var vs1data : Array[String] = Array()
        if (vv)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        if (vx) {
            if (simi.get("RS1") != None)
                vs1data = UtilFuncs.multilmuldatahandle(simi.get("RS1").get)
            if (simi.get("FS1") != None) {
                vs1data = UtilFuncs.multilmuldatahandle(simi.get("FS1").get)
                vs1data(0) = s"h${vs1data(0).slice(17, 33)}"
            }
        }

        val hasvs2 = simi.get("VS2") != None
        var vs2data : Array[String] = Array()
        if (hasvs2)
            vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        
        val hasOldvd = simi.get("OLD_VD") != None
        var oldvddata : Array[String] = Array()
        if (hasOldvd)
            oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        var mask = Array("hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
        if (simi.get("MASK") != None)
            mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        
        val vflmul = simi.get("vflmul").get
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)
        val vxrm = simi.get("vxrm").get.toInt
        val vstart = getVstart(simi)
        var vm = (simi.get("vm").get.toInt == 1)
        _vm match {
            case Some(__vm) => vm = __vm
            case None => {}
        }

        val vxsat = simi.get("vxsat").get.toInt == 1

        var hasVd = simi.get("VD") != None
        var hasRd = simi.get("RD") != None
        var expectvd : Array[String] = Array()
        if (hasVd)
            expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        if (hasRd)
            expectvd = UtilFuncs.multilmuldatahandle(simi.get("RD").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false  
        for(j <- 0 until n_inputs){
            var vs2 = "h0"
            if(hasvs2) vs2 = vs2data(j)
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            var srcBundle = SrcBundle(
                    vs2=vs2,
                    mask=mask(0))
            if (hasOldvd)
                srcBundle.old_vd=oldvddata(j)
            if (vv)
                srcBundle.vs1 = vs1data(j)
            if (vx)
                srcBundle.rs1 = vs1data(0)
            
            dut.io.in.bits.poke(genVFuInput(
                srcBundle, 
                ctrl.copy(
                    vsew=vsew,
                    vs1_imm=getImm(simi),
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = vm,
                    uopIdx=n_inputs - 1 -j,
                    vxrm = vxrm,
                    frm=getfrm(simi),
                    vstart = vstart
                )
            ))
            dut.clock.step(1)
            // println(s"??? ${finalVxsat}")
            finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            
            if (hasVd) {
                vd = dut.io.out.bits.vd.peek().litValue
                vdres = f"h$vd%032x".equals(expectvd(j))
                Logger.printvds(f"h$vd%032x", expectvd(j))
                if (!vdres) dump(simi, f"h$vd%032x", expectvd(j))
                assert(vdres)
            }
        }
        assert(finalVxsat == vxsat)

        if (hasRd) {
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%016x".equals(expectvd(0))
            Logger.printvds(f"h$vd%016x", expectvd(0))
            if (!vdres) dump(simi, f"h$vd%016x", expectvd(0))
            assert(vdres)
        }

        /*val aluOutput = genVAluOutput(f"h${0}%032x", finalVxsat)
        println(s"aluOutput == Bundle ${dut.io.out.bits.expect(aluOutput)}")*/
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}