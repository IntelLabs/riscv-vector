package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters

class VzextVf2TestBehavior extends VzvsTestBehavior("vzext.vf2.data", ctrlBundles.vzext_vf2, "u", "vzext_vf2", 6, 2) {}
class VsextVf2TestBehavior extends VzvsTestBehavior("vsext.vf2.data", ctrlBundles.vsext_vf2, "s", "vsext_vf2", 7, 2) {}
class VzextVf4TestBehavior extends VzvsTestBehavior("vzext.vf4.data", ctrlBundles.vzext_vf4, "u", "vzext_vf4", 4, 4) {}
class VsextVf4TestBehavior extends VzvsTestBehavior("vsext.vf4.data", ctrlBundles.vsext_vf4, "s", "vsext_vf4", 5, 4) {}
class VzextVf8TestBehavior extends VzvsTestBehavior("vzext.vf8.data", ctrlBundles.vzext_vf8, "u", "vzext_vf8", 2, 8) {}
class VsextVf8TestBehavior extends VzvsTestBehavior("vsext.vf8.data", ctrlBundles.vsext_vf8, "s", "vsext_vf8", 3, 8) {}


class VzvsTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int, multiple : Int) extends TestBehavior(fn, cb, s, instid) {
    
    override def _getNextTestCase(simi:Map[String,String]) : TestCase = {
        val ctrl = this.getCtrlBundle()
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxrm = simi.get("vxrm").get.toInt

        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        // var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false
            
        // println("1111")
        var n_ops = n_inputs

        val nRes = n_ops
        val resultChecker = ALUResultChecker.newVChecker(nRes, expectvd, true, vxsat, 
            (a, b) => this.dump(simi, a, b))
        var srcBundles : Seq[SrcBundle] = Seq()
        var ctrlBundles : Seq[CtrlBundle] = Seq()
        
        for(j <- 0 until n_ops){
            
                // vs2 has been changed
            val srcBundle = SrcBundle(
                    vs2=vs2data(n_ops - 1 - ((n_ops - 1 - j) / multiple)),
                    old_vd=oldvddata(j),
                    mask=mask(0))
            val ctrlBundle = ctrl.copy(
                    vsew=vsew,
                    vs1_imm=vs1_imm,
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = (simi.get("vm").get.toInt == 1),
                    uopIdx=n_ops - 1 -j,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
            )
            
            srcBundles :+= srcBundle
            ctrlBundles :+= ctrlBundle
        }
        // assert(finalVxsat == vxsat)
        return TestCase.newNormalCase(
            this.instid,
            srcBundles,
            ctrlBundles,
            resultChecker
        )
    }
}
