package darecreek.vfuAutotest.fullPipeline

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.vmask._

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import chipsalliance.rocketchip.config.Parameters

class VfirstmTestBehavior extends VfirTestBehavior("vfirst.m.data", ctrlBundles.vfirst_m, "u", "vfirst_m", 0x11) {}

class VfirTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int = 0) extends TestBehavior(fn, cb, s, instid) {
    
    override def isOrdered() : Boolean = false
    override def getTargetTestEngine() = TestEngine.MASK_TEST_ENGINE

    override def _getNextTestCase(simi:Map[String,String]) : TestCase = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxrm = simi.get("vxrm").get.toInt

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectrd = UtilFuncs.multilmuldatahandle(simi.get("RD").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        // var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false

        val resultChecker = new MaskVcpopVidResultChecker(1, expectrd, true, 
            (a, b) => this.dump(simi, a, b))

        val srcBundle = SrcBundle(
            vs2=vs2data(n_inputs - 1),
            mask=mask(0)
        )

        var ctrlBundle = ctrl.copy(
            vsew=vsew,
            vs1_imm=vs1_imm,
            vl=simi.get("vl").get.toInt,
            vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
            ma = (simi.get("ma").get.toInt == 1),
            ta = (simi.get("ta").get.toInt == 1),
            vm = (simi.get("vm").get.toInt == 1),
            uopIdx=0,
            vxrm = vxrm,
            vstart = getVstart(simi)
        )

        var srcBundles : Seq[SrcBundle] = Seq(srcBundle)
        var ctrlBundles : Seq[CtrlBundle] = Seq(ctrlBundle)
        
        return TestCase.newNormalCase(
            this.instid,
            srcBundles,
            ctrlBundles,
            resultChecker
        )
    }
}
