package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu.vmask._
import scala.collection.mutable.Map
import chipsalliance.rocketchip.config.Parameters

class VcpopmTestBehavior extends VcpTestBehavior("vcpop.m.data", ctrlBundles.vcpop_m, "-", "vcpop_m", 0x10) {}
class VidvTestBehavior extends VcpTestBehavior("vid.v.data", ctrlBundles.vid_v, "u", "vid_v", 0x11) {}
class ViotamTestBehavior extends VcpTestBehavior("viota.m.data", ctrlBundles.viota_m, "u", "viota_m", 0x10) {}

class VcpTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int = 0) extends TestBehavior(fn, cb, s, instid) {

    override def isOrdered() : Boolean = true
    override def getTargetTestEngine() = TestEngine.MASK_TEST_ENGINE

    override def _getNextTestCase(simi:Map[String,String]) : TestCase = {
        
        val hasVs2 = simi.get("VS2") != None
        val hasOldvd = simi.get("OLD_VD") != None
        var vs2data = Array("h0")
        var oldvddata = Array("h0")

        if (hasVs2)
            vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        if (hasOldvd)
            oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)

        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxrm = simi.get("vxrm").get.toInt
        
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        val vxsat = simi.get("vxsat").get.toInt == 1

        var hasRd = simi.get("RD") != None
        var expectrd = Array("h0")
        if (hasRd)
            expectrd = UtilFuncs.multilmuldatahandle(simi.get("RD").get)
        else
            expectrd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        var n_ops = n_inputs

        var vd : BigInt = 0
        var vdres = false

        var vs2 = "h0"
        var oldvd = "h0"

        val resultChecker = new MaskVcpopVidResultChecker(n_ops, expectrd, hasRd, 
            (a, b) => this.dump(simi, a, b))
        var srcBundles : Seq[SrcBundle] = Seq()
        var ctrlBundles : Seq[CtrlBundle] = Seq()

        for(j <- 0 until n_ops){
            if (hasVs2)
                vs2 = vs2data(n_inputs - 1)
            else
                vs2 = "h0"
            
            if (hasOldvd)
                oldvd = oldvddata(n_inputs - 1 - j)
            else
                oldvd = "h0"

            val srcBundle = SrcBundle(
                old_vd=oldvd,
                vs2=vs2, 
                vs1="h0",
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
                uopIdx=j,
                uopEnd = (j == n_ops - 1),
                vxrm = vxrm,
                vstart = getVstart(simi)
            )

            srcBundles :+= srcBundle
            ctrlBundles :+= ctrlBundle
        }

        return TestCase.newNormalCase(
            this.instid,
            srcBundles,
            ctrlBundles,
            resultChecker
        )
    }
}