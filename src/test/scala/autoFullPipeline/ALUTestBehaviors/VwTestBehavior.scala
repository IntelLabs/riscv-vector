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

class VwaddwvTestBehavior extends VwTestBehavior("vwadd.wv.data", ctrlBundles.vwadd_wv, "s", "vwadd_wv") {}
class VwadduwvTestBehavior extends VwTestBehavior("vwaddu.wv.data", ctrlBundles.vwaddu_wv, "u", "vwaddu_wv") {}
class VwsubwvTestBehavior extends VwTestBehavior("vwsub.wv.data", ctrlBundles.vwsub_wv, "s", "vwsub_wv") {}
class VwsubuwvTestBehavior extends VwTestBehavior("vwsubu.wv.data", ctrlBundles.vwsubu_wv, "u", "vwsubu_wv") {}

class VwaddvvTestBehavior extends VwTestBehavior("vwadd.vv.data", ctrlBundles.vwadd_vv, "s", "vwadd_vv", true) {}
class VwadduvvTestBehavior extends VwTestBehavior("vwaddu.vv.data", ctrlBundles.vwaddu_vv, "u", "vwaddu_vv", true) {}
class VwsubvvTestBehavior extends VwTestBehavior("vwsub.vv.data", ctrlBundles.vwsub_vv, "s", "vwsub_vv", true) {}
class VwsubuvvTestBehavior extends VwTestBehavior("vwsubu.vv.data", ctrlBundles.vwsubu_vv, "u", "vwsubu_vv", true) {}

class VwaddwxTestBehavior extends VwTestBehavior("vwadd.wx.data", ctrlBundles.vwadd_wx, "s", "vwadd_wx") {}
class VwadduwxTestBehavior extends VwTestBehavior("vwaddu.wx.data", ctrlBundles.vwaddu_wx, "u", "vwaddu_wx") {}
class VwsubwxTestBehavior extends VwTestBehavior("vwsub.wx.data", ctrlBundles.vwsub_wx, "s", "vwsub_wx") {}
class VwsubuwxTestBehavior extends VwTestBehavior("vwsubu.wx.data", ctrlBundles.vwsubu_wx, "u", "vwsubu_wx") {}

class VwaddvxTestBehavior extends VwTestBehavior("vwadd.vx.data", ctrlBundles.vwadd_vx, "s", "vwadd_vx", true) {}
class VwadduvxTestBehavior extends VwTestBehavior("vwaddu.vx.data", ctrlBundles.vwaddu_vx, "u", "vwaddu_vx", true) {}
class VwsubvxTestBehavior extends VwTestBehavior("vwsub.vx.data", ctrlBundles.vwsub_vx, "s", "vwsub_vx", true) {}
class VwsubuvxTestBehavior extends VwTestBehavior("vwsubu.vx.data", ctrlBundles.vwsubu_vx, "u", "vwsubu_vx", true) {}

class VwTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vwvv : Boolean = false) extends TestBehavior(fn, cb, s, instid) {
    
    override def _getNextTestCase(simi:Map[String,String]) : TestCase = {
        var vx = simi.get("RS1") != None
        var vv = simi.get("VS1") != None
        var vs1data : Array[String] = Array()
        if (vx)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("RS1").get)
        if (vv)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        val vxrm = simi.get("vxrm").get.toInt
        // println("lmel > 1, id", i)

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        var n_inputs = 0.5
        if(vflmul == "1.000000") n_inputs = 1.0
        if(vflmul == "2.000000") n_inputs = 2.0
        if(vflmul == "4.000000") n_inputs = 4.0
        if(vflmul == "8.000000") n_inputs = 8.0
        
        // var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false
            
        // println("1111")
        var n_ops = (n_inputs * 2.0).toInt

        var srcBundles : Seq[SrcBundle] = Seq()
        var ctrlBundles : Seq[CtrlBundle] = Seq()
        var expectvd1 : Array[String] = Array()

        for(j <- 0 until n_ops){
            
            var vs2idx = j
            if (vwvv) vs2idx = j / 2
            var srcBundle = SrcBundle(
                    vs2=vs2data(vs2idx), 
                    old_vd=oldvddata(j),
                    mask=mask(0))
            if (vx) srcBundle.rs1=vs1data(0)
            if (vv) srcBundle.vs1=vs1data(j / 2)
                    

            
            
            val ctrlBundle = ctrl.copy(
                vsew=vsew,
                widen2=(!vwvv),
                widen=vwvv,

                vl=simi.get("vl").get.toInt,
                vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                ma = (simi.get("ma").get.toInt == 1),
                ta = (simi.get("ta").get.toInt == 1),
                vm = (simi.get("vm").get.toInt == 1),
                uopIdx=n_ops - 1 - j,
                vxrm = vxrm,
                vstart = getVstart(simi)
            )
            /*dut.clock.step(1)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(j))
            Logger.printvds(f"h$vd%032x", expectvd(j))
            if (!vdres) { dump(simi, f"h$vd%032x", expectvd(j)) }
            assert(vdres)*/

            srcBundles :+= srcBundle
            ctrlBundles :+= ctrlBundle
            expectvd1 = expectvd1 :+ expectvd(j)
        }
        // assert(finalVxsat == vxsat)

        val resultChecker = ALUResultChecker.newGeneralVChecker(n_ops, expectvd1, 
            (a, b) => this.dump(simi, a, b))

        return TestCase.newNormalCase(
            this.instid,
            srcBundles,
            ctrlBundles,
            resultChecker
        )
    }
}