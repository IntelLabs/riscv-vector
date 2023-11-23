package darecreek.vfuAutotest.alu

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

class VnclipwvTestBehavior extends VnTestBehavior("vnclip.wv.data", ctrlBundles.vnclip_wv, "s", "vnclip_wv", true) {}
class VnclipuwvTestBehavior extends VnTestBehavior("vnclipu.wv.data", ctrlBundles.vnclipu_wv, "u", "vnclipu_wv", true) {}

class VnsrlwvTestBehavior extends VnTestBehavior("vnsrl.wv.data", ctrlBundles.vnsrl_wv, "u", "vnsrl_wv", false) {}
class VnsrawvTestBehavior extends VnTestBehavior("vnsra.wv.data", ctrlBundles.vnsra_wv, "u", "vnsra_wv", false) {}

class VnclipwxTestBehavior extends VnTestBehavior("vnclip.wx.data", ctrlBundles.vnclip_wx, "s", "vnclip_wx", true) {}
class VnclipuwxTestBehavior extends VnTestBehavior("vnclipu.wx.data", ctrlBundles.vnclipu_wx, "u", "vnclipu_wx", true) {}

class VnsrlwxTestBehavior extends VnTestBehavior("vnsrl.wx.data", ctrlBundles.vnsrl_wx, "u", "vnsrl_wx", false) {}
class VnsrawxTestBehavior extends VnTestBehavior("vnsra.wx.data", ctrlBundles.vnsra_wx, "u", "vnsra_wx", false) {}

class VnclipwiTestBehavior extends VnTestBehavior("vnclip.wi.data", ctrlBundles.vnclip_wi, "s", "vnclip_wi", true) {}
class VnclipuwiTestBehavior extends VnTestBehavior("vnclipu.wi.data", ctrlBundles.vnclipu_wi, "u", "vnclipu_wi", true) {}

class VnsrlwiTestBehavior extends VnTestBehavior("vnsrl.wi.data", ctrlBundles.vnsrl_wi, "u", "vnsrl_wi", false) {}
class VnsrawiTestBehavior extends VnTestBehavior("vnsra.wi.data", ctrlBundles.vnsra_wi, "u", "vnsra_wi", false) {}

class VnTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, useVxsat : Boolean) extends TestBehavior(fn, cb, s, instid) {
    
    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        var vx = simi.get("RS1") != None
        var vv = simi.get("VS1") != None
        var vs1data : Array[String] = Array()
        if (vx)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("RS1").get)
        if (vv)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        val vxrm = simi.get("vxrm").get.toInt
        // println("lmel > 1, id", i)

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        var n_ops = n_inputs * 2
        if(vflmul == "0.250000" || 
            vflmul == "0.500000" || 
            vflmul == "0.125000") n_ops = n_inputs

        var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false

        var prevVd = ""
            
        // println("1111")
        for(j <- 0 until n_ops){
            val sewIndex = n_inputs - 1 - (j / 2)
            val sew2Index = n_ops - 1 - j
            var oldvd = oldvddata(sewIndex)
            /* if(j % 2 == 0) 
            oldvd = oldvddata(sewIndex) */
            
            // println(s"vs2data(sew2Index) ${vs2data(sew2Index)}, sew2Index ${sew2Index}")
            var srcBundle = SrcBundle(
                    vs2=vs2data(sew2Index), 
                    old_vd=oldvd,
                    mask=mask(0))
            if (vx) srcBundle.rs1=vs1data(0)
            if (vv) srcBundle.vs1=vs1data(sewIndex)

            val ctrlBundle = ctrl.copy(
                    vsew=vsew,
                    vs1_imm=getImm(simi),
                    narrow=true,
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = (simi.get("vm").get.toInt == 1),
                    uopIdx=j,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
                )

            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.poke(genVFuInput(
                srcBundle, 
                ctrlBundle
            ))
            println(s"uopIdx ${j}, ctrlBundle: \n .. ${ctrlBundle}")
            dut.clock.step(1)
            finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(sewIndex))
            if (j % 2 == 1 || (n_ops == 1)) {
                // compare when it's odd uopidx or it has only one uop
                Logger.printvds(f"h$vd%032x", expectvd(sewIndex))
                if (!vdres) dump(simi, f"h$vd%032x", expectvd(sewIndex))
                assert(vdres)
            }

            /*if (j % 2 == 0)
                prevVd = f"h$vd%032x"*/

        }
        if(useVxsat) {
            if (finalVxsat != vxsat) dump(simi, s"(vxsat) ${finalVxsat}", s"(vxsat) ${vxsat}")
            assert(finalVxsat == vxsat)
        }
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}