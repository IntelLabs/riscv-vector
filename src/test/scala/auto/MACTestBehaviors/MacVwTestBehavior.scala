package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu._
import darecreek.exu.vfu.mac._
import darecreek.exu.vfu.VInstructions._
import org.chipsalliance.cde.config.Parameters

class VwmulvvTestBehavior extends VwmuTestBehavior("vwmul.vv.data", ctrlBundles.vwmul_vv, "s", "vwmul_vv", false) {}
class VwmuluvvTestBehavior extends VwmuTestBehavior("vwmulu.vv.data", ctrlBundles.vwmulu_vv, "u", "vwmulu_vv", false) {}
class VwmulsuvvTestBehavior extends VwmuTestBehavior("vwmulsu.vv.data", ctrlBundles.vwmulsu_vv, "u", "vwmulsu_vv", true) {}
class VwmaccvvTestBehavior extends VwmuTestBehavior("vwmacc.vv.data", ctrlBundles.vwmacc_vv, "s", "vwmacc_vv", false) {}
class VwmaccuvvTestBehavior extends VwmuTestBehavior("vwmaccu.vv.data", ctrlBundles.vwmaccu_vv, "u", "vwmaccu_vv", false) {}
class VwmaccsuvvTestBehavior extends VwmuTestBehavior("vwmaccsu.vv.data", ctrlBundles.vwmaccsu_vv, "s", "vwmaccsu_vv", false) {}

class VwmulvxTestBehavior extends VwmuTestBehavior("vwmul.vx.data", ctrlBundles.vwmul_vx, "s", "vwmul_vx", false) {}
class VwmuluvxTestBehavior extends VwmuTestBehavior("vwmulu.vx.data", ctrlBundles.vwmulu_vx, "u", "vwmulu_vx", false) {}
class VwmulsuvxTestBehavior extends VwmuTestBehavior("vwmulsu.vx.data", ctrlBundles.vwmulsu_vx, "u", "vwmulsu_vx", true) {}
class VwmaccvxTestBehavior extends VwmuTestBehavior("vwmacc.vx.data", ctrlBundles.vwmacc_vx, "s", "vwmacc_vx", false) {}
class VwmaccuvxTestBehavior extends VwmuTestBehavior("vwmaccu.vx.data", ctrlBundles.vwmaccu_vx, "u", "vwmaccu_vx", false) {}
class VwmaccsuvxTestBehavior extends VwmuTestBehavior("vwmaccsu.vx.data", ctrlBundles.vwmaccsu_vx, "s", "vwmaccsu_vx", false) {}

class VwmaccusvxTestBehavior extends VwmuTestBehavior("vwmaccus.vx.data", ctrlBundles.vwmaccus_vx, "s", "vwmaccus_vx", false) {}

class VwmuTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, su : Boolean) extends TestBehavior(fn, cb, s, instid) {

    override def getDut() : Module               = {
        val dut = new VMacWrapper
        return dut
    }

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) : Unit = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        
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
        for(j <- 0 until n_ops){
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            var srcBundle = SrcBundle(
                    vs2=vs2data(j / 2), 
                    // vs1=vs1data(j / 2),
                    old_vd=oldvddata(j),
                    mask=mask(0))
            if (vv)
                srcBundle.vs1=vs1data(j / 2)
            if (vx)
                srcBundle.rs1=vs1data(0)
            dut.io.in.bits.poke(genVFuInput(
                srcBundle, 
                ctrl.copy(
                    vsew=vsew, 
                    widen=true,
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = (simi.get("vm").get.toInt == 1),
                    uopIdx=n_ops - 1 -j,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
                )
            ))
            dut.clock.step(2)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(j))
            Logger.printvds(f"h$vd%032x", expectvd(j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(j))
            assert(vdres)
        }
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}

