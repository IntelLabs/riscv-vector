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
import org.chipsalliance.cde.config.Parameters

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
    
    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
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
        for(j <- 0 until n_ops){
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)

            var vs2idx = j
            if (vwvv) vs2idx = j / 2
            var srcBundle = SrcBundle(
                    vs2=vs2data(vs2idx), 
                    old_vd=oldvddata(j),
                    mask=mask(0))
            if (vx) srcBundle.rs1=vs1data(0)
            if (vv) srcBundle.vs1=vs1data(j / 2)
                    

            dut.io.in.bits.poke(genVFuInput(
                srcBundle, 
                ctrl.copy(
                    vsew=vsew,
                    widen2=(!vwvv),
                    widen=vwvv,

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
            dut.clock.step(1)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(j))
            Logger.printvds(f"h$vd%032x", expectvd(j))
            if (!vdres) { dump(simi, f"h$vd%032x", expectvd(j)) }
            assert(vdres)
        }
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}


/*class VwvvTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, su : Boolean = false) extends TestBehavior(fn, cb, s, instid) {
    
    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        // val vvtype = UtilFuncs.srctypeconvert(simi.get("vsew").get,s)
        // val vdtype = UtilFuncs.srctypeconvert(simi.get("vsew").get,"u") + 1
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)
        val vxrm = simi.get("vxrm").get.toInt
        // println("lmel > 1, id", i)

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
            dut.io.in.bits.poke(genVFuInput(
                SrcBundle(
                    vs2=vs2data(j / 2), 
                    vs1=vs1data(j / 2),
                    old_vd=oldvddata(j),
                    mask=mask(0)), 
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
            dut.clock.step(1)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(j))
            Logger.printvds(f"h$vd%032x", expectvd(j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(j))
            assert(vdres)
        }
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}*/


