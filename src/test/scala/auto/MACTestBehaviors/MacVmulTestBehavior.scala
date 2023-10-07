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

class VmulvvTestBehavior extends VmuTestBehavior("vmul.vv.data", ctrlBundles.vmul_vv, "s", "vmul_vv") {}
class VmulhvvTestBehavior extends VmuTestBehavior("vmulh.vv.data", ctrlBundles.vmulh_vv, "s", "vmulh_vv") {}
class VmulhuvvTestBehavior extends VmuTestBehavior("vmulhu.vv.data", ctrlBundles.vmulhu_vv, "u", "vmulhu_vv") {}
class VmulhsuvvTestBehavior extends VmuTestBehavior("vmulhsu.vv.data", ctrlBundles.vmulhsu_vv, "u", "vmulhsu_vv") {}

class VmulvxTestBehavior extends VmuTestBehavior("vmul.vx.data", ctrlBundles.vmul_vx, "s", "vmul_vx") {}
class VmulhvxTestBehavior extends VmuTestBehavior("vmulh.vx.data", ctrlBundles.vmulh_vx, "s", "vmulh_vx") {}
class VmulhuvxTestBehavior extends VmuTestBehavior("vmulhu.vx.data", ctrlBundles.vmulhu_vx, "u", "vmulhu_vx") {}
class VmulhsuvxTestBehavior extends VmuTestBehavior("vmulhsu.vx.data", ctrlBundles.vmulhsu_vx, "u", "vmulhsu_vx") {}

class VmuTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String) extends TestBehavior(fn, cb, s, instid) {
    
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

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        // var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false
            
        // println("1111")
        for(j <- 0 until n_inputs){
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)

            var srcBundle = SrcBundle(
                    vs2=vs2data(n_inputs - 1 - j), 
                    // vs1=vs1data(n_inputs - 1 - j),
                    old_vd=oldvddata(n_inputs - 1 - j),
                    mask=mask(0))
            
            if (vv)
                srcBundle.vs1=vs1data(n_inputs - 1 - j)
            if (vx)
                srcBundle.rs1=vs1data(0)

            dut.io.in.bits.poke(genVFuInput(
                srcBundle, 
                ctrl.copy(
                    vsew=vsew,
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = (simi.get("vm").get.toInt == 1),
                    uopIdx=j,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
                )
            ))
            dut.clock.step(2)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1 - j))
            Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1 - j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1 - j))
            assert(vdres)
        }
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}