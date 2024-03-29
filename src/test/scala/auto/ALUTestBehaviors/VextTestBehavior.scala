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

class VzextVf2TestBehavior extends VzvsTestBehavior("vzext.vf2.data", ctrlBundles.vzext_vf2, "u", "vzext_vf2", 6, 2) {}
class VsextVf2TestBehavior extends VzvsTestBehavior("vsext.vf2.data", ctrlBundles.vsext_vf2, "s", "vsext_vf2", 7, 2) {}
class VzextVf4TestBehavior extends VzvsTestBehavior("vzext.vf4.data", ctrlBundles.vzext_vf4, "u", "vzext_vf4", 4, 4) {}
class VsextVf4TestBehavior extends VzvsTestBehavior("vsext.vf4.data", ctrlBundles.vsext_vf4, "s", "vsext_vf4", 5, 4) {}
class VzextVf8TestBehavior extends VzvsTestBehavior("vzext.vf8.data", ctrlBundles.vzext_vf8, "u", "vzext_vf8", 2, 8) {}
class VsextVf8TestBehavior extends VzvsTestBehavior("vsext.vf8.data", ctrlBundles.vsext_vf8, "s", "vsext_vf8", 3, 8) {}


class VzvsTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int, multiple : Int) extends TestBehavior(fn, cb, s, instid) {
    
    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
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
        for(j <- 0 until n_ops){
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.poke(genVFuInput(
                // vs2 has been changed
                SrcBundle(
                    vs2=vs2data(n_ops - 1 - ((n_ops - 1 - j) / multiple)),
                    old_vd=oldvddata(j),
                    mask=mask(0)), 
                ctrl.copy(
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
            ))
            dut.clock.step(1)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(j))
            Logger.printvds(f"h$vd%032x", expectvd(j))
            assert(vdres)
        }
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}
