package darecreek.vfuAutotest.alu
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import org.chipsalliance.cde.config.Parameters

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._

class Vmv1rvTestBehavior extends VmvnrTestBehavior("vmv1r.v.data", ctrlBundles.vmv1r_v, "-", "vmv1r_v", 0) {}
class Vmv2rvTestBehavior extends VmvnrTestBehavior("vmv2r.v.data", ctrlBundles.vmv2r_v, "-", "vmv2r_v", 1) {}
class Vmv4rvTestBehavior extends VmvnrTestBehavior("vmv4r.v.data", ctrlBundles.vmv4r_v, "-", "vmv4r_v", 2) {}
class Vmv8rvTestBehavior extends VmvnrTestBehavior("vmv8r.v.data", ctrlBundles.vmv8r_v, "-", "vmv8r_v", 3) {}



class VmvnrTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, emul : Int) extends TestBehavior(fn, cb, s, instid) {

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        val vflmul = simi.get("vflmul").get
        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        val vxrm = simi.get("vxrm").get.toInt

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        var n_inputs = 1
        /*if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8*/
        emul match {
            case 0 => {}
            case 1 => n_inputs = 2
            case 2 => n_inputs = 4
            case 3 => n_inputs = 8
        }

        var n_ops = n_inputs

        var vd : BigInt = 0
        var vdres = false
        
        // println("1111")
        var v_index = 0
        for(j <- 0 until n_ops){
            v_index = n_inputs - 1 - j
            // if (j == 1) vs2_index = n_inputs - 1

            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.poke(genVFuInput(
                SrcBundle(vs2=vs2data(v_index), old_vd=oldvddata(v_index)), 
                ctrl.copy(
                    vsew=vsew,
                    vs1_imm=n_inputs - 1,
                    vl= ((1 << emul) * 128) / simi.get("vsew").get.toInt, // simi.get("vl").get.toInt,
                    vlmul = emul, // UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = (simi.get("vm").get.toInt == 1),
                    uopIdx=j,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
                )
            ))
            dut.clock.step(1)
            vd = dut.io.out.bits.vd.peek().litValue

            vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1 - j))
            // println(f"h$vd%032x", expectvd(n_inputs - 1 - j))
            Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1 - j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1 - j))
            assert(vdres)
        }
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}