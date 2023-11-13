package darecreek.vfuAutotest.alu
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
import chipsalliance.rocketchip.config.Parameters

class VmsbfmTestBehavior extends VmTestBehavior("vmsbf.m.data", ctrlBundles.vmsbf_m, "u", "vmsbf_m", false, 0x01) {}
class VmsifmTestBehavior extends VmTestBehavior("vmsif.m.data", ctrlBundles.vmsif_m, "u", "vmsif_m", false, 0x03) {}
class VmsofmTestBehavior extends VmTestBehavior("vmsof.m.data", ctrlBundles.vmsof_m, "u", "vmsof_m", false, 0x02) {}

class VmandmmTestBehavior extends VmTestBehavior("vmand.mm.data", ctrlBundles.vmand_mm, "u", "vmand_mm", true) {}
class VmnandmmTestBehavior extends VmTestBehavior("vmnand.mm.data", ctrlBundles.vmnand_mm, "u", "vmnand_mm", true) {}
class VmandnmmTestBehavior extends VmTestBehavior("vmandn.mm.data", ctrlBundles.vmandn_mm, "u", "vmandn_mm", true) {}
class VmxormmTestBehavior extends VmTestBehavior("vmxor.mm.data", ctrlBundles.vmxor_mm, "u", "vmxor_mm", true) {}
class VmormmTestBehavior extends VmTestBehavior("vmor.mm.data", ctrlBundles.vmor_mm, "u", "vmor_mm", true) {}
class VmnormmTestBehavior extends VmTestBehavior("vmnor.mm.data", ctrlBundles.vmnor_mm, "u", "vmnor_mm", true) {}
class VmornmmTestBehavior extends VmTestBehavior("vmorn.mm.data", ctrlBundles.vmorn_mm, "u", "vmorn_mm", true) {}
class VmxnormmTestBehavior extends VmTestBehavior("vmxnor.mm.data", ctrlBundles.vmxnor_mm, "u", "vmxnor_mm", true) {}

class VmTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, hasVs1 : Boolean, vs1_imm : Int = 0) extends TestBehavior(fn, cb, s, instid) {
    
    override def getDut() : Module               = {
        val dut = new VMask
        // TestHarnessPerm.test_init(dut)
        return dut
    }

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
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

        var robIdxValid = false
            
        // println("1111")
        // dut.io.out.ready.poke(true.B)
        dut.io.in.valid.poke(true.B)

        var srcBundle = SrcBundle()

        if (hasVs1) {
            srcBundle = SrcBundle(
                vs1=UtilFuncs.multilmuldatahandle(simi.get("VS1").get)(n_inputs - 1),
                vs2=vs2data(n_inputs - 1),
                old_vd=oldvddata(n_inputs - 1),
            )
        } else {
            srcBundle = SrcBundle(
                vs2=vs2data(n_inputs - 1),
                old_vd=oldvddata(n_inputs - 1),
                mask=UtilFuncs.multilmuldatahandle(simi.get("MASK").get)(0)
            )
        }

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
        var robIdx = (false, 0)
        robIdxValid = randomFlush()
        if (robIdxValid) {
            robIdx = (true, 1)
        }
        ctrlBundle.robIdx = robIdx

        dut.io.in.bits.poke(genVFuInput(
            srcBundle, 
            ctrlBundle
        ))
        // dut.io.redirect.poke(genFSMRedirect((robIdxValid, robIdxValid, 0)))
        dut.clock.step(1)

        if (robIdxValid) {
            // flushed
            println("flushed")

            dut.io.in.valid.poke(false.B)

            var srcBundle = SrcBundle()
            ctrlBundle = ctrl.copy()

            // turning off redirect bits
            dut.io.in.bits.poke(genVFuInput(
                srcBundle,
                ctrlBundle
            ))
            // dut.io.redirect.poke(genFSMRedirect())
            
            dut.clock.step(1)
            return
        }

        // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
        vd = dut.io.out.bits.vd.peek().litValue
        vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1))
        Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1))
        assert(vdres)
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}
