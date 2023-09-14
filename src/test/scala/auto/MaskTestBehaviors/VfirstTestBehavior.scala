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

class VfirstmTestBehavior extends VfirTestBehavior("vfirst.m.data", ctrlBundles.vfirst_m, "u", "vfirst_m", 0x11) {}

class VfirTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int = 0) extends TestBehavior(fn, cb, s, instid) {
    
    override def getDut() : Module               = {
        val dut = new VMask
        // TestHarnessPerm.test_init(dut)
        return dut
    }

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
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
            
        // println("1111")
        // dut.io.out.ready.poke(true.B)
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.poke(genVFuInput(
            SrcBundle(
                vs2=vs2data(n_inputs - 1),
                mask=mask(0)
            ), 
            ctrl.copy(
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
        ))
        dut.clock.step(1)
        // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
        vd = dut.io.out.bits.vd.peek().litValue
        vdres = f"h$vd%016x".equals(expectrd(0))
        Logger.printvds(f"h$vd%016x", expectrd(0))
        if (!vdres) dump(simi, f"h$vd%016x", expectrd(0))
        assert(vdres)
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}
