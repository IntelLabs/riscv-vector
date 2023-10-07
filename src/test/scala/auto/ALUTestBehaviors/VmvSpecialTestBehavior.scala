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


class VmvsxTestBehavior extends VmvSpecialTestBehavior("vmv.s.x.data", ctrlBundles.vmv_s_x, "u", "vmv_s_x") {}
class VfmvsfTestBehavior extends VmvSpecialTestBehavior("vfmv.s.f.data", ctrlBundles.vfmv_s_f, "-", "vfmv_s_f") {}

class VmvSpecialTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String) extends TestBehavior(fn, cb, s, instid) {

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
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

        val hasvs2 = simi.get("VS2") != None
        var vs2data : Array[String] = Array()
        if (hasvs2)
            vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        
        val hasOldvd = simi.get("OLD_VD") != None
        var oldvddata : Array[String] = Array()
        if (hasOldvd)
            oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        var mask = Array("hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
        if (simi.get("MASK") != None)
            mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        
        val vflmul = simi.get("vflmul").get
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)
        val vxrm = simi.get("vxrm").get.toInt
        val vstart = getVstart(simi)
        var vm = (simi.get("vm").get.toInt == 1)

        val vxsat = simi.get("vxsat").get.toInt == 1

        var hasVd = simi.get("VD") != None
        var hasRd = simi.get("RD") != None
        var expectvd : Array[String] = Array()
        if (hasVd)
            expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        if (hasRd)
            expectvd = UtilFuncs.multilmuldatahandle(simi.get("RD").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        var finalVxsat = false
        var vd : BigInt = 0
        var vdres = false  
        var j = 0
        var vs2 = "h0"
        if(hasvs2) vs2 = vs2data(n_inputs - 1 -j)
        dut.io.out.ready.poke(true.B)
        dut.io.in.valid.poke(true.B)
        var srcBundle = SrcBundle(
                vs2=vs2,
                mask=mask(0))
        if (hasOldvd)
            srcBundle.old_vd=oldvddata(n_inputs - 1 -j)
        if (vv)
            srcBundle.vs1 = vs1data(n_inputs - 1 -j)
        if (vx)
            srcBundle.rs1 = vs1data(0)
        
        dut.io.in.bits.poke(genVFuInput(
            srcBundle, 
            ctrl.copy(
                vsew=vsew,
                vs1_imm=getImm(simi),
                vl=simi.get("vl").get.toInt,
                vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                ma = (simi.get("ma").get.toInt == 1),
                ta = (simi.get("ta").get.toInt == 1),
                vm = vm,
                uopIdx=j,
                vxrm = vxrm,
                frm=getfrm(simi),
                vstart = vstart
            )
        ))
        dut.clock.step(1)
        // println(s"??? ${finalVxsat}")
        finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
        
        if (hasVd) {
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1 -j))
            Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1 -j))
            if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1 -j))
            assert(vdres)
        }
        assert(finalVxsat == vxsat)

        if (hasRd) {
            vd = dut.io.out.bits.vd.peek().litValue
            vdres = f"h$vd%016x".equals(expectvd(0))
            Logger.printvds(f"h$vd%016x", expectvd(0))
            if (!vdres) dump(simi, f"h$vd%016x", expectvd(0))
            assert(vdres)
        }

        /*val aluOutput = genVAluOutput(f"h${0}%032x", finalVxsat)
        println(s"aluOutput == Bundle ${dut.io.out.bits.expect(aluOutput)}")*/
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}