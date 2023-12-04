package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu.vmask._
import chipsalliance.rocketchip.config.Parameters

class VcpopmTestBehavior extends VcpTestBehavior("vcpop.m.data", ctrlBundles.vcpop_m, "-", "vcpop_m", 0x10) {}
class VidvTestBehavior extends VcpTestBehavior("vid.v.data", ctrlBundles.vid_v, "u", "vid_v", 0x11) {}
class ViotamTestBehavior extends VcpTestBehavior("viota.m.data", ctrlBundles.viota_m, "u", "viota_m", 0x10) {}

class VcpTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, vs1_imm : Int = 0) extends TestBehavior(fn, cb, s, instid) {
    
    override def getDut() : Module               = {
        val dut = new VMask
        // TestHarnessPerm.test_init(dut)
        return dut
    }

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
        
        val hasVs2 = simi.get("VS2") != None
        val hasOldvd = simi.get("OLD_VD") != None
        var vs2data = Array("h0")
        var oldvddata = Array("h0")

        if (hasVs2)
            vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        if (hasOldvd)
            oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)

        val mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        val vflmul = simi.get("vflmul").get
        val vxrm = simi.get("vxrm").get.toInt
        
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        val vxsat = simi.get("vxsat").get.toInt == 1

        var hasRd = simi.get("RD") != None
        var expectrd = Array("h0")
        if (hasRd)
            expectrd = UtilFuncs.multilmuldatahandle(simi.get("RD").get)
        else
            expectrd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        var n_ops = n_inputs

        var vd : BigInt = 0
        var vdres = false

        // var prevVd = "h00000000000000000000000000000000"
            
        // println("1111")
        var vs2 = "h0"
        var oldvd = "h0"

        var robIdxValid = false

        for(j <- 0 until n_ops){
            if (hasVs2)
                vs2 = vs2data(n_inputs - 1)
            else
                vs2 = "h0"
            
            if (hasOldvd)
                oldvd = oldvddata(n_inputs - 1 - j)
            else
                oldvd = "h0"
            // dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)

            var ctrlBundle = ctrl.copy(
                vsew=vsew,
                vs1_imm=vs1_imm,
                vl=simi.get("vl").get.toInt,
                vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                ma = (simi.get("ma").get.toInt == 1),
                ta = (simi.get("ta").get.toInt == 1),
                vm = (simi.get("vm").get.toInt == 1),
                uopIdx=j,
                uopEnd = (j == n_ops - 1),
                vxrm = vxrm,
                vstart = getVstart(simi)
            )

            var robIdx = (false, 0)
            robIdxValid = randomFlush()
            /*if (robIdxValid) {
                robIdx = (true, 1)
            }
            ctrlBundle.robIdx = robIdx*/

            if (!robIdxValid) {
                dut.io.in.bits.poke(genVFuInput(
                    SrcBundle(
                        old_vd=oldvd,
                        vs2=vs2, 
                        vs1="h0",
                        mask=mask(0)), 
                    ctrlBundle
                ))
                // dut.io.redirect.poke(genFSMRedirect((robIdxValid, robIdxValid, 0)))
                dut.clock.step(1)
            } else {
                // flushed
                println("flushed")
                return
            }

            vd = dut.io.out.bits.vd.peek().litValue
            // prevVd = f"h$vd%016x"
            if (!hasRd) {
                vdres = f"h$vd%032x".equals(expectrd(n_inputs - 1 - j))
                Logger.printvds(f"h$vd%032x", expectrd(n_inputs - 1 - j))
                if (!vdres) dump(simi, f"h$vd%032x", expectrd(n_inputs - 1 - j))
                assert(vdres)
            }
        }

        if (hasRd) {
            vdres = f"h$vd%016x".equals(expectrd(0))
            Logger.printvds(f"h$vd%016x", expectrd(0))
            if (!vdres) dump(simi, f"h$vd%016x", expectrd(0))
            assert(vdres)
        }
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}