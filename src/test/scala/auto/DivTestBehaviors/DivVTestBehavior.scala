package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.util.control.Breaks._

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters


class VdivvvTestBehavior extends VdTestBehavior("vdiv.vv.data", ctrlBundles.vdiv_vv, "-", "vdiv_vv") {}
class VdivuvvTestBehavior extends VdTestBehavior("vdivu.vv.data", ctrlBundles.vdivu_vv, "-", "vdivu_vv") {}
class VdivvxTestBehavior extends VdTestBehavior("vdiv.vx.data", ctrlBundles.vdiv_vx, "-", "vdiv_vx") {}
class VdivuvxTestBehavior extends VdTestBehavior("vdivu.vx.data", ctrlBundles.vdivu_vx, "-", "vdivu_vx") {}

class VremvvTestBehavior extends VdTestBehavior("vrem.vv.data", ctrlBundles.vrem_vv, "-", "vrem_vv") {}
class VremuvvTestBehavior extends VdTestBehavior("vremu.vv.data", ctrlBundles.vremu_vv, "-", "vremu_vv") {}
class VremvxTestBehavior extends VdTestBehavior("vrem.vx.data", ctrlBundles.vrem_vx, "-", "vrem_vx") {}
class VremuvxTestBehavior extends VdTestBehavior("vremu.vx.data", ctrlBundles.vremu_vx, "-", "vremu_vx") {}

class VfdivvvTestBehavior extends VdTestBehavior("vfdiv.vv.data", ctrlBundles.vfdiv_vv, "-", "vfdiv_vv") {}
class VfdivvfTestBehavior extends VdTestBehavior("vfdiv.vf.data", ctrlBundles.vfdiv_vf, "-", "vfdiv_vf") {}

class VfrdivvfTestBehavior extends VdTestBehavior("vfrdiv.vf.data", ctrlBundles.vfrdiv_vf, "-", "vfrdiv_vf") {}

class VfsqrtvTestBehavior extends VdTestBehavior("vfsqrt.v.data", ctrlBundles.vfsqrt_v, "-", "vfsqrt_v") {}



class VdTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String) extends TestBehavior(fn, cb, s, instid) {
    
    override def getDut() : Module               = {
        val dut = new VDivWrapper
        return dut
    }

    override def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) : Unit = {
        var vf = simi.get("VS1") == None && (simi.get("RS1") != None || simi.get("FS1") != None)
        val v = simi.get("VS1") == None && simi.get("RS1") == None && simi.get("FS1") == None
        val vv = simi.get("VS1") != None
        val hasvs2 = simi.get("VS2") != None

        var vs1data : Array[String] = Array()
        if (vv)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        if (vf) {
            if (simi.get("RS1") != None) {
                vs1data = UtilFuncs.multilmuldatahandle(simi.get("RS1").get)
            }

            if (simi.get("FS1") != None) {
                vs1data = UtilFuncs.multilmuldatahandle(simi.get("FS1").get)
                vs1data(0) = s"h${vs1data(0).slice(17, 33)}"
            }
        }
        var vs2data : Array[String] = Array()
        if (hasvs2)
            vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        var mask = Array("hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
        if (simi.get("MASK") != None)
            mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)
        
        val vflmul = simi.get("vflmul").get
        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)
        val vxrm = simi.get("vxrm").get.toInt
        val frm = getfrm(simi)
        val vstart = getVstart(simi)
        var vm = (simi.get("vm").get.toInt == 1)

        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)

        val expectfflags = getFflags(simi)

        // ===========================================================
        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        
        var vd : BigInt = 0
        var vdres = false
        var n_res = n_inputs

        // ===========================================================
        var fpRes = new DivResult(n_res, (a, b, c) => this.dump(a, b, c, "")) // * fpRes

        // ===========================================================
        val MAX_READY_WAIT = 100
        var curReadyWait = 0
        var j = 0
        var ctrlBundles : Map[Int, CtrlBundle] = Map()
        breakable{ while (j < n_inputs){
            dut.io.in.valid.poke(true.B) // TODO randomly block

            // preparing input ============================
            var vs2 = "h0"
            if(hasvs2) vs2 = vs2data(n_inputs - 1 -j)
            var uopIdx = j
            var srcBundle = SrcBundle(
                    vs2=vs2,
                    old_vd=oldvddata(n_inputs - 1 -j),
                    mask=mask(0))
            if (vf) {
                srcBundle.rs1 = vs1data(0)
            } 
            if (vv) {
                srcBundle.vs1 = vs1data(n_inputs - 1 -j)
            }

            var ctrlBundle = ctrl.copy(
                    vsew=vsew,
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1),
                    vm = vm,
                    uopIdx=uopIdx,
                    vxrm = vxrm,
                    frm=frm,
                    vstart = vstart
                )

            // sending input ============================
            dut.io.in.bits.poke(genVFpuInput(
                srcBundle, 
                ctrlBundle
            ))

            // waiting for dut in.ready ===================
            while((dut.io.in.ready.peek().litValue != 1) &&
                    curReadyWait < MAX_READY_WAIT) {
                // check vd before clock tick ==============
                if (!fpRes.finished()) { // * fpRes
                    fpRes.checkAndCompare(dut, simi, ctrlBundles, expectvd)
                }
                dut.clock.step(1)
                curReadyWait += 1
            }

            // when it waits too long ====================
            if (!(curReadyWait < MAX_READY_WAIT)) {
                println(s"no io.ready signal received")
                dump(simi, s"(no io.ready signal received), sent ${j} uops", "(no io.ready signal received)")
            }
            assert(curReadyWait < MAX_READY_WAIT)

            println(s"sent uop ${uopIdx}")
            curReadyWait = 0

            ctrlBundles = ctrlBundles + (uopIdx -> ctrlBundle)
            
            // check vd before clock tick ==================
            if (!fpRes.finished()) { // * fpRes
                fpRes.checkAndCompare(dut, simi, ctrlBundles, expectvd)
            }
            dut.clock.step(1)
            j += 1
        } }
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.in.uop.uopEnd.poke(false.B)

        // check rest vds ======================================================================
        j = 0
        val LOOP_MAX = 100
        var curIter = 0
        var fflags : Int = 0
        breakable{ while(true) {
            if (!(curIter < LOOP_MAX)) {
                println("no vd received after LOOP_MAX")
                dump(simi, s"(no vd received after LOOP_MAX), received ${fpRes.cur_res}", "(no vd received after LOOP_MAX)")
            }
            assert(curIter < LOOP_MAX)

            if (fpRes.finished()) { // * fpRes
                break
            }
            fpRes.checkAndCompare(dut, simi, ctrlBundles, expectvd) // * fpRes

            dut.clock.step(1)

            curIter += 1
        } }

        fflags = fpRes.getFflags() // * fpRes
        var fflagsRes = fflags == expectfflags
        if (!fflagsRes) {
            println("fflags incorrect")
            dump(simi, f"(fflags) h$fflags%016x", f"(fflags) h$expectfflags%016x")
        }
        assert(fflagsRes)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) : Unit = {
        testMultiple(simi, ctrl, s, dut)
    }
}