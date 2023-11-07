package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chipsalliance.rocketchip.config.Parameters

class VmadcvvTestBehavior extends VmadcsbcTestBehavior("vmadc.vv.data", ctrlBundles.vmadc_vv, "u", "vmadc_vv") {}
class VmadcvvmTestBehavior extends VmadcsbcTestBehavior("vmadc.vvm.data", ctrlBundles.vmadc_vvm, "u", "vmadc_vvm", _vm = Option(false)) {}

class VmsbcvvTestBehavior extends VmadcsbcTestBehavior("vmsbc.vv.data", ctrlBundles.vmsbc_vv, "u", "vmsbc_vv") {}
class VmsbcvvmTestBehavior extends VmadcsbcTestBehavior("vmsbc.vvm.data", ctrlBundles.vmsbc_vvm, "u", "vmsbc_vvm", _vm = Option(false)) {}

class VmseqvvTestBehavior extends VmadcsbcTestBehavior("vmseq.vv.data", ctrlBundles.vmseq_vv, "u", "vmseq_vv") {}
class VmsnevvTestBehavior extends VmadcsbcTestBehavior("vmsne.vv.data", ctrlBundles.vmsne_vv, "u", "vmsne_vv") {}
class VmsltuvvTestBehavior extends VmadcsbcTestBehavior("vmsltu.vv.data", ctrlBundles.vmsltu_vv, "u", "vmsltu_vv") {}
class VmsleuvvTestBehavior extends VmadcsbcTestBehavior("vmsleu.vv.data", ctrlBundles.vmsleu_vv, "u", "vmsleu_vv") {}

class VmsltvvTestBehavior extends VmadcsbcTestBehavior("vmslt.vv.data", ctrlBundles.vmslt_vv, "s", "vmslt_vv") {}
class VmslevvTestBehavior extends VmadcsbcTestBehavior("vmsle.vv.data", ctrlBundles.vmsle_vv, "s", "vmsle_vv") {}

class VmsgtuvxTestBehavior extends VmadcsbcTestBehavior("vmsgtu.vx.data", ctrlBundles.vmsgtu_vx, "u", "vmsgtu_vx") {}
class VmsgtvxTestBehavior extends VmadcsbcTestBehavior("vmsgt.vx.data", ctrlBundles.vmsgt_vx, "s", "vmsgt_vx") {}

class VmadcvxTestBehavior extends VmadcsbcTestBehavior("vmadc.vx.data", ctrlBundles.vmadc_vx, "u", "vmadc_vx") {}
class VmadcvxmTestBehavior extends VmadcsbcTestBehavior("vmadc.vxm.data", ctrlBundles.vmadc_vxm, "u", "vmadc_vxm", _vm = Option(false)) {}

class VmsbcvxTestBehavior extends VmadcsbcTestBehavior("vmsbc.vx.data", ctrlBundles.vmsbc_vx, "u", "vmsbc_vx") {}
class VmsbcvxmTestBehavior extends VmadcsbcTestBehavior("vmsbc.vxm.data", ctrlBundles.vmsbc_vxm, "u", "vmsbc_vxm", _vm = Option(false)) {}

class VmseqvxTestBehavior extends VmadcsbcTestBehavior("vmseq.vx.data", ctrlBundles.vmseq_vx, "u", "vmseq_vx") {}
class VmsnevxTestBehavior extends VmadcsbcTestBehavior("vmsne.vx.data", ctrlBundles.vmsne_vx, "u", "vmsne_vx") {}
class VmsltuvxTestBehavior extends VmadcsbcTestBehavior("vmsltu.vx.data", ctrlBundles.vmsltu_vx, "u", "vmsltu_vx") {}
class VmsleuvxTestBehavior extends VmadcsbcTestBehavior("vmsleu.vx.data", ctrlBundles.vmsleu_vx, "u", "vmsleu_vx") {}

class VmsltvxTestBehavior extends VmadcsbcTestBehavior("vmslt.vx.data", ctrlBundles.vmslt_vx, "s", "vmslt_vx") {}
class VmslevxTestBehavior extends VmadcsbcTestBehavior("vmsle.vx.data", ctrlBundles.vmsle_vx, "s", "vmsle_vx") {}

class VmsgtuviTestBehavior extends VmadcsbcTestBehavior("vmsgtu.vi.data", ctrlBundles.vmsgtu_vi, "u", "vmsgtu_vi") {}
class VmseqviTestBehavior extends VmadcsbcTestBehavior("vmseq.vi.data", ctrlBundles.vmseq_vi, "u", "vmseq_vi") {}
class VmsleuviTestBehavior extends VmadcsbcTestBehavior("vmsleu.vi.data", ctrlBundles.vmsleu_vi, "u", "vmsleu_vi") {}
class VmsgtviTestBehavior extends VmadcsbcTestBehavior("vmsgt.vi.data", ctrlBundles.vmsgt_vi, "s", "vmsgt_vi") {}
class VmsneviTestBehavior extends VmadcsbcTestBehavior("vmsne.vi.data", ctrlBundles.vmsne_vi, "u", "vmsne_vi") {}
class VmsleviTestBehavior extends VmadcsbcTestBehavior("vmsle.vi.data", ctrlBundles.vmsle_vi, "s", "vmsle_vi") {}

class VmadcviTestBehavior extends VmadcsbcTestBehavior("vmadc.vi.data", ctrlBundles.vmadc_vi, "u", "vmadc_vi") {}
class VmadcvimTestBehavior extends VmadcsbcTestBehavior("vmadc.vim.data", ctrlBundles.vmadc_vim, "u", "vmadc_vim", _vm = Option(false)) {}


class VmadcsbcTestBehavior(fn : String, cb : CtrlBundle, s : String, instid : String, 
    _vm : Option[Boolean] = None) extends TestBehavior(fn, cb, s, instid) {
    
    override def isOrdered() : Boolean = true

    override def _getNextTestCase(simi:Map[String,String]) : TestCase = {
        var vx = simi.get("RS1") != None
        var vv = simi.get("VS1") != None
        var vs1data : Array[String] = Array()
        if (vv)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("VS1").get)
        if (vx)
            vs1data = UtilFuncs.multilmuldatahandle(simi.get("RS1").get)

        var wmask = simi.get("MASK") != None
        val vs2data = UtilFuncs.multilmuldatahandle(simi.get("VS2").get)
        val oldvddata = UtilFuncs.multilmuldatahandle(simi.get("OLD_VD").get)
        
        val vflmul = simi.get("vflmul").get
        val vxsat = simi.get("vxsat").get.toInt == 1
        val expectvd = UtilFuncs.multilmuldatahandle(simi.get("VD").get)
        val vxrm = simi.get("vxrm").get.toInt

        val vsew = UtilFuncs.vsewconvert(simi.get("vsew").get)

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8
        var n_ops = n_inputs
        
        var vd : BigInt = 0
        var prevVd = oldvddata(n_ops - 1)
        var vdres = false
            
        // println("1111")
        var vm = (simi.get("vm").get.toInt == 1)
        _vm match {
            case Some(__vm) => vm = __vm
            case None => {}
        }
        for(j <- 0 until n_ops){
            val reversej = n_ops - 1 - j
            var srcBundle = SrcBundle(
                vs2=vs2data(reversej),
                // old_vd=prevVd
                old_vd=oldvddata(reversej)
            )
            if (vx) {
                srcBundle.rs1 = vs1data(0)
            }
            if (vv) {
                srcBundle.vs1 = vs1data(reversej)
            }

            if (wmask) {
                srcBundle.mask = UtilFuncs.multilmuldatahandle(simi.get("MASK").get)(0)
            }

            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)
            dut.io.in.bits.poke(genVFuInput(
                // vs2 has been changed
                srcBundle, 
                ctrl.copy(
                    vsew=vsew,
                    vs1_imm=getImm(simi),
                    narrow_to_1=true, 
                    vl=simi.get("vl").get.toInt,
                    vlmul = UtilFuncs.lmulconvert(vflmul).toInt, 
                    ma = (simi.get("ma").get.toInt == 1),
                    ta = (simi.get("ta").get.toInt == 1), // true, // ta = 1?
                    vm = vm,
                    uopIdx=j,
                    uopEnd=j == n_ops - 1,
                    vxrm = vxrm,
                    vstart = getVstart(simi)
                )
            ))
            dut.clock.step(1)
            // finalVxsat = finalVxsat || dut.io.out.bits.vxsat.peek().litValue == 1
            vd = dut.io.out.bits.vd.peek().litValue
            // prevVd = f"h$vd%032x"
        }
        vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1))
        Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1))
        assert(vdres)
        // assert(finalVxsat == vxsat)
    }

    override def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) : Unit = {
        testMultiple(simi,ctrl,s, dut)
    }
}
