package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import scala.collection.mutable.Map
import chipsalliance.rocketchip.config.Parameters

object TestCase {
    def newNormalCase(srcBundles : Seq[SrcBundle], ctrlBundles : Seq[CtrlBundle], 
            expectvd : Array[String]) {
        var tc = new TestCase(ctrlBundles, expectvd)
        tc.srcBundles = srcBundles
        return tc
    }

    def newFSMCase(fsmSrcBundles : Seq[SrcBundle], ctrlBundles : Seq[CtrlBundle], 
            expectvd : Array[String]) {
        var tc = new TestCase(ctrlBundles, expectvd)
        tc.fsmSrcBundles = fsmSrcBundles
        tc.isFSM = true
        return tc
    }
}

class TestCase(val ctrlBundles : Seq[CtrlBundle], val expectvd : Array[String]) {
    var srcBundles : Seq[SrcBundle] = Seq()
    var fsmSrcBundles : Seq[FSMSrcBundle] = Seq()

    var isFSM : Boolean = false
    var uopIx = 0

    def isCompleted() = {
        return this.uopIx == this.ctrlBundles.length
    }

    def nextVfuInput() = {
        if (this.isFSM) println("ERROR: generating normal input for FSM test case")

        val vfuInput = genVFuInput(
            this.srcBundles[this.uopIx], 
            this.ctrlBundles[this.uopIx]
        )
        this.uopIx += 1

        return vfuInput
    }

    def nextFsmInput() = {
        if (!this.isFSM) println("ERROR: generating FSM input for normal test case")

        val fsmInput = genFSMInput(
            this.fsmSrcBundles[this.uopIx],
            this.ctrlBundles[this.uopIx]
        )

        this.uopIx += 1

        return fsmInput
    }
}

abstract class TestBehavior(filename : String, ctrlBundle : TestCtrlBundleBase, sign : String, instid : String) extends BundleGenHelper {

    var lmulLsOneDone : Boolean = false
    var inputMap : Seq[Map[String, String]] = Seq()
    var inputMapCurIx = 0

    def setLmulLsOneDone() = {lmulLsOneDone = true}
    def getLmulLsOneDone() = lmulLsOneDone

    def getTestfilePath() : String              = Datapath.testdataRoot + filename
    def getCtrlBundle() : TestCtrlBundleBase    = ctrlBundle
    def getSign() : String                      = sign
    def getInstid() : String                    = instid

    // full pipeline
    def getTargetTestEngine(): Int = ALU_TEST_ENGINE
    def isOrdered(): Boolean = false
    
    def getDut() : Module               = {
        val dut = new VAluWrapper
        return dut
    }

    def getHexfield(simi : Map[String,String], keyname : String) : Int = {
        if(simi.get(keyname) != None) {
            val fflags = simi.get(keyname).get
            var startFrom = 2
            if (fflags.slice(0, 4).equals("0x0x")) startFrom = 4
            // println(s"fflags.slice(0, 4): ${fflags.slice(0, 4)},")
            var parsed = Integer.parseInt(fflags.slice(startFrom, fflags.length), 16)
            // println(keyname, fflags, parsed)
            return parsed
        }
        return 0
    }

    def getImm(simi : Map[String,String]) : Int = {
        var hasImm = simi.get("IMM") != None
        // var hasUimm = simi.get("UIMM") != None
        if (hasImm) return getHexfield(simi, "IMM")
        return getHexfield(simi, "UIMM")
    }

    def getVstart(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "VSTART")
    }

    def getFflags(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "fflags")
    }

    def getfrm(simi : Map[String,String]) : Int = {
        return getHexfield(simi, "frm")
    }

    def dump(simi : Map[String,String], dut_out : String, golden_vd : String, fault_wb : String = "") = {
        //println("fault_wb in TestBehavior", fault_wb)
        /*TestResults.addResult(TestResults.InstTestRes(
            this.getInstid(),
            true,
            dut_out,
            golden_vd,
            fault_wb
        ))*/
        Dump.dump(simi, instid, dut_out, golden_vd, fault_wb=fault_wb)
    }

    def _readInputsToMap() = {
        val dataSplitIx = sys.props.getOrElse("dataSplitIx", "0").toInt
        val dataSplitN = sys.props.getOrElse("dataSplitN", "1").toInt
        val dataSplitInst = sys.props.getOrElse("dataSplitInst", "")
        val dataSplitMode : Boolean = !dataSplitInst.equals("")

        val test_file = this.getTestfilePath()
        val inst = this.getCtrlBundle()

        if (Files.exists(Paths.get(test_file))) {
            val key = ReadTxt.readFromTxtByline(test_file)
            val hasvstart1 = ReadTxt.hasVstart(key)

            var each_input_n_lines = ReadTxt.getEachInputNLines(key)
            println(s"Each input has $each_input_n_lines lines")

            var dataN = 1
            var j = 0
            if (dataSplitMode) {
                dataN = dataSplitN
                j = dataSplitIx
            }

            val each_asisgned_lines = ReadTxt.getNEachAssignedLines(key.length, j, dataN, each_input_n_lines)
            val startingIndex = j * each_asisgned_lines
            if (startingIndex < key.length) {
                println(s"Data Split $j / $dataN: $startingIndex + $each_asisgned_lines, total ${key.length}")
                
                val keymap = ReadTxt.KeyFileUtil(key.slice(startingIndex, startingIndex + each_asisgned_lines))

                this.inputMap = keymap
            }
        } else {
            println(s"Data file does not exist for instruction: ${tb.getInstid()} , skipping")
            Dump.recordIncorrectInst(tb.getInstid())
        }
    }

    def getNextTestCase() = {
        if (this.inputMap.length == 0) {
            this._readInputsToMap()
        }

        return this._getNextTestCase()
    }

    def _getNextTestCase() = {
        println("!!!!!! called unimplemented _getNextTestCase()!!")
    }

    def verifyVd() = {}

    /*def test_init(dut : Module) {
        dut match {
            case alu_dut : VAluWrapper => TestHarnessAlu.test_init(alu_dut)
            case mac_dut : VMacWrapper => TestHarnessMac.test_init(mac_dut)
            case mask_dut : VMask => TestHarnessMask.test_init(mask_dut)
            case dut : VFPUWrapper => TestHarnessFPU.test_init(dut)
            case dut : VDivWrapper => TestHarnessDiv.test_init(dut)
            case dut : Reduction => {}
            case perm_dut : Permutation => TestHarnessFSM.test_init(perm_dut)
        }
    }

    def testMultiple(simi:Map[String,String],ctrl:TestCtrlBundleBase,s:String, dut:Module) {
        dut match {
            case alu_dut : VAluWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, alu_dut)
            case mac_dut : VMacWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, mac_dut)
            case mask_dut : VMask => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, mask_dut)
            case dut : VFPUWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : VDivWrapper => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : Reduction => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case perm_dut : Permutation => testMultiple(simi,ctrl.asInstanceOf[CtrlBundle],s, perm_dut)
        }
    }

    def testSingle(simi:Map[String,String],ctrl:TestCtrlBundleBase,s:String, dut:Module) {
        dut match {
            case alu_dut : VAluWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, alu_dut)
            case mac_dut : VMacWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, mac_dut)
            case mask_dut : VMask => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, mask_dut)
            case dut : VFPUWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : VDivWrapper => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case dut : Reduction => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, dut)
            case perm_dut : Permutation => testSingle(simi,ctrl.asInstanceOf[CtrlBundle],s, perm_dut)
        }
    }

    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) = { println("!!!!!!called unimplemented testMultiple alu")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VAluWrapper) = { println("!!!!!!called unimplemented testSingle alu") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Permutation) = { println("!!!!!!called unimplemented testMultiple perm")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Permutation) = { println("!!!!!!called unimplemented testSingle perm") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) = { println("!!!!!!called unimplemented testMultiple mac")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMacWrapper) = { println("!!!!!!called unimplemented testSingle mac") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) = { println("!!!!!!called unimplemented testMultiple mask")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VMask) = { println("!!!!!!called unimplemented testSingle mask") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VFPUWrapper) = { println("!!!!!!called unimplemented testMultiple FPU")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VFPUWrapper) = { println("!!!!!!called unimplemented testSingle FPU") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) = { println("!!!!!!called unimplemented testMultiple Div")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:VDivWrapper) = { println("!!!!!!called unimplemented testSingle Div") }
    def testMultiple(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Reduction) = { println("!!!!!!called unimplemented testMultiple Reduction")}
    def testSingle(simi:Map[String,String],ctrl:CtrlBundle,s:String, dut:Reduction) = { println("!!!!!!called unimplemented testSingle Reduction") }*/

}