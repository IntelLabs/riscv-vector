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
    def newNormalCase(
        instid : String, 
        srcBundles : Seq[SrcBundle], 
        ctrlBundles : Seq[CtrlBundle], rc : ResultChecker) {
        
        var tc = new TestCase(instid, ctrlBundles, rc)
        tc.srcBundles = srcBundles
        return tc
    }

    def newFSMCase(
        instid : String, 
        fsmSrcBundles : Seq[SrcBundle], 
        ctrlBundles : Seq[CtrlBundle], rc : ResultChecker) {
        
        var tc = new TestCase(instid, ctrlBundles, rc)
        tc.fsmSrcBundles = fsmSrcBundles
        tc.isFSM = true
        return tc
    }
}

class TestCase(
        val instid : String,
        val ctrlBundles : Seq[CtrlBundle], 
        val rc : ResultChecker,
        // val checkRes : (String, Boolean, Int, Int) => (Boolean, Boolean) 
        // (dut vd, vxsat, fflags, uopIdx) => (correctness, isCompleted)
    ) {
    var srcBundles : Seq[SrcBundle] = Seq()
    var fsmSrcBundles : Seq[FSMSrcBundle] = Seq()

    var isFSM : Boolean = false
    var uopIx = 0

    def isCompleted() : Boolean = {
        return this.rc.isCompleted()
    }

    def nextVfuInput() = {
        if (this.isFSM) println("ERROR: generating normal input for FSM test case")

        val vfuInput = genVFuInput(
            this.srcBundles[this.uopIx], 
            this.ctrlBundles[this.uopIx]
        )
        this.uopIx += 1

        return (vfuInput, this.ctrlBundles[this.uopIx].uopIdx)
    }

    def nextFsmInput() = {
        if (!this.isFSM) println("ERROR: generating FSM input for normal test case")

        val fsmInput = genFSMInput(
            this.fsmSrcBundles[this.uopIx],
            this.ctrlBundles[this.uopIx]
        )

        this.uopIx += 1

        return (fsmInput, 0)
    }
}

abstract class TestBehavior(filename : String, ctrlBundle : CtrlBundle, sign : String, instid : String) extends BundleGenHelper {

    var inputMaps : Seq[Map[String, String]] = Seq()
    var inputMapCurIx = 0

    // var testResult = true

    def getTestfilePath() : String              = Datapath.testdataRoot + filename
    def getCtrlBundle() : CtrlBundle    = ctrlBundle
    def getInstid() : String                    = instid

    // full pipeline
    def getTargetTestEngine(): Int = TestEngine.ALU_TEST_ENGINE
    def isOrdered(): Boolean = {
        println(s"!!!!!!!! $instid not specified isOrdered")
        false
    }
    // def recordFail() = {this.testResult = false}
    // def recordSuccess() = {this.testResult = true}
    def isFinished() = {
        return this.inputMapCurIx >= this.inputMaps.length
    }
    
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

                this.inputMaps = keymap
            }
        } else {
            println(s"Data file does not exist for instruction: ${tb.getInstid()} , skipping")
            Dump.recordIncorrectInst(tb.getInstid())
        }
    }

    def getNextTestCase() = {
        if (this.inputMaps.length == 0) {
            this._readInputsToMap()
        }

        val testCase = this._getNextTestCase(this.inputMaps(this.inputMapCurIx))
        this.inputMapCurIx += 1

        println(s"Adding ${this.instid}, number ${this.inputMapCurIx - 1} input to pool")
        return testCase
    }

    def _getNextTestCase(simi : Map[String, String]) = {
        // TODO generate test case
        println("!!!!!! called unimplemented _getNextTestCase()!!")
    }
}