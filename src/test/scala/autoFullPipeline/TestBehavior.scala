package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import java.io.FileWriter
import java.time.{LocalDate, LocalDateTime}
import java.nio.file.{Paths, Files}
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import scala.collection.mutable.LinkedList

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


abstract class TestBehavior(filename : String, val ctrl : CtrlBundle, sign : String, val instid : String) extends BundleGenHelper {

    var inputMaps : LinkedList[Map[String, String]] = LinkedList()
    var inputMapCurIx = 0
    var totalInputs = 0

    var testResult = true

    var mapLoaded : Boolean = false

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: TestBehavior => this.instid == other.getInstid()
            case _ => false
        }
    }

    def getTestfilePath() : String              = Datapath.testdataRoot + filename
    def getCtrlBundle() : CtrlBundle    = ctrl
    def getInstid() : String                    = instid

    def recordFail() = {this.testResult = false}
    def recordSuccess() = {this.testResult = true}
    def isFinished() : Boolean = {
        return mapLoaded && (this.inputMapCurIx >= this.totalInputs)
    }

    // change depending on test behavior =================================
    def getTargetTestEngine(): Int = TestEngine.ALU_TEST_ENGINE
    def isOrdered(): Boolean = {
        // println(s"!!!!!!!! $instid not specified isOrdered")
        false
    }
    // ====================================================================

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
                this.totalInputs = this.inputMaps.length
            }
        } else {
            println(s"Data file does not exist for instruction: ${getInstid()} , skipping")
            Dump.recordIncorrectInst(getInstid())
        }

        mapLoaded = true
    }

    def getNextTestCase() : TestCase = {
        if (!this.mapLoaded) {
            this._readInputsToMap()
        }

        val testCase = this._getNextTestCase(
            this.inputMaps.head
        )
        this.inputMaps = this.inputMaps.tail
        this.inputMapCurIx += 1

        // println(s"Adding ${this.instid}, number ${this.inputMapCurIx - 1} input to pool")
        return testCase
    }

    def _getNextTestCase(simi : Map[String, String]) : TestCase

    def destory() = {
        this.inputMaps = null
    }
}