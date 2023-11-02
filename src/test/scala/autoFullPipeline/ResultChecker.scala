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

object ALUVResultChecker {
    def newChecker(
        nRes : Int, 
        expectvd : Array[String],
        vdOrRd : Boolean,
        goldenVxsat : Boolean = false,
        dump : (String, String) => Unit = (a, b) => {}) = {
            val resultChecker = new ALUVResultChecker(nRes, expectvd, vdOrRd, dump)
            resultChecker.setGoldenVxsat(goldenVxsat)

            return resultChecker
    }
}

class ALUVResultChecker(
    val nRes : Int, 
    val expectvd : Array[String],
    val vdOrRd : Boolean,
    val dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(nRes, expectvd, dump) {
    
    override def checkRes(dutVd : String, uopIdx : Int) : Boolean = {
        var correctness : Boolean
        var vdRes : String
        var goldenVd : String
        if (vdOrRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(nRes - 1 - uopIdx)
        } else {
            // RD or FD
            if (uopIdx == 0) {
                vdRes = f"h$dutVd%016x"
                goldenVd = expectvd(0)
            }
        }
        correctness = vdRes.equals(goldenVd)
        Logger.printvds(vdRes, goldenVd)
        if (!correctness) dump(dutVd, goldenVd)

        return correctness
    }
}

class ResultChecker(val nRes : Int, val expectvd : Array[String], 
        val dump : (String, String) => Unit = (a, b) => {}) {
    val goldenVxsat : Boolean = false
    val goldenFflags : Int = false

    var resVxsat : Boolean = false
    var resFflags : Int = 0

    var isFPDIV : Boolean = false

    var checkedRes = 0

    def setGoldenVxsat(goldenVxsat : Boolean) = { 
        this.goldenVxsat = goldenVxsat
        this.isFPDIV = false
    }

    def setGoldenFflags(goldenFflags : Int) = {
        this.goldenFflags = goldenFflags
        this.isFPDIV = true
    }

    def isCompleted() : Boolean = { this.checkedRes == this.nRes }

    def checkRes(dutVd : String, uopIdx : Int, 
            dutVxsat : Boolean = false, dutFflags : Int = 0) : Boolean = {
        
        var res = this._checkRes(dutVd, uopIdx)

        this.resFflags |= dutFflags
        this.resVxsat = this.resVxsat || dutVxsat

        this.checkedRes += 1
        if (res) {
            if (this.isCompleted()) {
                if (this.isFPDIV) {
                    res = res && (this.goldenFflags == this.resFflags)
                    if (this.goldenFflags != this.resFflags) {
                        dump(f"(fflags) h${this.resFflags}%016x", f"(fflags) h${this.goldenFflags}%016x")
                    }
                } else {
                    res = res && (this.goldenVxsat == this.resVxsat)
                    if (this.goldenVxsat != this.resVxsat) {
                        dump(f"(vxsat) ${this.resVxsat}", f"(vxsat) ${this.goldenVxsat}")
                    }
                }
            }
        }

        return res
    }

    def _checkRes(dutVd : String, uopIdx : Int) : Boolean = {
        println("!!!!!! _checkRes not implemented")
    }
}