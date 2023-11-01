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

class ALUVResultChecker(
    val nRes : Int, 
    val expectvd : Array[String],
    val vdOrRd : Boolean) extends ResultChecker(nRes, expectvd) {
    
    override def checkRes(dutVd : String, uopIdx : Int) {
        if (vdOrRd) {
            vd = dut.io.out.bits.vd.peek().litValue
            vdRes = f"h$vd%032x"
            vdres = vdRes.equals(expectvd(nRes - 1 - uopIdx))
            Logger.printvds(vdRes, expectvd(j))
            if (!vdres) dump(simi, vdRes, expectvd(j))
            assert(vdres)
        }
    }
}

class ResultChecker(val nRes : Int, val expectvd : Array[String]) {
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
        if (this.isCompleted()) {
            if (this.isFPDIV) {
                res = res && (this.goldenFflags == this.resFflags)
            } else {
                res = res && (this.goldenVxsat == this.resVxsat)
            }
        }

        return res
    }

    def _checkRes(dutVd : String, uopIdx : Int) : Boolean = {
        println("!!!!!! _checkRes not implemented")
    }
}