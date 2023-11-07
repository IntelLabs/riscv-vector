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
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import scala.collection.mutable.Map
import chipsalliance.rocketchip.config.Parameters

object ALUResultChecker {
    def newVChecker(
        nRes : Int, 
        expectvd : Array[String],
        vdOrRd : Boolean,
        goldenVxsat : Boolean = false,
        dump : (String, String) => Unit = (a, b) => {}) : ALUVResultChecker = {
            val resultChecker = new ALUVResultChecker(nRes, expectvd, vdOrRd, dump)
            resultChecker.setGoldenVxsat(goldenVxsat)

            return resultChecker
    }
}

class ALUVResultChecker(
    nRes : Int, 
    expectvd : Array[String],
    vdOrRd : Boolean,
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(nRes, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (vdOrRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(nRes - 1 - uopIdx)
            Logger.printvds(vdRes, goldenVd)
        } else {
            // RD or FD
            if (uopIdx == 0) {
                vdRes = f"h$dutVd%016x"
                goldenVd = expectvd(0)
                Logger.printvds(vdRes, goldenVd)
            }
        }
        correctness = vdRes.equals(goldenVd)
        if (!correctness) dump(vdRes, goldenVd)

        return correctness
    }
}

class ALUN21ResultChecker(
    expectvd : Array[String],
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(1, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (vdOrRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(nRes - 1 - uopIdx)
            Logger.printvds(vdRes, goldenVd)
        } else {
            // RD or FD
            if (uopIdx == 0) {
                vdRes = f"h$dutVd%016x"
                goldenVd = expectvd(0)
                Logger.printvds(vdRes, goldenVd)
            }
        }
        correctness = vdRes.equals(goldenVd)
        if (!correctness) dump(vdRes, goldenVd)

        return correctness
    }
}
