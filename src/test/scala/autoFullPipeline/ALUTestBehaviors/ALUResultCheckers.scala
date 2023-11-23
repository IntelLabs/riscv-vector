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
        n_ops : Int, 
        expectvd : Array[String],
        vdOrRd : Boolean,
        goldenVxsat : Boolean = false,
        dump : (String, String) => Unit = (a, b) => {}
    ) : ALUVResultChecker = {
        val resultChecker = new ALUVResultChecker(n_ops, expectvd, vdOrRd, dump)
        resultChecker.setGoldenVxsat(goldenVxsat)

        return resultChecker
    }

    def newGeneralVChecker(
        n_ops : Int, 
        expectvd : Array[String],
        dump : (String, String) => Unit = (a, b) => {}
    ) : ALUVResultChecker = {
        val resultChecker = new ALUVResultChecker(n_ops, expectvd, true, dump)

        return resultChecker
    }

    def newN21Checker(
        n_ops : Int,
        expectvd : Array[String],
        dump : (String, String) => Unit = (a, b) => {}
    ) : ALUN21ResultChecker = {
        return new ALUN21ResultChecker(n_ops, expectvd, dump)
    }

    def newVnChecker(
        n_ops : Int, 
        n_inputs : Int,
        expectvd : Array[String],
        vflmul : String,
        useVxsat : Boolean = false,
        goldenVxsat : Boolean = false,
        dump : (String, String) => Unit = (a, b) => {}
    ) : ALUVnResultChecker = {
        var rc = new ALUVnResultChecker(
            n_ops, n_inputs, expectvd, vflmul, dump
        )

        if (useVxsat) {
            rc.setGoldenVxsat(goldenVxsat)
        }

        return rc
    }

}

class ALUVmvSpecResultChecker(
    val n_inputs : Int, 
    expectvd : Array[String],
    vdOrRd : Boolean,
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(1, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (vdOrRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(n_inputs - 1 - uopIdx)
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

class ALUVResultChecker(
    n_ops : Int, 
    expectvd : Array[String],
    vdOrRd : Boolean,
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(n_ops, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (vdOrRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(n_ops - 1 - uopIdx)
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
    n_ops : Int,
    expectvd : Array[String],
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(n_ops, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (uopIdx == n_ops - 1) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(n_ops - 1)
            Logger.printvds(vdRes, goldenVd)
            correctness = vdRes.equals(goldenVd)
            if (!correctness) dump(vdRes, goldenVd)
        } 

        return correctness
    }
}

class ALUVnResultChecker(
    n_ops : Int, 
    n_inputs : Int,
    expectvd : Array[String],
    val vflmul : String,
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(n_ops, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {

        var correctness : Boolean = true
        // var vdRes : String = ""
        // var goldenVd : String = ""
        
        val expectvdIdx = uopIdx / 2
        val lower = uopIdx % 2 == 0
        var goldenvd = ""
        // var vd = dut.io.out.bits.vd.peek().litValue
        var dutvd = f"h$dutVd%032x"
        
        if (vflmul != "0.250000" && 
            vflmul != "0.500000" && 
            vflmul != "0.125000") {
            
            goldenvd = expectvd((n_ops / 2) - 1 - expectvdIdx)
            if (lower) {
                goldenvd = s"h${goldenvd.slice(17, 33)}"
                dutvd = s"h${dutvd.slice(17, 33)}"
            } else {
                goldenvd = s"h${goldenvd.slice(1, 17)}"
                dutvd = s"h${dutvd.slice(1, 17)}"
            }
        }else {
            goldenvd = expectvd(0)
        }
        correctness = dutvd.equals(goldenvd)
        Logger.printvds(dutvd, goldenvd)

        
        /*val sewIndex = n_inputs - 1 - (uopIdx / 2)
        if (uopIdx % 2 == 1 || (n_ops == 1)) {
            // compare when it's odd uopidx or it has only one uop
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(sewIndex)
            Logger.printvds(vdRes, goldenVd)
        }

        correctness = vdRes.equals(goldenVd)*/
        if (!correctness) dump(dutvd, goldenvd)

        return correctness
    }
}