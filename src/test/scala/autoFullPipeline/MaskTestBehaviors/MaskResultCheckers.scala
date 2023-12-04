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

class MaskVcpopVidResultChecker(
    n_ops : Int, 
    expectvd : Array[String],
    hasRd : Boolean,
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(n_ops, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        if (!hasRd) {
            vdRes = f"h$dutVd%032x"
            goldenVd = expectvd(n_ops - 1 - uopIdx)
            Logger.printvds(vdRes, goldenVd)
        } else {
            // RD or FD
            if (uopIdx == n_ops - 1) {
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

class MaskVmResultChecker(
    n_inputs : Int, 
    expectvd : Array[String],
    dump : (String, String) => Unit = (a, b) => {}) extends ResultChecker(1, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true
        var vdRes : String = ""
        var goldenVd : String = ""
        
        vdRes = f"h$dutVd%032x"
        goldenVd = expectvd(n_inputs - 1)
        Logger.printvds(vdRes, goldenVd)
        
        correctness = vdRes.equals(goldenVd)
        if (!correctness) dump(vdRes, goldenVd)

        return correctness
    }
}