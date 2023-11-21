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



class RedResultChecker(
    val n_inputs : Int, 
    val widen : Boolean,
    val vflmul : String,
    expectvd : Array[String],
    dump : (String, String) => Unit
) extends ResultChecker(1, expectvd, dump) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        var correctness : Boolean = true

        if (uopIdx == n_inputs - 1) {
            var vdidx = n_inputs - 1
            if (widen && (
                vflmul != "0.125000" && 
                vflmul != "0.250000" && 
                vflmul != "0.500000"
            )) vdidx = n_inputs * 2 - 1

            var goldenVd = expectvd(vdidx)
            var resVd = f"h$dutVd%032x"
            correctness = resVd.equals(goldenVd)
            if (!correctness) dump(resVd, goldenVd)
            Logger.printvds(resVd, goldenVd)
        } else {
            println(s"received red ${uopIdx}")
        }

        return correctness
    }
}

