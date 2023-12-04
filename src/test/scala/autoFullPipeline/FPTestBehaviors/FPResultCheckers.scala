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



class FPResultChecker(
    n_res : Int, 
    expectvd : Array[String],
    val simi : Map[String, String],
    val fpRes : FPResult) extends ResultChecker(n_res, expectvd, (a, b) => {}) {
    
    override def _checkRes(dutVd : BigInt, uopIdx : Int) : Boolean = {
        return fpRes.checkAndCompare(dutVd, uopIdx, simi, expectvd)
    }
}

