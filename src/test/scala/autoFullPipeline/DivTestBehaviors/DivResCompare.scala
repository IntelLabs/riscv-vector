package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import scala.util.control.Breaks._

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.VInstructions._

class DivResult(val n_res : Int, val dump : (Map[String,String], String, String) => Unit) extends BundleGenHelper {

    var cur_res = 0
    var fflags : Int = 0

    def vCompare(vd : BigInt, simi : Map[String, String], 
            uopIdx : Int, expectvd : Array[String]) : Boolean = {
        // var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%032x".equals(expectvd(n_res - 1 - uopIdx))
        Logger.printvds(f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        // assert(vdres)

        return vdres
    }

    def checkAndCompare(dutVd : BigInt, uopIdx : Int, simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        vCompare(dutVd, simi, uopIdx, expectvd)
    }
}