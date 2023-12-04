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

abstract class FPResult extends BundleGenHelper {

    var cur_res = 0
    var fflags : Int = 0
    var dump : (Map[String,String], String, String) => Unit = (a, b, c) => {}
    var n_res : Int = 0
    
    def setup(n_res : Int, dump : (Map[String,String], String, String) => Unit) = {
        this.n_res = n_res
        this.dump = dump
    }

    def narrowVdCompare(
        vd : BigInt,
        simi : Map[String, String], 
        uopIdx : Int, expectvd : Array[String]
    ) : Boolean = {
        val vflmul = simi.get("vflmul").get
        val expectvdIdx = uopIdx / 2
        val lower = uopIdx % 2 == 0
        var goldenvd = ""
        // var vd = dut.io.out.bits.vd.peek().litValue
        var dutvd = f"h$vd%032x"
        
        if (vflmul != "0.250000" && 
            vflmul != "0.500000" && 
            vflmul != "0.125000") {
            
            goldenvd = expectvd((n_res / 2) - 1 - expectvdIdx)
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
        var vdres = dutvd.equals(goldenvd)
        Logger.printvds(dutvd, goldenvd)
        if (!vdres) dump(simi, dutvd, goldenvd)
        return vdres
    }

    def trimLow(vd : String) : String = {
        return f"${vd.slice(0, vd.length - 1)}0"
    }

    def redVdCompare(
        vd : BigInt,
        simi : Map[String, String], 
        uopIdx : Int, expectvd : Array[String], widen : Boolean, only_high : Boolean = false
    ) : Boolean = {
        val vflmul = simi.get("vflmul").get

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8

        // var vd = dut.io.out.bits.vd.peek().litValue
        var vdidx = n_inputs - 1
        if (widen && (
            vflmul != "0.125000" && 
            vflmul != "0.250000" && 
            vflmul != "0.500000"
        )) vdidx = n_inputs * 2 - 1


        var dut_vd = f"h$vd%032x"
        var expect_vd = expectvd(vdidx)

        if (only_high) {
            dut_vd = trimLow(f"h$vd%032x")
            expect_vd = trimLow(expectvd(vdidx))
        }

        // println(f"dur orig: ${f"h$vd%032x"}\ndut trim: ${dut_vd}\nexp orig: ${expectvd(vdidx)}\nexp trim: ${expect_vd}")

        var vdres = dut_vd.equals(expect_vd)
        Logger.printvds(dut_vd, expect_vd)
        if (!vdres) dump(simi, dut_vd, expect_vd)
        return vdres
    }

    def narrowToOneCompare(
        vd : BigInt,
        simi : Map[String, String], 
        uopIdx : Int, expectvd : Array[String]
    ) : Boolean = {
        
        val vflmul = simi.get("vflmul").get
        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8

        // var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1))
        Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1))
        return vdres
    }

    def vCompare(
        vd : BigInt,
        simi : Map[String, String], 
        uopIdx : Int, expectvd : Array[String]
    ) : Boolean = {
        // var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%032x".equals(expectvd(n_res - 1 - uopIdx))
        Logger.printvds(f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        return vdres
    }

    def fdCompare(
        vd : BigInt,
        simi : Map[String, String], 
        uopIdx : Int, expectvd : Array[String]
    ) : Boolean = {
        // var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%x".equals(expectvd(0))
        Logger.printvds(f"h$vd%x", expectvd(0))
        if (!vdres) dump(simi, f"h$vd%x", expectvd(0))
        return vdres
    }

    def checkAndCompare(dutVd : BigInt, uopIdx : Int, simi : Map[String, String], 
            expectvd : Array[String]
    ) : Boolean = {
        println("checkAndCompare unimplemented")
        return false
    }

    def _checkAndCompare(
        dutVd : BigInt, uopIdx : Int,
        simi : Map[String, String], 
        expectvd : Array[String], 
        compFunc : (BigInt,
        Map[String, String], 
        Int, Array[String]) => Boolean = vCompare
    ) : Boolean = {
        return compFunc(dutVd, simi, uopIdx, expectvd)
    }

    /*def finished() : Boolean = {
        return cur_res == n_res
    }

    def getFflags() : Int = {
        return fflags
    }*/
}

class RedFPResult(widen : Boolean) extends FPResult {
    override def checkAndCompare(
        dutVd : BigInt, uopIdx : Int,
        simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            (d, s, i, expvd) => redVdCompare(d, s, i, expvd, widen)
        )
    }
}

class RedUSumVsFPResult(widen : Boolean) extends FPResult {
    override def checkAndCompare(dutVd : BigInt, uopIdx : Int,simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            (d, s, i, expvd) => redVdCompare(d, s, i, expvd, widen, true)
        )
    }
}

class NarrowFPResult extends FPResult {
    override def checkAndCompare(dutVd : BigInt, uopIdx : Int,simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            this.narrowVdCompare
        )
    }
}

class NarrowToOneFPResult extends FPResult {
    override def checkAndCompare(dutVd : BigInt, uopIdx : Int,simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            this.narrowToOneCompare
        )
    }
}

class FdFPResult extends FPResult {
    override def checkAndCompare(dutVd : BigInt, uopIdx : Int,simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            this.fdCompare
        )
    }
}

class NormalFPResult extends FPResult {
    override def checkAndCompare(dutVd : BigInt, uopIdx : Int,simi : Map[String, String], 
            expectvd : Array[String]) : Boolean = {
        
        _checkAndCompare(
            dutVd, uopIdx, simi, expectvd,
            this.vCompare
        )
    }
}