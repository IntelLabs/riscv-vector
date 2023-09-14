package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
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

    def narrowVdCompare(dut : VFPUWrapper, simi : Map[String, String], uopIdx : Int, expectvd : Array[String]) = {
        val vflmul = simi.get("vflmul").get
        val expectvdIdx = uopIdx / 2
        val lower = uopIdx % 2 == 0
        var goldenvd = ""
        var vd = dut.io.out.bits.vd.peek().litValue
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
        assert(vdres)
    }

    def redVdCompare(dut : VFPUWrapper, simi : Map[String, String], 
            uopIdx : Int, expectvd : Array[String], widen : Boolean) = {
        val vflmul = simi.get("vflmul").get

        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8

        var vd = dut.io.out.bits.vd.peek().litValue
        var vdidx = n_inputs - 1
        if (widen && (
            vflmul != "0.125000" && 
            vflmul != "0.250000" && 
            vflmul != "0.500000"
        )) vdidx = n_inputs * 2 - 1
        var vdres = f"h$vd%032x".equals(expectvd(vdidx))
        Logger.printvds(f"h$vd%032x", expectvd(vdidx))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(vdidx))
        assert(vdres)
    }

    def narrowToOneCompare(dut : VFPUWrapper, simi : Map[String, String], 
            uopIdx : Int, expectvd : Array[String]) = {
        
        val vflmul = simi.get("vflmul").get
        var n_inputs = 1
        if(vflmul == "2.000000") n_inputs = 2
        if(vflmul == "4.000000") n_inputs = 4
        if(vflmul == "8.000000") n_inputs = 8

        var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%032x".equals(expectvd(n_inputs - 1))
        Logger.printvds(f"h$vd%032x", expectvd(n_inputs - 1))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_inputs - 1))
        assert(vdres)
    }

    def vCompare(dut : VFPUWrapper, simi : Map[String, String], 
            uopIdx : Int, expectvd : Array[String]) = {
        var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%032x".equals(expectvd(n_res - 1 - uopIdx))
        Logger.printvds(f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        if (!vdres) dump(simi, f"h$vd%032x", expectvd(n_res - 1 - uopIdx))
        assert(vdres)
    }

    def fdCompare(dut : VFPUWrapper, simi : Map[String, String], 
            uopIdx : Int, expectvd : Array[String]) = {
        var vd = dut.io.out.bits.vd.peek().litValue
        var vdres = f"h$vd%x".equals(expectvd(0))
        Logger.printvds(f"h$vd%x", expectvd(0))
        if (!vdres) dump(simi, f"h$vd%x", expectvd(0))
        assert(vdres)
    }

    def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {println("checkAndCompare unimplemented")}

    def _checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String], 
            compFunc : (VFPUWrapper, Map[String, String], 
                Int, Array[String]) => Unit = vCompare) = {
        dut.io.out.ready.poke(true.B) // TODO randomly block

        if (dut.io.out.valid.peek().litValue == 1) {

            var uopIdx = dut.io.out.bits.uop.uopIdx.peek().litValue.toInt
            // var srcBundle = srcBundles(uopIdx)
            var ctrlBundle = ctrlBundles(uopIdx)
            var uop = genVFuUop(ctrlBundle)
            
            println(s"checking for result of uop ${uopIdx}")
            dut.io.out.bits.uop.expect(uop) // TODO check uop

            compFunc(dut, simi, uopIdx, expectvd)

            fflags = fflags | dut.io.out.bits.fflags.peek().litValue.toInt
            cur_res += 1
        }
    }

    def finished() : Boolean = {
        return cur_res == n_res
    }

    def getFflags() : Int = {
        return fflags
    }
}

class RedFPResult(widen : Boolean) extends FPResult {
    override def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {
        
        _checkAndCompare(
            dut, simi, ctrlBundles, expectvd,
            (d, s, i, expvd) => redVdCompare(d, s, i, expvd, widen)
        )
    }
}

class NarrowFPResult extends FPResult {
    override def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {
        
        _checkAndCompare(
            dut, simi, ctrlBundles, expectvd,
            this.narrowVdCompare
        )
    }
}

class NarrowToOneFPResult extends FPResult {
    override def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {
        
        _checkAndCompare(
            dut, simi, ctrlBundles, expectvd,
            this.narrowToOneCompare
        )
    }
}

class FdFPResult extends FPResult {
    override def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {
        
        _checkAndCompare(
            dut, simi, ctrlBundles, expectvd,
            this.fdCompare
        )
    }
}

class NormalFPResult extends FPResult {
    override def checkAndCompare(dut : VFPUWrapper, simi : Map[String, String], 
            ctrlBundles : Map[Int, CtrlBundle], expectvd : Array[String]) = {
        
        _checkAndCompare(
            dut, simi, ctrlBundles, expectvd,
            this.vCompare
        )
    }
}