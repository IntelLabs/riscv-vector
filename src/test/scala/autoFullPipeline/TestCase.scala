package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import java.io.FileWriter
import java.time.{LocalDate, LocalDateTime}
import java.nio.file.{Paths, Files}
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

object TestCase {
    def newNormalCase(
        instid : String, 
        srcBundles : Seq[SrcBundle], 
        ctrlBundles : Seq[CtrlBundle], rc : ResultChecker) : TestCase = {
        
        var tc = new TestCase(instid, ctrlBundles, rc)
        tc.srcBundles = srcBundles
        return tc
    }

    def newFSMCase(
        instid : String, 
        fsmSrcBundles : Seq[FSMSrcBundle], 
        ctrlBundles : Seq[CtrlBundle], rc : ResultChecker) : TestCase = {
        
        var tc = new TestCase(instid, ctrlBundles, rc)
        tc.fsmSrcBundles = fsmSrcBundles
        tc.isFSM = true
        return tc
    }
}

class TestCase(
        val instid : String,
        val ctrlBundles : Seq[CtrlBundle], 
        val rc : ResultChecker,
        var uopIx : Int = 0
        // val checkRes : (String, Boolean, Int, Int) => (Boolean, Boolean) 
        // (dut vd, vxsat, fflags, uopIdx) => (correctness, isCompleted)
    ) extends BundleGenHelper {
    var srcBundles : Seq[SrcBundle] = Seq()
    var fsmSrcBundles : Seq[FSMSrcBundle] = Seq()

    var isFSM : Boolean = false

    var flushed : Boolean = false
    def flush() = { flushed = true }

    def isCompleted() : Boolean = {
        if (this.rc.isCompleted() && !this.rc.checkBitPatCompleted()) {
            println(s"WARNING: ${instid} test case completed but some uop might not be checked..")
        }
        return this.rc.isCompleted()
    }

    def areAllAcked() : Boolean = {
        return this.rc.areAllAcked()
    }

    def ackRes() = {
        this.rc.ack()
    }

    def isExhausted() : Boolean = {
        return uopIx >= ctrlBundles.length
    }

    def getCtrlBundleByUopIdx(uopIdx : Int) : CtrlBundle = {
        for (cb <- ctrlBundles) {
            if (cb.uopIdx == uopIdx) {
                return cb
            }
        }
        assert(false, s"inst id ${instid} cannot find uop uopIdx=${uopIdx}")
        return ctrlBundles(0)
    }

    def nextVfuInput(robIdx : (Boolean, Int) = (false, 0)) : (VFuInput, Int) = {
        if (isFSM) println("ERROR: generating normal input for FSM test case")

        var ctrlBundle = ctrlBundles(uopIx)
        ctrlBundle.robIdx = robIdx
        val vfuInput: VFuInput = genVFuInput(
            srcBundles(uopIx), 
            ctrlBundle
        )

        val idx = ctrlBundles(uopIx).uopIdx
        uopIx += 1

        return (vfuInput, idx)
    }

    def nextFsmInput(robIdx : (Boolean, Int) = (false, 0)) : (VPermInput, Int) = {
        if (!isFSM) println("ERROR: generating FSM input for normal test case")

        var ctrlBundle = ctrlBundles(uopIx)
        ctrlBundle.robIdx = robIdx
        val fsmInput: VPermInput = genFSMInput(
            fsmSrcBundles(uopIx),
            ctrlBundle
        )

        uopIx += 1

        return (fsmInput, 0)
    }
}
