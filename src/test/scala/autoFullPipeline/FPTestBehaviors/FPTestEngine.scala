package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map
import scala.util.Random

import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import chipsalliance.rocketchip.config.Parameters
import scala.util.control.Breaks._

class FPTestEngine extends TestEngine {

    var curReadyWait = 0

    override def getName() = "FPTestEngine"
    override def getDut() = new VFPUWrapper

    var historyTCs : List[(Int, Int, TestCase)] = List() // robIdx, uopIdx, TestCase
    var historyTCIx = 0

    var results : List[(Boolean, Int)] = List()

    def checkOutput(dut : VFPUWrapper) = {
        dut.io.out.ready.poke(true.B) // TODO randomly block

        if (dut.io.out.valid.peek().litValue == 1) {
            
            var robIdx = dut.io.out.bits.uop.sysUop.robIdx.value.peek().litValue.toInt
            var uopIdx = dut.io.out.bits.uop.uopIdx.peek().litValue.toInt

            // TODO add result to the queue
            while(historyTCs.length > 0 && historyTCs(0)._1 != robIdx) {
                historyTCs = historyTCs.tail
            }

            if (historyTCs.length == 0) {
                println(s"robIdx ${robIdx} is not in the historyTCs!!!!!!!!")
                assert(false)
            }

            val resTestCase = historyTCs(0)._3
            historyTCs = historyTCs.tail

            val dutVd = dut.io.out.bits.vd.peek().litValue
            val fflags = dut.io.out.bits.fflags.peek().litValue.toInt

            val resCorrectness = resTestCase.rc.checkRes(dutVd, uopIdx, dutFflags=fflags)
            val resRobIdx = robIdx

            println(s"2.2. Received result for robIdx ${resRobIdx}, uopIdx ${uopIdx}, in FPTestEngine")
            results :+= (resCorrectness, resRobIdx)

            /*var uopIdx = dut.io.out.bits.uop.uopIdx.peek().litValue.toInt
            // var srcBundle = srcBundles(uopIdx)
            var ctrlBundle = ctrlBundles(uopIdx)
            var uop = genVFuUop(ctrlBundle)
            
            println(s"checking for result of uop ${uopIdx}")
            dut.io.out.bits.uop.expect(uop) // TODO check uop

            compFunc(dut, simi, uopIdx, expectvd)

            fflags = fflags | dut.io.out.bits.fflags.peek().litValue.toInt
            cur_res += 1*/
        }
    }

    override def iterate(
        dut : VFPUWrapper, chosenTestCase : TestCase, 
        sendRobIdx : Int, allExhausted : Boolean, 
        flush : Boolean, flushedRobIdx : Int
    ) : (Boolean, Int) = {
        
        val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
        println(s"1. Sending ${chosenTestCase.instid}, uop ${uopIdx}/${chosenTestCase.ctrlBundles.length}, robIdx ${sendRobIdx}")
        // ===================== manipulating dut ========================

        val MAX_READY_WAIT = 100
        
        if (!allExhausted) {
            dut.io.in.valid.poke(true.B) // TODO randomly block

            // sending input ====================================
            // dut.io.dontCare.poke(false.B)
            
            dut.io.in.bits.poke(input)
            if (flush) {
                println(s"2. Flushed (all < ${flushedRobIdx}), from FPTestEngine")
                dut.io.redirect.poke(genFSMRedirect(flush, flush, flushedRobIdx))
            } else {
                dut.io.redirect.poke(genFSMRedirect())
            }

            // waiting for dut's ready signal, which represents an ack of the uop ========
            while((dut.io.in.ready.peek().litValue != 1) &&
                    curReadyWait < MAX_READY_WAIT) {
                
                checkOutput(dut)
                dut.clock.step(1)
                // dut.io.redirect.poke(genFSMRedirect())
                curReadyWait += 1
            }

            // waits too long.. =====================================
            if (!(curReadyWait < MAX_READY_WAIT)) {
                println(s"ERROR!!! no io.ready signal received")
                assert(false)
            }
            assert(curReadyWait < MAX_READY_WAIT)
            curReadyWait = 0

            historyTCs :+= (sendRobIdx, uopIdx, chosenTestCase)

            checkOutput(dut)
            dut.clock.step(1)
        } else {
            dut.io.in.valid.poke(false.B)
            dut.io.in.bits.uop.uopEnd.poke(false.B)

            // dut.io.dontCare.poke(true.B)
            dut.io.in.bits.poke(input) // don't care..
            dut.io.redirect.poke(genFSMRedirect())

            checkOutput(dut)
            dut.clock.step(1)
        }

        // TODO 1.3. check for potential results, get the comparison result
        // val resCorrectness = chosenTestCase.rc.checkRes(dutVd, uopIdx, dutVxsat=dutVxsat)
        // val resRobIdx = sendRobIdx
        if (results.length > 0) {
            val resCorrectness = results(0)._1
            val resRobIdx = results(0)._2
            results = results.tail
            return (resCorrectness, resRobIdx)
        }
        return (false, NO_RESULT_ROBIDX)
    }

}