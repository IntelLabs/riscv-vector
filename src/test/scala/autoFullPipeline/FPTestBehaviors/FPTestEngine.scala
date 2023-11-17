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

    override def getName() = "FPTestEngine"
    override def getDut() = new VFPUWrapper

    var historyTCs : List[(Int, Int, TestCase)] = List() // robIdx, uopIdx, TestCase
    var historyTCIx = 0

    var results : List[(Boolean, Int)] = List()

    def clearFlushedRes(robIdx : Int) = {
        results = results.filter(_._2 >= robIdx)
    }

    def checkOutput(dut : VFPUWrapper) = {
        val block = randomBlock()
        dut.io.out.ready.poke((!block).B) // TODO randomly block

        println(s".. checkOutput block = ${block}, ready = ${!block}")

        if (!block && dut.io.out.valid.peek().litValue == 1) {

            println(".. valid == true, checking results..")
            
            var robIdx = dut.io.out.bits.uop.sysUop.robIdx.value.peek().litValue.toInt
            var uopIdx = dut.io.out.bits.uop.uopIdx.peek().litValue.toInt

            while(
                historyTCs.length > 0 &&
                // && (historyTCs(0)._1 != robIdx || historyTCs(0)._2 != uopIdx)
                historyTCs(0)._3.flushed
            ) {
                if (historyTCs(0)._1 == robIdx) {
                    println(s"ERROR!!!!!! Received flushed result for robIdx ${robIdx}, uop ${uopIdx}!!!!!!")
                    assert(false)
                }
                historyTCs = historyTCs.tail
            }

            if (historyTCs.length == 0) {
                println(s"historyTCs is empty!!!!!!!!")
                assert(false)
            }

            val resRobIdx = historyTCs(0)._1
            val resUopIdx = historyTCs(0)._2
            val resTestCase = historyTCs(0)._3
            val testCaseFlushed = resTestCase.flushed
            historyTCs = historyTCs.tail
            
            println(s"2.2. Received result for robIdx ${robIdx}, uopIdx ${uopIdx}, in FPTestEngine:")
            if (testCaseFlushed) {
                println(".. 2.2.1. flushed! so not comparing")
            } else {
                val cb = resTestCase.getCtrlBundleByUopIdx(resUopIdx)
                val uop = genVFuUop(cb)
                dut.io.out.bits.uop.expect(uop)

                val dutVd = dut.io.out.bits.vd.peek().litValue
                val fflags = dut.io.out.bits.fflags.peek().litValue.toInt

                val resCorrectness = resTestCase.rc.checkRes(dutVd, uopIdx, dutFflags=fflags)
                val resRobIdx = robIdx

                results :+= (resCorrectness, resRobIdx)
            }

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

        val MAX_READY_WAIT = 100
        var curReadyWait = 0
        
        if (!allExhausted) {
            val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((false, sendRobIdx))
            println(s"1. Sending ${chosenTestCase.instid}, robIdx ${sendRobIdx}, uop ${uopIdx}/(0-${chosenTestCase.ctrlBundles.length - 1})")
            // ===================== manipulating dut ========================

            dut.io.in.valid.poke(true.B) // TODO randomly block

            // sending input ====================================
            // dut.io.dontCare.poke(false.B)
            
            dut.io.in.bits.poke(input)
            if (flush) {
                dut.io.redirect.poke(genFSMRedirect(flush, flush, flushedRobIdx))
            } else {
                dut.io.redirect.poke(genFSMRedirect())
            }

            // in the same cycle, before marking flushed instructions, check output
            //  If there's any, the historyTCs item will be removed
            //  and one result will be added
            checkOutput(dut)

            // Tag test cases' uops that should be flushed
            // and no vd received yet
            for (i <- 0 until historyTCs.length) {
                if(historyTCs(i)._1 < flushedRobIdx) {
                    historyTCs(i)._3.flush()
                }
            }

            // clear past results of test case with less robIdx
            clearFlushedRes(flushedRobIdx)
            println(s"2. Flushed (all < ${flushedRobIdx}), from FPTestEngine")
            // dut.clock.step(1)
            // dut.io.redirect.poke(genFSMRedirect())

            // waiting for dut's ready signal, which represents an ack of the uop ========
            while((dut.io.in.ready.peek().litValue != 1) &&
                    curReadyWait < MAX_READY_WAIT) {
                
                if (curReadyWait != 0) checkOutput(dut)
                dut.clock.step(1)
                dut.io.redirect.poke(genFSMRedirect())
                curReadyWait += 1
            }

            // waits too long.. =====================================
            if (!(curReadyWait < MAX_READY_WAIT)) {
                println(s"ERROR!!! no io.ready signal received")
                assert(false)
            }
            assert(curReadyWait < MAX_READY_WAIT)

            // dut.io.in.valid.poke(false.B)

            if (
                chosenTestCase.rc.n_ops != 1 ||
                (chosenTestCase.rc.n_ops == 1 && chosenTestCase.isExhausted())
            ) {
                historyTCs :+= (sendRobIdx, uopIdx, chosenTestCase)
            }

            if (curReadyWait > 0) {
                // Here, the Engine passed through the while loop,
                //  clock ticked inside it
                //  then one should check one more time for the result
                
                // if the Engine didn't enter the while loop (curReadyWait == 0),
                //  then the engine didn't tick after "sending the input and checking
                //  the result for the first time".
                //  then here we should not check for the result second time.
                checkOutput(dut)
            }
            dut.clock.step(1)
        } else {
            dut.io.in.valid.poke(false.B)
            dut.io.in.bits.uop.uopEnd.poke(false.B)

            // dut.io.dontCare.poke(true.B)
            dut.io.in.bits.poke(getEmptyVFuInput()) // don't care..
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