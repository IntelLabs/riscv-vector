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
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import darecreek.exu.vfu.VInstructions._

class RedTestEngine extends TestEngine {

    override def getName() = "RedTestEngine"
    override def getDut() = new Reduction

    var historyTCs : List[(Int, Int, TestCase)] = 
        List() // robIdx, uopIdx, TestCase, flushed
    var historyTCIx = 0

    var results : List[(Boolean, Int)] = List()

    def clearFlushedRes(robIdx : Int) = {
        results = results.filter(_._2 >= robIdx)
    }

    def checkOutput(dut : Reduction) = {
        if (dut.io.out.valid.peek().litValue == 1) {
            // var robIdx = dut.io.out.bits.uop.sysUop.robIdx.value.peek().litValue.toInt
            // var uopIdx = dut.io.out.bits.uop.uopIdx.peek().litValue.toInt

            // Filter out flushed test cases' uops
            /*while(
                historyTCs.length > 0 &&
                !historyTCs(0)._4
                // && (historyTCs(0)._1 != robIdx || historyTCs(0)._2 != uopIdx)
            ) {
                historyTCs = historyTCs.tail
            }

            if (historyTCs.length == 0) {
                println(s"ERROR!!!!!! Result comes but nothing valid in the historyTCs!!!!!!!!\nIs it flushed before?")
                assert(false)
            }*/

            val robIdx = historyTCs(0)._1
            val uopIdx = historyTCs(0)._2
            val resTestCase = historyTCs(0)._3
            val testCaseFlushed = resTestCase.flushed
            historyTCs = historyTCs.tail

            val dutVd = dut.io.out.bits.vd.peek().litValue

            println(s"2.2. Received result, Comparing with ${resTestCase.instid} robIdx ${robIdx}, uopIdx ${uopIdx}, in RedTestEngine:")

            val resCorrectness = resTestCase.rc.checkRes(dutVd, uopIdx)
            val resRobIdx = robIdx

            if (testCaseFlushed) {
                println(".. 2.2.1. flushed! so not comparing")
            } else {
                results :+= (resCorrectness, resRobIdx)
            }
        }
    }


    override def iterate(
        dut : Reduction, chosenTestCase : TestCase, 
        sendRobIdx : Int, allExhausted : Boolean, 
        flush : Boolean, flushedRobIdx : Int
    ) : (Boolean, Int) = {
        
        val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
        println(s"2. Sending ${chosenTestCase.instid}, uop ${uopIdx}, robIdx ${sendRobIdx}")

        // ===================== manipulating dut ========================
        if(!allExhausted) {
            dut.io.in.valid.poke(true.B)

            dut.io.in.bits.poke(input)

            if (flush) {
                for (i <- 0 until historyTCs.length) {
                    if(historyTCs(i)._1 < flushedRobIdx) {
                        historyTCs(i)._3.flush()
                    }
                }
            }
            /*if (flush) {
                dut.io.redirect.poke(genFSMRedirect((flush, flush, flushedRobIdx)))
            } else {*/
            // dut.io.redirect.poke(genFSMRedirect())
            // }

            // Only check for the result after the last uop
            if (chosenTestCase.isExhausted()) 
                historyTCs :+= (sendRobIdx, uopIdx, chosenTestCase)

            // TestEngine (outside) needs not know the flushed result.
            clearFlushedRes(flushedRobIdx)
        }

        dut.clock.step(1)

        dut.io.in.valid.poke(false.B)
        // dut.io.redirect.poke(genFSMRedirect())
        checkOutput(dut)

        if (results.length > 0) {
            val resCorrectness = results(0)._1
            val resRobIdx = results(0)._2
            results = results.tail
            return (resCorrectness, resRobIdx)
        }

        return (false, NO_RESULT_ROBIDX)
    }

}