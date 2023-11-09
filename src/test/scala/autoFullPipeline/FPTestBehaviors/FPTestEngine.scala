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

    var historyTCs : List[(robIdx, uopIdx, TestCase)] = List()
    var historyTCIx = 0

    def checkOutput(dut : VFPUWrapper) = {
        dut.io.out.ready.poke(true.B) // TODO randomly block

        if (dut.io.out.valid.peek().litValue == 1) {

            // TODO add result to the queue

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
        sendRobIdx : Int, allExhausted : Boolean
    ) : (Boolean, Int) = {
        val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
        println(s"Sending ${chosenTestCase.instid}, uop ${uopIdx}, robIdx ${sendRobIdx}")

        // ===================== manipulating dut ========================

        if (!allExhausted) {
            dut.io.in.valid.poke(true.B) // TODO randomly block

            // sending input ====================================
            // dut.io.dontCare.poke(false.B)
            
            dut.io.in.bits.poke(input)
            dut.io.redirect.poke(genFSMRedirect())

            // waiting for dut's ready signal, which represents an ack of the uop ========
            while((dut.io.in.ready.peek().litValue != 1) &&
                    curReadyWait < MAX_READY_WAIT) {
                
                checkOutput(dut)
                dut.clock.step(1)
                curReadyWait += 1
            }

            // waits too long.. =====================================
            if (!(curReadyWait < MAX_READY_WAIT)) {
                println(s"no io.ready signal received")
                dump(simi, s"(no io.ready signal received), sending ${uopIdx}", "(no io.ready signal received)")
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
        val resCorrectness = chosenTestCase.rc.checkRes(dutVd, uopIdx, dutVxsat=dutVxsat)
        val resRobIdx = sendRobIdx

        return (resCorrectness, resRobIdx)
    }

}