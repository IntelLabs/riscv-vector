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


class ALUTestEngine extends TestEngine {

    override def getName() = "ALUTestEngine"

    override def iterate(
        dut : VAluWrapper, chosenTestCase : TestCase, 
        sendRobIdx : Int, allExhausted : Boolean,
        flush : Boolean, flushedRobIdx : Int
    ) : (Boolean, Int) = {
        val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
        println(s"Sending ${chosenTestCase.instid}, robIdx ${sendRobIdx}, uop ${uopIdx}/(0-${chosenTestCase.ctrlBundles.length - 1})")

        // ===================== manipulating dut ========================

        dut.io.out.ready.poke(true.B)
        dut.io.in.valid.poke(true.B)

        dut.io.in.bits.poke(input)
        dut.clock.step(1)

        val dutVd = dut.io.out.bits.vd.peek().litValue
        val dutVxsat = dut.io.out.bits.vxsat.peek().litValue == 1

        // TODO 1.3. check for potential results, get the comparison result
        val resCorrectness = chosenTestCase.rc.checkRes(dutVd, uopIdx, dutVxsat=dutVxsat)
        val resRobIdx = sendRobIdx

        return (resCorrectness, resRobIdx)
    }
}