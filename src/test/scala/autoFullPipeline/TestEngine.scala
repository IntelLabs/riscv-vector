package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
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

val ALU_TEST_ENGINE = 0
val MAC_TEST_ENGINE = 1
val RED_TEST_ENGINE = 2
val MASK_TEST_ENGINE = 3
val PERM_TEST_ENGINE = 4
val DIV_TEST_ENGINE = 5
val FP_TEST_ENGINE = 6

object TestEngine {
    def getEngine(testEngineId : Int) : TestEngine {
        return testEngineId match {
            case ALU_TEST_ENGINE : new ALUTestEngine
            case MAC_TEST_ENGINE : {}
            case RED_TEST_ENGINE : {}
            case MASK_TEST_ENGINE : {}
            case PERM_TEST_ENGINE : {}
            case DIV_TEST_ENGINE : {}
            case FP_TEST_ENGINE : {}
        }
    }
}

class ALUTestEngine extends TestEngine {
    val MAX_PARA_INSTS = 3
    val MAX_PARA_TESTCASES = 5
    var curTestCasePool : Map[Int, TestCase] = Map()
    var testBehaviorPool : Seq[TestBehavior] = Seq()

    var nextNormalModeTBIx = 0
    var robIndex = 0

    override def run(dut:VAluWrapper) = {
        // println("ALUTestEngine run() not implemented")

        // TODO 11.1: Add redirect later..

        breakable{ while(true) {
            // TODO 1. run tests for normal mode instructions
            //  TODO MAX parallel instruction limit
            //  TODO randomly chosen between normal mode instructions
            //  TODO record done when all inputs of one instruction are fed

            // TODO 1.1. Fill insts (TestCases from TestBehavior)
            while (this.testBehaviorPool.length < MAX_PARA_TESTCASES &&
            this.nextNormalModeTBIx < this.normalModeTBs.length) {
                this.testBehaviorPool :+= this.normalModeTBs(this.nextNormalModeTBIx)
                this.nextNormalModeTBIx += 1
            }

            while (this.curTestCasePool.length < MAX_PARA_TESTCASES &&
            this.testBehaviorPool.length > 0) {
                val randomTBinPool = this.testBehaviorPool(Random.nextInt(this.testBehaviorPool.length))
                this.curTestCasePool += (this.robIndex -> randomTBinPool.getNextTestCase())
                this.robIndex += 1
            }

            // TODO 1.2. Randomly choose one among TestCases
            //  TODO 1.2.1. Randomly redirect and remove 
            //  TODO 1.2.2. If not redirecting, get next uop from it and feed it to the dut
            val chosenTestCase = Random.shuffle(myMap.toList).head._2

            // ===================== manipulating dut ========================
            dut.io.out.ready.poke(true.B)
            dut.io.in.valid.poke(true.B)

            dut.io.in.bits.poke(chosenTestCase.nextVfuInput())
            dut.clock.step(1)
            // ===============================================================

            // TODO 1.3. check for potential results, give it to corresponding TestBehavior and get the comparison result
            //  TODO 1.3.0. if the result is incorrect, record the incorrect result and remove the TestCase and TestBehavior
            //  TODO 1.3.1. check if all uops' results are checked and remove the TestCase from the pool
            //  TODO 1.3.2. check if all test cases of an TestBehavior are done and record the result, remove it from the pool
            //  TODO 1.3.2. check if all normal mode instruction tests are done, proceed to test ordered instructions

        } }

        // TODO 2. run tests for ordered instructions

    }
}

abstract class TestEngine extends BundleGenHelper {
    var normalModeTBs : Seq[TestBehavior] = Seq()
    var orderedTBs : Seq[TestBehavior] = Seq()
    
    def run(tbs: Seq[TestBehavior], dut:Module) = {
        if(this.normalModeTBs.length == 0 && this.orderedTBs.length == 0) {
            this.fillTBs(tbs)
        }

        dut match {
            case alu_dut : VAluWrapper => run(alu_dut)
            case mac_dut : VMacWrapper => run(mac_dut)
            case mask_dut : VMask => run(mask_dut)
            case dut : VFPUWrapper => run(dut)
            case dut : VDivWrapper => run(dut)
            case dut : Reduction => run(dut)
            case perm_dut : Permutation => run(perm_dut)
        }
    }

    def fillTBs(testBehaviors : Seq[TestBehavior]) = {
        for (i <- 0 until testBehaviors.length) {
            if (!testBehaviors(i).isOrdered) {
                this.normalModeTBs :+= testBehaviors(i)
            } else {
                this.orderedTBs :+= testBehaviors(i)
            }
        }
    }

    def run(dut:VAluWrapper) = { println("!!!!!!called unimplemented run alu")}
    def run(dut:Permutation) = { println("!!!!!!called unimplemented run perm")}
    def run(dut:VMacWrapper) = { println("!!!!!!called unimplemented run mac")}
    def run(dut:VMask) = { println("!!!!!!called unimplemented run mask")}
    def run(dut:VFPUWrapper) = { println("!!!!!!called unimplemented run FPU")}
    def run(dut:VDivWrapper) = { println("!!!!!!called unimplemented run Div")}
    def run(dut:Reduction) = { println("!!!!!!called unimplemented run Reduction")}
}