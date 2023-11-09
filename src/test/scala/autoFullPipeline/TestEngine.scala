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

object TestEngine {
    val ALU_TEST_ENGINE = 0
    val MAC_TEST_ENGINE = 1
    val RED_TEST_ENGINE = 2
    val MASK_TEST_ENGINE = 3
    val PERM_TEST_ENGINE = 4
    val DIV_TEST_ENGINE = 5
    val FP_TEST_ENGINE = 6

    def getEngine(testEngineId : Int) : TestEngine = {
        return testEngineId match {
            case 0 => new ALUTestEngine
            /*case MAC_TEST_ENGINE : {}
            case RED_TEST_ENGINE : {}
            case MASK_TEST_ENGINE : {}
            case PERM_TEST_ENGINE : {}
            case DIV_TEST_ENGINE : {}
            case FP_TEST_ENGINE : {}*/
        }
    }
}

class ALUTestEngine extends TestEngine {

    override def getName() = "ALUTestEngine"

    override def iterate(dut : VAluWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {
        val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
        println(s"Sending ${chosenTestCase.instid}, uop ${uopIdx}, robIdx ${sendRobIdx}")

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


    /*def runThroughTBs(
        dut:VAluWrapper, tbs:Seq[TestBehavior], 
        MAX_PARA_INSTS: Int = 2, MAX_PARA_TESTCASES: Int = 3
    ) = {
        
        var tbIx : Int = 0
        var curTestCasePool : Map[Int, (TestBehavior, TestCase)] = Map()
        var testBehaviorPool : Seq[TestBehavior] = Seq()

        var exhaustedCount : Int = 0

        breakable{ while(true) {
            if (tbIx >= tbs.length 
                && testBehaviorPool.length == 0
                && curTestCasePool.isEmpty) {
                break
            }
            // TODO 1. run tests for normal mode instructions
            //  TODO MAX parallel instruction limit
            //  TODO randomly chosen between normal mode instructions
            //  TODO record done when all inputs of one instruction are fed


            // TODO 1.1. Fill insts (TestCases from TestBehavior)
            while (testBehaviorPool.length < MAX_PARA_TESTCASES &&
            tbIx < tbs.length) {
                testBehaviorPool :+= tbs(tbIx)
                tbIx += 1
            }

            while ((curTestCasePool.size - exhaustedCount) < MAX_PARA_TESTCASES &&
            testBehaviorPool.length > 0) {
                val randomTBinPool = testBehaviorPool(Random.nextInt(testBehaviorPool.length))
                curTestCasePool += (this.robIndex -> (randomTBinPool, randomTBinPool.getNextTestCase()))
                this.robIndex += 1
            }

            // TODO 1.2. Randomly choose one among TestCases
            //  TODO 1.2.1. Randomly redirect and remove 
            //  TODO 1.2.2. If not redirecting, get next uop from it and feed it to the dut
            val randomTestCase = Random.shuffle(curTestCasePool.filter(!_._2._2.isExhausted()).toList).head
            val (chosenTestBehavior, chosenTestCase) : (TestBehavior, TestCase) = randomTestCase._2
            val sendRobIdx = randomTestCase._1
            val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
            if (chosenTestCase.isExhausted()) 
                exhaustedCount += 1
            println(s"Sending ${chosenTestCase.instid}, uop ${uopIdx}, robIdx ${sendRobIdx}")

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
            // ===============================================================

            //  TODO 1.3.0. if the result is incorrect, record the incorrect result and remove the TestCase and TestBehavior
            val (resTestBehavior, resTestCase) : (TestBehavior, TestCase) = curTestCasePool(resRobIdx)
            if (!resCorrectness) {
                println(s"${resTestCase.instid}, result incorrect")

                resTestBehavior.recordFail()
                curTestCasePool = curTestCasePool.filterNot(_._2._1 == resTestBehavior)
                testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)

                exhaustedCount = curTestCasePool.filter(_._2._2.isExhausted()).size
            } else {
                //  TODO 1.3.1. check if all uops' results are checked and remove the TestCase from the pool
                if (resTestCase.isCompleted()) {
                    curTestCasePool = curTestCasePool.filterNot(_._2._2 == resTestCase)
                    exhaustedCount -= 1

                    //  TODO 1.3.2. check if TestBehavior are done and record the result, remove it from the pool
                    if (!curTestCasePool.values.exists(_._1 == resTestBehavior) &&
                            resTestBehavior.isFinished()) {
                        println(s"${resTestBehavior.getInstid()}, tests are done.")
                        Dump.recordDone(s"${resTestBehavior.getInstid()}")
                        resTestBehavior.recordSuccess()
                    }
                }
            }

            //  TODO 1.3.2. check if all test cases of an TestBehavior are fetched
            if (resTestBehavior.isFinished()) {
                testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)
            }
        } }
    }

    override def run(dut:VAluWrapper) = {

        // TODO 11.1: Add redirect later..
        this.runThroughTBs(dut, this.normalModeTBs)
        println("TestEngine: All normal mode instructions are tested")

        println("Starting Tests for Ordered Instructions ============================")

        this.runThroughTBs(dut, this.orderedTBs, 1, 1)

        println("TestEngine: All ordered mode instructions are tested")
    }*/
}

abstract class TestEngine extends BundleGenHelper {
    var normalModeTBs : Seq[TestBehavior] = Seq()
    var orderedTBs : Seq[TestBehavior] = Seq()

    var robIndex = 0
    
    def getName() = "TestEngine"
    def getDut() = new VAluWrapper

    def runThroughTBs(
        dut:Module, tbs:Seq[TestBehavior], 
        MAX_PARA_INSTS: Int = 2, MAX_PARA_TESTCASES: Int = 3
    ) = {
        
        var tbIx : Int = 0
        var curTestCasePool : Map[Int, (TestBehavior, TestCase)] = Map()
        var testBehaviorPool : Seq[TestBehavior] = Seq()

        var exhaustedCount : Int = 0

        breakable{ while(true) {
            if (tbIx >= tbs.length 
                && testBehaviorPool.length == 0
                && curTestCasePool.isEmpty) {
                break
            }
            // TODO 1. run tests for normal mode instructions
            //  TODO MAX parallel instruction limit
            //  TODO randomly chosen between normal mode instructions
            //  TODO record done when all inputs of one instruction are fed


            // TODO 1.1. Fill insts (TestCases from TestBehavior)
            while (testBehaviorPool.length < MAX_PARA_TESTCASES &&
            tbIx < tbs.length) {
                testBehaviorPool :+= tbs(tbIx)
                tbIx += 1
            }

            while ((curTestCasePool.size - exhaustedCount) < MAX_PARA_TESTCASES &&
            testBehaviorPool.length > 0) {
                val randomTBinPool = testBehaviorPool(Random.nextInt(testBehaviorPool.length))
                curTestCasePool += (this.robIndex -> (randomTBinPool, randomTBinPool.getNextTestCase()))
                this.robIndex += 1
            }

            // TODO 1.2. Randomly choose one among TestCases
            //  TODO 1.2.1. Randomly redirect and remove 
            //  TODO 1.2.2. If not redirecting, get next uop from it and feed it to the dut
            println(s"curTestCasePool.size: ${curTestCasePool.size}, exhaustedCount: ${exhaustedCount}")
            val nonExhavustedTestCases = curTestCasePool.filter(!_._2._2.isExhausted()).toList
            var stepRes : (Boolean, Int) = (false, 0)
            if (nonExhavustedTestCases.length != 0) {
                val randomTestCase = Random.shuffle(nonExhavustedTestCases).head
                val (chosenTestBehavior, chosenTestCase) : (TestBehavior, TestCase) = randomTestCase._2
                val sendRobIdx = randomTestCase._1
                
                stepRes = iterate(dut, chosenTestCase, sendRobIdx)
                if (chosenTestCase.isExhausted()) 
                    exhaustedCount += 1
            } else {
                stepRes = iterate(dut, curTestCasePool.toList.head._2._2, -1, true)
            }
            val resCorrectness = stepRes._1
            val resRobIdx = stepRes._2
            // val (input, uopIdx) : (VFuInput, Int) = chosenTestCase.nextVfuInput((true, sendRobIdx))
            // println(s"Sending ${chosenTestCase.instid}, uop ${uopIdx}, robIdx ${sendRobIdx}")

            // // ===================== manipulating dut ========================

            // // dut.io.out.ready.poke(true.B)
            // // dut.io.in.valid.poke(true.B)

            // // dut.io.in.bits.poke(input)
            // // dut.clock.step(1)

            // // val dutVd = dut.io.out.bits.vd.peek().litValue
            // // val dutVxsat = dut.io.out.bits.vxsat.peek().litValue == 1

            // // // TODO 1.3. check for potential results, get the comparison result
            // // val resCorrectness = chosenTestCase.rc.checkRes(dutVd, uopIdx, dutVxsat=dutVxsat)
            // // val resRobIdx = sendRobIdx
            // ===============================================================

            //  TODO 1.3.0. if the result is incorrect, record the incorrect result and remove the TestCase and TestBehavior
            val (resTestBehavior, resTestCase) : (TestBehavior, TestCase) = curTestCasePool(resRobIdx)
            if (!resCorrectness) {
                println(s"${resTestCase.instid}, result incorrect")

                resTestBehavior.recordFail()
                curTestCasePool = curTestCasePool.filterNot(_._2._1 == resTestBehavior)
                testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)

                exhaustedCount = curTestCasePool.filter(_._2._2.isExhausted()).size
            } else {
                //  TODO 1.3.1. check if all uops' results are checked and remove the TestCase from the pool
                if (resTestCase.isCompleted()) {
                    curTestCasePool = curTestCasePool.filterNot(_._2._2 == resTestCase)
                    exhaustedCount -= 1

                    //  TODO 1.3.2. check if TestBehavior are done and record the result, remove it from the pool
                    if (!curTestCasePool.values.exists(_._1 == resTestBehavior) &&
                            resTestBehavior.isFinished()) {
                        println(s"${resTestBehavior.getInstid()}, tests are done.")
                        Dump.recordDone(s"${resTestBehavior.getInstid()}")
                        resTestBehavior.recordSuccess()
                    }
                }
            }

            //  TODO 1.3.2. check if all test cases of an TestBehavior are fetched
            if (resTestBehavior.isFinished()) {
                testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)
            }
        } }
    }

    def run(dut:Module) = {

        // TODO 11.1: Add redirect later..
        this.runThroughTBs(dut, this.normalModeTBs)
        println("TestEngine: All normal mode instructions are tested")

        println("Starting Tests for Ordered Instructions ============================")

        this.runThroughTBs(dut, this.orderedTBs, 1, 1)

        println("TestEngine: All ordered mode instructions are tested")
    }
    
    /*def run(dut:Module) : Unit = {
        /*if(this.normalModeTBs.length == 0 && this.orderedTBs.length == 0) {
            this.fillTBs(tbs)
        }*/

        dut match {
            case alu_dut : VAluWrapper => run(alu_dut)
            case mac_dut : VMacWrapper => run(mac_dut)
            case mask_dut : VMask => run(mask_dut)
            case dut : VFPUWrapper => run(dut)
            case dut : VDivWrapper => run(dut)
            case dut : Reduction => run(dut)
            case perm_dut : Permutation => run(perm_dut)
        }
    }*/

    def fillTBs(testBehaviors : Seq[TestBehavior]) = {
        for (i <- 0 until testBehaviors.length) {
            if (!testBehaviors(i).isOrdered) {
                this.normalModeTBs :+= testBehaviors(i)
            } else {
                this.orderedTBs :+= testBehaviors(i)
            }
        }
    }

    def test_init(dut : Module) {
        dut match {
            case alu_dut : VAluWrapper => TestHarnessAlu.test_init(alu_dut)
            case mac_dut : VMacWrapper => TestHarnessMac.test_init(mac_dut)
            case mask_dut : VMask => TestHarnessMask.test_init(mask_dut)
            case dut : VFPUWrapper => TestHarnessFPU.test_init(dut)
            case dut : VDivWrapper => TestHarnessDiv.test_init(dut)
            case dut : Reduction => {}
            case perm_dut : Permutation => TestHarnessFSM.test_init(perm_dut)
        }
    }

    def iterate(dut : Module, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean = false) : (Boolean, Int) = {
        return dut match {
            case alu_dut : VAluWrapper => iterate(alu_dut, chosenTestCase, sendRobIdx, allExhausted)
            case mac_dut : VMacWrapper => iterate(mac_dut, chosenTestCase, sendRobIdx, allExhausted)
            case mask_dut : VMask => iterate(mask_dut, chosenTestCase, sendRobIdx, allExhausted)
            case dut : VFPUWrapper => iterate(dut, chosenTestCase, sendRobIdx, allExhausted)
            case dut : VDivWrapper => iterate(dut, chosenTestCase, sendRobIdx, allExhausted)
            case dut : Reduction => iterate(dut, chosenTestCase, sendRobIdx, allExhausted)
            case perm_dut : Permutation => iterate(perm_dut, chosenTestCase, sendRobIdx, allExhausted)
        }
    }

    def iterate(dut:VAluWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run alu"); return (false, 0)}
    def iterate(dut:Permutation, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run perm"); return (false, 0)}
    def iterate(dut:VMacWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run mac"); return (false, 0)}
    def iterate(dut:VMask, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run mask"); return (false, 0)}
    def iterate(dut:VFPUWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run FPU"); return (false, 0)}
    def iterate(dut:VDivWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run Div"); return (false, 0)}
    def iterate(dut:Reduction, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean) : (Boolean, Int) = {println("!!!!!!called unimplemented run Reduction"); return (false, 0)}
}