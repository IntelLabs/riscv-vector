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

object RandomGen {
    val rand = new Random(seed = 42)
}

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
            case 1 => new MacTestEngine
            case 2 => new RedTestEngine
            case 3 => new MaskTestEngine
            case 5 => new DivTestEngine
            case 6 => new FPTestEngine
            /*case MAC_TEST_ENGINE : {}
            case PERM_TEST_ENGINE : {}
            case DIV_TEST_ENGINE : {}*/
        }
    }
}


abstract class TestEngine extends BundleGenHelper {
    val NO_RESULT_ROBIDX = -1

    var normalModeTBs : Seq[TestBehavior] = Seq()
    var orderedTBs : Seq[TestBehavior] = Seq()

    var robIndex : Int = 0

    var flush = false
    var flushedRobIdx = 0
    
    def getName() = "TestEngine"
    def getDut() : Module = new VAluWrapper

    def advRobIdx() = {
        robIndex = (robIndex + 1) % 256
    }

    def randomFlush() : Boolean = {
        return RandomGen.rand.nextInt(100) > 90
    }

    def randomBlock() : Boolean = {
        return RandomGen.rand.nextInt(100) > 80
    }

    def tbSuccess(tb : TestBehavior) = {
        println(s"${tb.getInstid()}, tests are done.")
        Dump.recordDone(s"${tb.getInstid()}")
        tb.recordSuccess()
    }

    def runThroughTBs(
        dut:Module, tbs:Seq[TestBehavior], 
        MAX_PARA_INSTS: Int = 3, MAX_PARA_TESTCASES: Int = 6
    ) : Unit = {
        
        var curTestCasePool : Map[Int, (TestBehavior, TestCase)] = Map()
        var exhaustedCount : Int = 0

        var tbIx : Int = 0
        var testBehaviorPool : Seq[TestBehavior] = Seq()
        var failedTBs : Map[Int, TestBehavior] = Map()


        while(true) {
        breakable{
            if (
                tbIx >= tbs.length 
                && testBehaviorPool.length == 0
                && curTestCasePool.isEmpty
            ) {
                // no more test case and test behavior
                return
            }
            
            // refill test behavior
            while (
                testBehaviorPool.length < MAX_PARA_TESTCASES &&
                tbIx < tbs.length
            ) {
                testBehaviorPool :+= tbs(tbIx)
                tbIx += 1
            }

            // refill test case
            while (
                (curTestCasePool.size - exhaustedCount) < MAX_PARA_TESTCASES &&
                testBehaviorPool.length > 0
            ) {
                val randIx = RandomGen.rand.nextInt(testBehaviorPool.length)
                val randomTBinPool = testBehaviorPool(randIx)
                if (randomTBinPool.isFinished()) {
                    testBehaviorPool = testBehaviorPool.filterNot(_ == randomTBinPool)
                } else {
                    curTestCasePool += (this.robIndex -> (randomTBinPool, randomTBinPool.getNextTestCase()))
                    failedTBs = failedTBs - this.robIndex
                    
                    println(s"0. Adding ${randomTBinPool.getInstid()}, robIdx ${robIndex} to the pool")
                    advRobIdx()
                }
            }

            // TODO 1.2. Randomly choose one among TestCases
            //  TODO 1.2.1. Randomly redirect and remove 
            //  TODO 1.2.2. If not redirecting, get next uop from it and feed it to the dut
            // println(s"curTestCasePool.size: ${curTestCasePool.size}, exhaustedCount: ${exhaustedCount}")
            val nonExhavustedTestCases = curTestCasePool.filter(!_._2._2.isExhausted()).toList
            var stepRes : (Boolean, Int) = (false, 0)
            
            if (nonExhavustedTestCases.length != 0) {
                val randomTestCase = RandomGen.rand.shuffle(nonExhavustedTestCases).head
                val (chosenTestBehavior, chosenTestCase) : (TestBehavior, TestCase) = randomTestCase._2
                val sendRobIdx = randomTestCase._1

                // TODO randomly flush
                if (!flush) {
                    flush = randomFlush()
                    if (flush && (robIndex <= flushedRobIdx)) { // flush compare
                        // in case of robIdx wrapped around, send=0 flush=255
                        println("1.1. flushedRobIdx wrapped around! Ignoring..")
                        flush = false
                    }

                    if (flush) {
                        flushedRobIdx = sendRobIdx // + 1
                        println(s"1.1. Flush (all <= ${flushedRobIdx})")

                        val prevSize = curTestCasePool.size
                        var deletedTBs : Set[TestBehavior] = Set()

                        // delete flushed test cases, so TestEngine will not
                        // give new uops of them to the next level detailed TestEngine
                        curTestCasePool = curTestCasePool.filterNot(x => {
                            if (x._1 <= flushedRobIdx) { // flush compare
                                val tb : TestBehavior = x._2._1
                                deletedTBs = deletedTBs + tb
                                println(s".. deleted robIdx ${x._1}")
                            }
                            x._1 <= flushedRobIdx // flush compare
                        })

                        // if any TB has no test case left, record success
                        for (tb <- deletedTBs) {
                            if (!curTestCasePool.values.exists(_._1 == tb) &&
                                tb.isFinished()
                            ) {
                                tbSuccess(tb)
                            }
                        }

                        println(s"1.2. curTestCasePool shrinked from ${prevSize} to ${curTestCasePool.size}")
                        exhaustedCount = curTestCasePool.filter(_._2._2.isExhausted()).size
                        
                        // give uop and flush info in the next iteration
                        break // continue
                    }
                }

                println(s"0.1. Before sending to the dut, flush=${flush}, flushedRobIdx=${flushedRobIdx}")
                stepRes = iterate(dut, chosenTestCase, sendRobIdx, flush=flush, flushedRobIdx=flushedRobIdx)
                if (chosenTestCase.isExhausted()) 
                    exhaustedCount += 1
                
                /*if (flush) {
                    val prevSize = curTestCasePool.size
                    curTestCasePool = curTestCasePool.filterNot(_._1 < sendRobIdx)
                    println(s"3. (After flush) curTestCasePool shrinked from ${prevSize} to ${curTestCasePool.size}")
                    exhaustedCount = curTestCasePool.filter(_._2._2.isExhausted()).size
                }*/
                flush = false
            } else {
                if (curTestCasePool.size == 0) break
                // Rest test cases are all waiting for results, no more uop to give
                stepRes = iterate(dut, curTestCasePool.toList.head._2._2, -1, true, flush=flush, flushedRobIdx=flushedRobIdx)
            }
            val resCorrectness = stepRes._1
            val resRobIdx = stepRes._2

            if (resRobIdx != NO_RESULT_ROBIDX) {
                //  TODO 1.3.0. if the result is incorrect, record the incorrect result and remove the TestCase and TestBehavior
                // TODO When robIdx does not exist in the pool.. failed or flushed..
                if (!curTestCasePool.contains(resRobIdx)) {
                    if (failedTBs.contains(resRobIdx)) {
                        println(s"WARNING: Received result ${resRobIdx} for already incorrect ${failedTBs(resRobIdx).getInstid()}.. Ignoring..")
                        println("=============================================================================")
                        break
                    } else {
                        /*for ((key, value) <- curTestCasePool) {
                            // Do something with each key-value pair
                            println(s".. in curTestCasePool, robIdx: $key, Value: $value")
                        }*/
                        assert(false, s"ERROR!!! Received result for flushed robIdx ${resRobIdx}!!!")
                    }
                }

                val (resTestBehavior, resTestCase) : (TestBehavior, TestCase) = curTestCasePool(resRobIdx)
                // println(s"4. Received results for ${resTestCase.instid}, robIdx ${resRobIdx}")
                resTestCase.ackRes()
                if (!resCorrectness) {
                    println(s"${resTestCase.instid}, result incorrect")

                    resTestBehavior.recordFail()
                    curTestCasePool.foreach(x => {
                        if (x._2._1 == resTestBehavior) {
                            failedTBs += (x._1 -> resTestBehavior)
                        }
                    })
                    curTestCasePool = curTestCasePool.filterNot(_._2._1 == resTestBehavior)
                    testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)

                    exhaustedCount = curTestCasePool.filter(_._2._2.isExhausted()).size
                } else {
                    //  TODO 1.3.1. check if all uops' results are checked and remove the TestCase from the pool
                    if (resTestCase.isCompleted() && resTestCase.areAllAcked()) {
                        curTestCasePool = curTestCasePool.filterNot(x => {
                            if (x._2._2 == resTestCase)
                                println(s".. Completed, deleting robIdx ${x._1}")
                            x._2._2 == resTestCase
                        })
                        exhaustedCount -= 1

                        //  TODO 1.3.2. check if TestBehavior are done and record the result, remove it from the pool
                        if (
                            !curTestCasePool.values.exists(_._1 == resTestBehavior) &&
                            resTestBehavior.isFinished()
                        ) {
                            tbSuccess(resTestBehavior)
                        }
                    }
                }

                //  check if all test cases of an TestBehavior are fetched
                // NOTICE that after this, there might still be some test cases 
                // of the removed TestBehavior running
                if (resTestBehavior.isFinished()) {
                    testBehaviorPool = testBehaviorPool.filterNot(_ == resTestBehavior)
                }
            } else {
                println("Waiting for TestCase result..")
            }
            println("=============================================================================")
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
            case dut : VDiv => TestHarnessDiv.test_init(dut)
            case dut : Reduction => {}
            case perm_dut : Permutation => TestHarnessFSM.test_init(perm_dut)
        }
    }

    def iterate(dut : Module, chosenTestCase : TestCase, sendRobIdx : Int, 
        allExhausted : Boolean = false, flush : Boolean = false, flushedRobIdx : Int = 0
    ) : (Boolean, Int) = {
        return dut match {
            case alu_dut : VAluWrapper => iterate(alu_dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case mac_dut : VMacWrapper => iterate(mac_dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case mask_dut : VMask => iterate(mask_dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case dut : VFPUWrapper => iterate(dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case dut : VDiv => iterate(dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case dut : Reduction => iterate(dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
            case perm_dut : Permutation => iterate(perm_dut, chosenTestCase, sendRobIdx, allExhausted, flush, flushedRobIdx)
        }
    }

    def iterate(dut:VAluWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run alu"); return (false, 0)}
    def iterate(dut:Permutation, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run perm"); return (false, 0)}
    def iterate(dut:VMacWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run mac"); return (false, 0)}
    def iterate(dut:VMask, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run mask"); return (false, 0)}
    def iterate(dut:VFPUWrapper, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run FPU"); return (false, 0)}
    def iterate(dut:VDiv, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run Div"); return (false, 0)}
    def iterate(dut:Reduction, chosenTestCase : TestCase, sendRobIdx : Int, allExhausted : Boolean, flush : Boolean, flushedRobIdx : Int) : (Boolean, Int) = {println("!!!!!!called unimplemented run Reduction"); return (false, 0)}
}