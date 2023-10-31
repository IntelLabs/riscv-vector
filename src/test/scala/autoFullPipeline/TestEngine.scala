package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.WriteVcdAnnotation
import scala.reflect.io.File
import scala.reflect.runtime.universe._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import darecreek.exu.vfu.perm._
import darecreek.exu.vfu.fp._
import darecreek.exu.vfu.div._
import darecreek.exu.vfu.vmask._
import darecreek.exu.vfu.reduction._
import chipsalliance.rocketchip.config.Parameters

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
    val MAX_PARA_INSTS = 5
    val curTestCasePool : Seq[TestCase] = Seq()

    override def run(tbs: Seq[TestBehavior]) = {
        // println("ALUTestEngine run() not implemented")

        // TODO 1. run tests for normal mode instructions
        //  TODO MAX parallel instruction limit
        //  TODO randomly chosen between normal mode instructions
        //  TODO record done when all inputs of one instruction are fed

        // TODO 1.1. Fill insts (TestCases from TestBehavior)
        if (this.curTestCasePool.length < MAX_PARA_INSTS) {

        }

        // TODO 1.2. Randomly choose one among TestCases, get next uop from it and feed it to the dut

        // TODO 1.3. check for potential results, give it to corresponding TestBehavior and get the result
        //  TODO 1.3.1. 

        // TODO 2. run tests for ordered instructions

    }
}

abstract class TestEngine extends BundleGenHelper {
    var normalModeTBs : Seq[TestBehavior] = Seq()
    var orderedTBs : Seq[TestBehavior] = Seq()
    var normalModeTBMaps : Seq[Map[Int,Map[String, String]]] = Seq()
    var orderedTBMaps : Seq[Map[Int,Map[String, String]]] = Seq()


    
    def run(tbs: Seq[TestBehavior]) = {println("TestEngine run() not implemented")}
    def fillTBs(testBehaviors : Seq[TestBehavior]) = {
        for (i <- 0 until testBehaviors.length) {
            if (!testBehaviors(i).isOrdered) {
                this.normalModeTBs :+= testBehaviors(i)
            } else {
                this.orderedTBMaps :+= testBehaviors(i)
            }
        }

        this._readFilesToMaps(this.normalModeTBs, this.normalModeTBMaps)
        this._readFilesToMaps(this.orderedTBs, this.orderedTBMaps)
    }
}