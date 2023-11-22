
package darecreek.vfuAutotest.fullPipeline

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{FunSuite, ParallelTestExecution}
import chisel3._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chiseltest.WriteVcdAnnotation
import scala.reflect.runtime.universe._
import scala.collection.mutable.Map

import scala.io.Source
import java.io.FileWriter
import java.time.{LocalDate, LocalDateTime}
import java.nio.file.{Paths, Files}
import java.time.format.DateTimeFormatter
import java.io._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._
import ExecutionContext.Implicits.global
import scala.collection.mutable.Map
import scala.collection.convert.decorateAsScala._
import java.util.concurrent.ConcurrentHashMap

object ReadTxt {
  //parsing a txt file
  def readFromTxtByline(file:String) = {
    import scala.io.Source
    val source = Source.fromFile(file,"UTF-8")
    val lines = source.getLines().toArray
    source.close()
    lines
  }

  def hasVstart(lines : Array[String]) : Boolean = {
    var i : Int = 0
    for (line <- lines) {
      if (i >= 15) return false
      if (line.contains("VSTART")) {
        return true
      }
      i += 1
    }
    return false
  }

  def getEachInputNLines(lines : Array[String]) : Int = {
    val INVALID = -1
    var i : Int = 0
    for (line <- lines) {
      if (i >= 30) return INVALID
      if (line.contains("------")) {
        return i + 1
      }
      i += 1
    }
    return INVALID
  }

  def getNEachAssignedLines(n_lines : Int, self_idx : Int, n : Int, 
                      each_input_n_lines : Int) : Int = {
    var n_inputs = n_lines / each_input_n_lines
    var each_n_inputs = n_inputs / n
    if(n_inputs % n > 0) {
      each_n_inputs += 1
    }
    return each_n_inputs * each_input_n_lines
  }

  def KeyFileUtil(array:Array[String]) = {
    var keyMapList = Map[String, String]()
    var keyMapList2 = Seq[Map[String,String]]()
    // var number = 0
    for (i <- 0 until array.length) {
      val lineArray = array(i).trim.split("=")
      if(lineArray.size==2){
        keyMapList += (lineArray(0).trim -> lineArray(1))
      }

      if(lineArray.size != 2 || lineArray(0).equals("--------------------------test_cnt")) {
        keyMapList2 :+= keyMapList
        keyMapList = Map[String, String]()
      }
      // keyMapList2 = keyMapList2 ++ Map(number -> keyMapList)
      /*if(lineArray(0).equals("--------------------------test_cnt")) {
        number = number + 1
      }*/
    }
    keyMapList2
  }
}

trait VAluBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  def testMain(
    testEngine : TestEngine, j:Int = -1, 
    printFunc : () => Unit = () => {},
    vcdFileSuffix : String = ""
  ): Unit = {
    var testName = "Tests on " + testEngine.getName()
    if (j != -1) testName += s" datasplit $j"
    testName += vcdFileSuffix
    it should s"$testName" in {
      test(testEngine.getDut()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        testEngine.test_init(dut)
        println(s"Starting tests for ${testEngine.getName()}")
        testEngine.run(dut)
      }

      printFunc()
    }
  }

}

object Logger {
  def printvds(dut_vd : String, expected_vd : String) : Unit = {
    println("dut:\t\t" + dut_vd + "\nexpected: \t" + expected_vd)
  }
}

object Dump {
  val DUMP = true // switch

  val name = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd_kk_mm"))
  // var fileDire = "./src/test/scala/log/"
  var fileDire = "./src/test/scala/autoFullPipeline/IncorrectInputs/"
  var fileName = fileDire + s"${name}.txt"
  var incorrectDataFilePath = fileDire + s"${name}_data"
  Files.createDirectories(Paths.get(incorrectDataFilePath));
  var incorrectInstFilename = ""
  var doneInstFilename = ""

  def updateIncorrDataDire(dire : String) = {
    fileDire = dire
    fileName = fileDire + s"${name}.txt"
    incorrectDataFilePath = fileDire + s"${name}_data"
    Files.createDirectories(Paths.get(incorrectDataFilePath));
  }

  def hasInst(file: String, instid: String): Boolean = {
    val source = Source.fromFile(file)
    try {
      source.getLines().exists(_.trim == instid)
    } finally {
      source.close()
    }
  }

  def dump(map : Map[String,String], instid : String, dut_out : String = "", golden_vd : String = "", any_str : String = "", fault_wb : String = "") {
    if (!DUMP) return
    // val currentDate = LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd"))
    val currentDatetime = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd_kk_mm"))

    val file = new File(fileName)
    file.createNewFile()
    var writer = new FileWriter(file, true)
    writer.write("##############################################################################################\n")
    writer.write(s"$instid ($any_str, $currentDatetime)\n")
    writer.write("=========================================================================\n")
    map.foreach {
        case (key, value) => writer.write(s"$key=$value\n")
    }
    writer.write("=========================================================================\n")
    writer.write(s"Incorrect input:\n")
    println("fault_wb", fault_wb)
    if (!fault_wb.equals("")) {
      writer.write(s"faulty wb index: ${fault_wb}\n")
    }
    writer.write(s"dut: \t\t\t${dut_out}\n")
    writer.write(s"expected: \t\t${golden_vd}\n")
    writer.write(s"##############################################################################################\n")
    writer.close()

    val incorrectDataFile = new File(s"${incorrectDataFilePath}/${instid.replaceAll("_", ".")}.data")
    incorrectDataFile.createNewFile()
    writer = new FileWriter(incorrectDataFile, true)
    var sepkey : String = ""
    var sepValue : String = ""
    map.foreach {
        case (key, value) => {
          if (!key.contains("------")) {
            writer.write(s"$key=$value\n")
          } else {
            sepkey = key
            sepValue = value
          }
        }
    }
    if (sepkey.length != 0) {
      writer.write(s"$sepkey=$sepValue\n")
    } else {
      writer.write("---------------------------------------------------\n")
    }
    writer.close()

    recordIncorrectInst(instid)
  }

  def recordIncorrectInst(instid : String) {
    if(!incorrectInstFilename.equals("")) {
      val file1 = new File(incorrectInstFilename)
      file1.createNewFile()

      val instFailedBefore = hasInst(incorrectInstFilename, instid)

      val writer1 = new FileWriter(file1, true)
      if(!instFailedBefore) {
        writer1.write(s"$instid\n")
      }
      writer1.close()
    }
  }

  def recordDone(str : String) {
    if (!DUMP) return
    if(!doneInstFilename.equals("")) {
      val file1 = new File(doneInstFilename)
      file1.createNewFile()

      val writer1 = new FileWriter(file1, true)
      writer1.write(s"$str\n")
      writer1.close()
    }
  }
}

object Datapath {
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_23/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_29/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_30/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_1/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_4/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_6/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/10_10/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/10_13/unittest/"
  val testdataRoot = "/home/maoting/nanhu/testdata/11_22/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/debug/"
  //val testdataRoot = "C:\\kou\\XS_Vector_Unit\\src\\test\\scala\\unittest\\"
  // val testdataRoot = "/home/kou/unittest/"
  // val testdataRoot = "/home/maoting/xs-unittest/testdata/"
}


class VAluSpec extends AnyFlatSpec with ChiselScalatestTester
  with BundleGenHelper with VAluBehavior {
  behavior of "Test"

  println(s"===========\nTEST STARTED\n============")
  if(Dump.DUMP) println(s"DUMP is turned on. Incorrect inputs will be saved to ${Dump.fileName}")

  var tbs : Seq[TestBehavior] = Seq(

    // new VredsumvsTestBehavior,

    // new VfmulvvTestBehavior,
    // new VfmaccvvTestBehavior

    // new VfncvtxfwTestBehavior,
    // new VfncvtxufwTestBehavior

    // new VnclipwvTestBehavior
    // new VzextVf2TestBehavior,
    // new Vmv2rvTestBehavior,
    // new VmvsxTestBehavior,
    // new VmvxsTestBehavior,
    // new VmseqvvTestBehavior,
    // new VmadcvvTestBehavior,
    // new VmvxsTestBehavior,

    // new VslideupvxFSMTestBehavior,
    // new VfaddvvTestBehavior,
    // new VnclipwiTestBehavior,
    // new VnclipwxTestBehavior,
    // new VaddvxTestBehavior,
    // new VaddviTestBehavior,

    // new VmadcvvTestBehavior,

    // new VaddvvTestBehavior,
    //  new Vmv8rTestBehavior,
    // new VaddvvTestBehavior,
    // new VminuvvTestBehavior,
    // new VmaxuvvTestBehavior

    // new VmadcvxmTestBehavior,

    // new VmvxsTestBehavior,
    // new VfmvfsTestBehavior,

    // new VfncvtxufwTestBehavior,

    //   // FSM ==================================
    // new VslideupvxFSMTestBehavior,
    //   new VslidedownvxFSMTestBehavior,
    //   new Vslide1upvxFSMTestBehavior,
    //   new Vslide1downvxFSMTestBehavior,
    //   new VrgathervvFSMTestBehavior,
    //   new VrgathervxFSMTestBehavior,
    //  new Vrgatherei16vvFSMTestBehavior,
    //   new VcompressvmFSMTestBehavior,
    // new VslideupviFSMTestBehavior,
    // new VslidedownviFSMTestBehavior,
    // new VrgatherviFSMTestBehavior,
    // new Vfslide1upvfFSMTestBehavior,
    // new Vfslide1downvfFSMTestBehavior,

    //   // Reduction ===============================
    //   new VredsumvsTestBehavior,
    //   new VredmaxuvsTestBehavior,
    //   new VredmaxvsTestBehavior,
    //   new VredminuvsTestBehavior,
    //   new VredminvsTestBehavior,
    //   new VredandvsTestBehavior,
    //   new VredorvsTestBehavior,
    //   new VredxorvsTestBehavior,
    //   new VwredsumuvsTestBehavior,
    //   new VwredsumvsTestBehavior,

    //   // Mask ==================================
    //   new VmsbfmTestBehavior,
    //   new VmsifmTestBehavior,
    //   new VmsofmTestBehavior,

    //   new VmandmmTestBehavior,
    //   new VmnandmmTestBehavior,
    //   new VmandnmmTestBehavior,
    //   new VmxormmTestBehavior,
    //   new VmormmTestBehavior,
    //   new VmnormmTestBehavior,
    //   new VmornmmTestBehavior,
    //   new VmxnormmTestBehavior,

    //   new VfirstmTestBehavior,

    //   new VcpopmTestBehavior,
    //   new VidvTestBehavior,
    //   new ViotamTestBehavior,
    // new VfmvsfTestBehavior,
    // new VmadcvxmTestBehavior,

    // FP ===============================
    // new VfredosumvsTestBehavior, // vfredosum.vs
    // new VfredusumvsTestBehavior, // vfredusum.vs
    // new VfredmaxvsTestBehavior, // vfredmax.vs
    // new VfredminvsTestBehavior, // vfredmin.vs

    // new VfwredosumvsTestBehavior, // vfwredosum.vs
    // new VfwredusumvsTestBehavior, // vfwredusum.vs

    // new VfncvtxufwTestBehavior, // vfncvt.xu.f.w
    // new VfncvtxfwTestBehavior, // vfncvt.x.f.w
    // new VfncvtrtzxufwTestBehavior, // vfncvt.rtz.xu.f.w
    // new VfncvtrtzxfwTestBehavior, // vfncvt.rtz.x.f.w
    // new VfncvtfxuwTestBehavior, // vfncvt.f.xu.w
    // new VfncvtfxwTestBehavior, // vfncvt.f.x.w

    // new VfncvtffwTestBehavior, // vfncvt.f.f.w
    // new VfncvtrodffwTestBehavior, // vfncvt.rod.f.f.w

    // new VfaddvvTestBehavior, // vfadd.vv
    // new VfsubvvTestBehavior, // vfsub.vv

    new VfmulvvTestBehavior, // vfmul.vv
    new VfmaccvvTestBehavior, // vfmacc.vv
    new VfnmaccvvTestBehavior, // vfnmacc.vv
    new VfmsacvvTestBehavior, // vfmsac.vv
    new VfnmsacvvTestBehavior, // vfnmsac.vv
    new VfmaddvvTestBehavior, // vfmadd.vv
    new VfnmaddvvTestBehavior, // vfnmadd.vv
    new VfmsubvvTestBehavior, // vfmsub.vv
    new VfnmsubvvTestBehavior, // vfnmsub.vv

    // new VfminvvTestBehavior, // vfmin.vv
    // new VfmaxvvTestBehavior, // vfmax.vv

    // new VmfeqvvTestBehavior, // vmfeq.vv
    // new VmfnevvTestBehavior, // vmfne.vv
    // new VmfltvvTestBehavior, // vmflt.vv
    // new VmflevvTestBehavior, // vmfle.vv

    // new VfsgnjvvTestBehavior, // vfsgnj.vv
    // new VfsgnjnvvTestBehavior, // vfsgnjn.vv
    // new VfsgnjxvvTestBehavior, // vfsgnjx.vv

    // new VfaddvfTestBehavior, // vfadd.vf
    // new VfsubvfTestBehavior, // vfsub.vf

    // new VfmulvfTestBehavior, // vfmul.vf
    // new VfmaccvfTestBehavior, // vfmacc.vf
    // new VfnmaccvfTestBehavior, // vfnmacc.vf
    // new VfmsacvfTestBehavior, // vfmsac.vf
    // new VfnmsacvfTestBehavior, // vfnmsac.vf
    // new VfmaddvfTestBehavior, // vfmadd.vf
    // new VfnmaddvfTestBehavior, // vfnmadd.vf
    // new VfmsubvfTestBehavior, // vfmsub.vf
    // new VfnmsubvfTestBehavior, // vfnmsub.vf

    // new VfminvfTestBehavior, // vfmin.vf
    // new VfmaxvfTestBehavior, // vfmax.vf

    // new VmfeqvfTestBehavior, // vmfeq.vf
    // new VmfnevfTestBehavior, // vmfne.vf
    // new VmfltvfTestBehavior, // vmflt.vf
    // new VmflevfTestBehavior, // vmfle.vf

    // new VfsgnjvfTestBehavior, // vfsgnj.vf
    // new VfsgnjnvfTestBehavior, // vfsgnjn.vf
    // new VfsgnjxvfTestBehavior, // vfsgnjx.vf

    // new VfrsubvfTestBehavior, // vfrsub.vf
    // new VmfgtvfTestBehavior, // vmfgt.vf
    // new VmfgevfTestBehavior, // vmfge.vf
    // new VfclassvTestBehavior, // vfclass.v
    // new VfmergevfmTestBehavior, // vfmerge.vfm
    // new VfmvTestBehavior, // vfmv.v.f
    // new Vfrsqrt7vTestBehavior, // vfrsqrt7.v
    // new Vfrec7vTestBehavior, // vfrec7.v


    // new VfcvtxufvTestBehavior, // vfcvt.xu.f.v
    // new VfcvtxfvTestBehavior, // vfcvt.x.f.v
    // new VfcvtrtzxufvTestBehavior, // vfcvt.rtz.xu.f.v
    // new VfcvtrtzxfvTestBehavior, // vfcvt.rtz.x.f.v
    // new VfcvtfxuvTestBehavior, // vfcvt.f.xu.v
    // new VfcvtfxvTestBehavior, // vfcvt.f.x.v

    // new VfwaddwvTestBehavior, // vfwadd.wv
    // new VfwsubwvTestBehavior, // vfwsub.wv
    // new VfwaddwfTestBehavior, // vfwadd.wf
    // new VfwsubwfTestBehavior, // vfwsub.wf

    // new VfwaddvvTestBehavior, // vfwadd.vv
    // new VfwsubvvTestBehavior, // vfwsub.vv
    // new VfwmaccvvTestBehavior, // vfwmacc.vv
    // new VfwnmaccvvTestBehavior, // vfwnmacc.vv
    // new VfwmsacvvTestBehavior, // vfwmsac.vv
    // new VfwnmsacvvTestBehavior, // vfwnmsac.vv
    // new VfwmulvvTestBehavior, // vfwmul.vv

    // new VfwaddvfTestBehavior, // vfwadd.vf
    // new VfwsubvfTestBehavior, // vfwsub.vf
    // new VfwmaccvfTestBehavior, // vfwmacc.vf
    // new VfwnmaccvfTestBehavior, // vfwnmacc.vf
    // new VfwmsacvfTestBehavior, // vfwmsac.vf
    // new VfwnmsacvfTestBehavior, // vfwnmsac.vf
    // new VfwmulvfTestBehavior, // vfwmul.vf

    // new VfwcvtffvTestBehavior, // vfwcvt.f.f.v
    // new VfwcvtxufvTestBehavior, // vfwcvt.xu.f.v
    // new VfwcvtxfvTestBehavior, // vfwcvt.x.f.v
    // new VfwcvtrtzxufvTestBehavior, // vfwcvt.rtz.xu.f.v
    // new VfwcvtrtzxfvTestBehavior, // vfwcvt.rtz.x.f.v
    // new VfwcvtfxuvTestBehavior, // vfwcvt.f.xu.v
    // new VfwcvtfxvTestBehavior, // vfwcvt.f.x.v

    // Div ========================================
    // new VdivvvTestBehavior,
    // new VdivuvvTestBehavior,
    // new VdivvxTestBehavior,
    // new VdivuvxTestBehavior,

    // new VremvvTestBehavior,
    // new VremuvvTestBehavior,
    // new VremvxTestBehavior,
    // new VremuvxTestBehavior,

    // new VfdivvvTestBehavior,
    // new VfdivvfTestBehavior,

    // new VfrdivvfTestBehavior,

    // new VfsqrtvTestBehavior,
  )

  var testInsts : Array[String] = Array()

  val param = sys.props.getOrElse("insfile", "")
  val incorrectInstFile = sys.props.getOrElse("incorrInsts", "") // failed instruction names
  val doneInstFile = sys.props.getOrElse("doneInsts", "") // successfully passed instruction names
  val incorrectInputFile = sys.props.getOrElse("incorrInput", "") // incorrect inputs in one file
  val incorrectDataDireFile = sys.props.getOrElse("incorrDataDire", "") // for building data file directly usable

  if(!incorrectInputFile.equals("")) Dump.fileName = incorrectInputFile
  if(!incorrectInstFile.equals("")) Dump.incorrectInstFilename = incorrectInstFile
  if(!doneInstFile.equals("")) Dump.doneInstFilename = doneInstFile
  if(!incorrectDataDireFile.equals("")) Dump.updateIncorrDataDire(incorrectDataDireFile)

  // ===================== data split ======================================
  val dataSplitIx = sys.props.getOrElse("dataSplitIx", "0").toInt
  val dataSplitN = sys.props.getOrElse("dataSplitN", "1").toInt
  val dataSplitInst = sys.props.getOrElse("dataSplitInst", "")
  // =======================================================================

  val dataSplitMode : Boolean = !dataSplitInst.equals("")

  if (!dataSplitMode) {
    // normal, no data splitting
    if (param.equals("")) {
      println("WARNING: No file specified.. Using TestBehaviors specified in VIntFixpAluSpec.scala")
      for (tb <- tbs) {
        testInsts = testInsts :+ tb.getInstid()
      }
    } else {
      tbs = Seq()
      println("Loading instructions from file: ", param)
      testInsts = ReadTxt.readFromTxtByline(param).distinct
      for (inst <- testInsts) {
        if (!inst.equals("")) {
          println(s".. Adding $inst")
          tbs = tbs :+ TBMap.tbMap(inst)()
        }
      }
    }
  } else {
    // single inst, data splitting
    tbs = Seq(TBMap.tbMap(dataSplitInst)())
    println(s"Testing $dataSplitInst with Data Split .. $dataSplitIx / $dataSplitN")
  }

  var dataN = 1
  var j = 0
  if (dataSplitMode) {
    dataN = dataSplitN
    j = dataSplitIx
  }

  var testEngineToTB : Array[Seq[TestBehavior]] = Array(Seq(),Seq(),Seq(),Seq(),Seq(),Seq(),Seq())
  for(i <- 0 until tbs.length) {
    val testEngineId = tbs(i).getTargetTestEngine()
    testEngineToTB(testEngineId) :+= tbs(i)
  }

  var testEngines : Seq[TestEngine] = Seq()
  for(testEngineId <- 0 until testEngineToTB.length) {
    if (testEngineToTB(testEngineId).length != 0) {
      val testEngine = TestEngine.getEngine(testEngineId)
      testEngine.fillTBs(testEngineToTB(testEngineId))
      testEngines :+= testEngine
    }
  }

  val printRes : (() => Unit) = () => { 
    println("============================== TEST RESULT ==================================")
    for(tb <- tbs) {
      var result = "\u001b[32mSUCCESS\u001b[0m"
      if (!tb.testResult) result = "\u001b[31mFAILED\u001b[0m"
      println(s"${tb.getInstid()} \t ${result}")
    }
    println("============================== TEST RESULT ==================================")
  }

  val currentDateTime: String = 
    LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy_MM_dd_HH_mm_ss"))  // Get the current date and time
  // val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")  // Define the desired format
  // val dateTimeString: String = currentDateTime.format(formatter)

  println(s"NOTICE vcds will have suffix: ${currentDateTime}")

  for(i <- 0 until testEngines.length) {
    val testEngine = testEngines(i)
    var printFunc : () => Unit = () => {}
    if (i == testEngines.length - 1) {
      printFunc = printRes
    }
    it should behave like testMain(testEngine, j, printFunc, currentDateTime)
  }

}

