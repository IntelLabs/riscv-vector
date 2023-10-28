
package darecreek.vfuAutotest.alu

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{FunSuite, ParallelTestExecution}
import chisel3._
import darecreek.exu.vfu._
import darecreek.exu.vfu.alu._
import darecreek.exu.vfu.VInstructions._
import chiseltest.WriteVcdAnnotation
import scala.reflect.runtime.universe._
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
import scala.collection.concurrent.{Map => ConCurMap}
import scala.collection.mutable.ListBuffer
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
    var keyMapList2 = Map[Int,Map[String,String]]()
    var number = 0
    for (i <- 0 until array.length) {
      val lineArray = array(i).trim.split("=")
      if(lineArray.size==2){
        keyMapList = keyMapList ++ Map(lineArray(0).trim -> lineArray(1))
      }else{
        number = number + 1 
      }
      keyMapList2 = keyMapList2 ++ Map(number -> keyMapList)
      if(lineArray(0).equals("--------------------------test_cnt")) {
        number = number + 1
      }
    }
    keyMapList2
  }
}

trait VAluBehavior {
  this: AnyFlatSpec with ChiselScalatestTester with BundleGenHelper =>

  def vIntTestAll(sim:Map[Int,Map[String,String]],ctrl:TestCtrlBundleBase,s:String, tb:TestBehavior, j:Int = -1): Unit = {
    var testName = "pass the test: " + tb.getInstid() + " lmul ls 1"
    if (j != -1) testName += s" datasplit $j"
    it should s"$testName" in {
      test(tb.getDut()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        tb.test_init(dut)
        val nameList=sim.map(_._1)
        println(s"Starting test for ${tb.getInstid()}, lmul <= 1")
        println("test input counts",nameList.max+1)
        var i = 0
        do{
          val vflmul = sim(i).get("vflmul").get
          if((vflmul != "2.000000") && (vflmul != "4.000000") && (vflmul) != "8.000000"){
            println("lmul <= 1, id: ", i)
            tb.testSingle(sim(i), ctrl, s, dut)
          }
          i = i + 1
        }while(i <= nameList.max)

        println(s"${tb.getInstid()}, lmul <= 1 tests are done.")
        tb.setLmulLsOneDone()
        Dump.recordDone(s"${tb.getInstid()}, lmul <= 1")
      }
    }
  }

  def vsalu(sim:Map[Int,Map[String,String]], ctrl:TestCtrlBundleBase, s:String, tb:TestBehavior, j:Int = -1): Unit = {
    var testName = "pass the test: " + tb.getInstid() + " lmul gt 1"
    if (j != -1) testName += s" datasplit $j"
    it should s"$testName" in {
      test(tb.getDut()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        if (tb.getLmulLsOneDone()) {
          // assume lmul <= 1 tests are done before
          tb.test_init(dut)
        
          val nameList=sim.map(_._1)
          println(s"Starting test for ${tb.getInstid()}, lmul > 1")
          println("test counts:",nameList.max)
          for(i <- 0 until nameList.max+1){
            val vflmul = sim(i).get("vflmul").get
            if((vflmul == "2.000000") || (vflmul == "4.000000") || (vflmul) == "8.000000"){
              println("lmul > 1, id: ", i)
              tb.testMultiple(sim(i), ctrl, s, dut)
            }
          }
          println(s"${tb.getInstid()}, lmul > 1 tests are done.")
          Dump.recordDone(s"${tb.getInstid()}, lmul > 1")
        } else {
          println(s"lmulLsOneDone has not been set. Is lmul <= 1 tests failed for ${tb.getInstid()}?")
        }
      }
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
  var fileDire = "./src/test/scala/auto/IncorrectInputs/"
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

/*object TestResults {

  case class InstTestRes(
    inst : String,
    failed : Boolean = false,
    fault_dut_out : String = "",
    fault_golden_out : String = "",
    fault_fsm_wb_idx : String = "",
  )

  var results: ConCurMap[String, InstTestRes] = new ConcurrentHashMap().asScala

  def initResults(insts : Array[String]) : Unit = {
    for(inst <- insts) {
      results(inst) = InstTestRes(inst)
    }
  }

  def addResult(testRes : InstTestRes) : Unit = {
    results(testRes.inst) = testRes
  }

  def checkResult(inst : String) : Boolean = {
    return results(inst).failed
  }

  def print() : Unit = {
    println("TestResults: ")
    results.foreach {
        case (key, value) => println(s"$key=${value.failed}, ${value.fault_dut_out}")
    }
  }
}*/

object Datapath {
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_23/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_29/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/8_30/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_1/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_4/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/9_6/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/10_10/unittest/"
  // val testdataRoot = "/home/maoting/nanhu/testdata/10_13/unittest/"
  val testdataRoot = "/home/maoting/nanhu/testdata/debug/"
  //val testdataRoot = "C:\\kou\\XS_Vector_Unit\\src\\test\\scala\\unittest\\"
  // val testdataRoot = "/home/kou/unittest/"
  // val testdataRoot = "/home/maoting/xs-unittest/testdata/"
}


class VAluSpec extends AnyFlatSpec with ChiselScalatestTester
  with BundleGenHelper with VAluBehavior {
  behavior of "Int fixP test"

  println(s"============\nTEST STARTED\n============")
  if(Dump.DUMP) println(s"DUMP is turned on. Incorrect inputs will be saved to ${Dump.fileName}")

  var tbs : Seq[TestBehavior] = Seq(

    // new VredsumvsTestBehavior,

    new VslideupvxFSMTestBehavior,
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

    // new VfmulvvTestBehavior, // vfmul.vv
    // new VfmaccvvTestBehavior, // vfmacc.vv
    // new VfnmaccvvTestBehavior, // vfnmacc.vv
    // new VfmsacvvTestBehavior, // vfmsac.vv
    // new VfnmsacvvTestBehavior, // vfnmsac.vv
    // new VfmaddvvTestBehavior, // vfmadd.vv
    // new VfnmaddvvTestBehavior, // vfnmadd.vv
    // new VfmsubvvTestBehavior, // vfmsub.vv
    // new VfnmsubvvTestBehavior, // vfnmsub.vv

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
  val incorrectInstFile = sys.props.getOrElse("incorrInsts", "")
  val doneInstFile = sys.props.getOrElse("doneInsts", "")
  val incorrectInputFile = sys.props.getOrElse("incorrInput", "")

  val incorrectDataDireFile = sys.props.getOrElse("incorrDataDire", "")

  val dataSplitIx = sys.props.getOrElse("dataSplitIx", "0").toInt
  val dataSplitN = sys.props.getOrElse("dataSplitN", "1").toInt
  val dataSplitInst = sys.props.getOrElse("dataSplitInst", "")


  if(!incorrectInputFile.equals("")) Dump.fileName = incorrectInputFile
  if(!incorrectInstFile.equals("")) Dump.incorrectInstFilename = incorrectInstFile
  if(!doneInstFile.equals("")) Dump.doneInstFilename = doneInstFile
  if(!incorrectDataDireFile.equals("")) Dump.updateIncorrDataDire(incorrectDataDireFile)

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

  // val futures = new ListBuffer[Future[Unit]]
  
  for(i <- 0 until tbs.length) {
    // params
    val tb = tbs(i)

    // test code
    // tb.changeSwitch()
    val test_file = tb.getTestfilePath()
    val inst = tb.getCtrlBundle()
    val sign = tb.getSign()

    if (Files.exists(Paths.get(test_file))) {
      val key = ReadTxt.readFromTxtByline(test_file)
      val hasvstart1 = ReadTxt.hasVstart(key)
      println(s"hasVstart $hasvstart1")
      var each_input_n_lines = ReadTxt.getEachInputNLines(key)
      println("each_input_n_lines", each_input_n_lines)

      var dataN = 1
      var j = 0
      if (dataSplitMode) {
        dataN = dataSplitN
        j = dataSplitIx
      }

      val each_asisgned_lines = ReadTxt.getNEachAssignedLines(key.length, j, dataN, each_input_n_lines)
      val startingIndex = j * each_asisgned_lines
      if (startingIndex < key.length) {
        println(s"Data Split $j / $dataN: $startingIndex + $each_asisgned_lines, total ${key.length}")
        // val testFuture : Future[Unit] = Future 
        
        val keymap = ReadTxt.KeyFileUtil(key.slice(startingIndex, startingIndex + each_asisgned_lines))
        // println(s"$j Future is looking at ${tb.getInstid()}, $startingIndex + $each_asisgned_lines")
        var reportIx = -1
        if (dataSplitMode) {
          reportIx = j
        }
        it should behave like vIntTestAll(keymap, inst, sign, tb, reportIx)
        it should behave like vsalu(keymap, inst, sign, tb, reportIx)
        // println("wtf??????????")
      }
    } else {
      println(s"Data file does not exist for instruction: ${tb.getInstid()} , skipping")
      Dump.recordIncorrectInst(tb.getInstid())
    }
  }
}

