/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package utility

import chisel3._
import chisel3.util._

trait ConstantinParams {
  def UIntWidth = 64
  def AutoSolving = false
  def getdpicFunc(constName: String) = {
    s"${constName}_constantin_read"
  }
  def getModuleName(constName: String) = {
    s"${constName}_constantinReader"
  }
}

private class SignalReadHelper(constName: String, initValue: BigInt)
  extends BlackBox with HasBlackBoxInline with ConstantinParams {
  val io = IO(new Bundle{
    val value = Output(UInt(UIntWidth.W))
  })

  val moduleName = getModuleName(constName)
  val dpicFunc = getdpicFunc(constName)

  val verilog =
    s"""
       |`ifndef SYNTHESIS
       |import "DPI-C" function longint $dpicFunc();
       |`endif
       |
       |module $moduleName(
       |  output reg [$UIntWidth - 1:0] value
       |);
       |
       |`ifdef SYNTHESIS
       |  initial value = $initValue;
       |`else
       |  initial value = $dpicFunc();
       |`endif
       |endmodule
       |""".stripMargin
  setInline(s"$moduleName.v", verilog)
  override def desiredName: String = moduleName
}

class MuxModule[A <: Record](gen: A, n: Int) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Vec(n, gen))
    val sel = Input(UInt(log2Ceil(n).W))
    val out = gen
  })
  io.in.foreach(t => t <> DontCare)
  io.out <> io.in(0)
  io.in.zipWithIndex.map{case (t, i) => when (io.sel === i.U) {io.out <> t}}
}

/*
* File format: constName expected_runtimevalue (unsigned DEC)
* */

object Constantin extends ConstantinParams {
  // store init value => BigInt
  private val initMap = scala.collection.mutable.Map[String, BigInt]()
  private val objectName = "constantin"
  private var enable = true

  def init(enable: Boolean): Unit = {
    this.enable = enable
  }

  def createRecord(constName: String, initValue: Boolean): Bool = {
    createRecord(constName, if (initValue) 1 else 0)(0)
  }

  def createRecord(constName: String, initValue: BigInt = 0): UInt = {
    initMap(constName) = initValue

    if (!this.enable) {
      println(s"Constantin initRead: $constName = $initValue")
      initValue.U(UIntWidth.W)
    } else {
      println(s"Constantin fileRead: $constName = $initValue")
      Module(new SignalReadHelper(constName, initValue)).suggestName(s"recordModule_$constName").io.value
    }
  }

  def getCHeader: String = {
    s"""
       |#ifndef CONSTANTIN_H
       |#define CONSTANTIN_H
       |
       |#endif // CONSTANTIN_H
       |""".stripMargin
  }

  def getInitCpp: String = {
    val initStr = initMap.map({ a => s"""  constantinMap["${a._1}"] = ${a._2};\n""" }).foldLeft("")(_ + _)
    s"""
       |#include <map>
       |#include <string>
       |#include <stdint.h>
       |using namespace std;
       |
       |map<string, uint64_t> constantinMap;
       |
       |void constantinInit(){
       |${initStr}
       |}
       |""".stripMargin
  }

  def getPreProcessCpp: String = {
    s"""
       |#include <iostream>
       |#include <fstream>
       |#include <map>
       |#include <string>
       |#include <cstdlib>
       |#include <stdint.h>
       |using namespace std;
       |
       |fstream cf;
       |extern map<string, uint64_t> constantinMap;
       |
       |void constantinLoad() {
       |  constantinInit();
       |  uint64_t num;
       |  const char *noop_home = getenv("NOOP_HOME");
       |#ifdef NOOP_HOME
       |  if (!noop_home) {
       |    noop_home = NOOP_HOME;
       |  }
       |#endif
       |  string noop_home_s = noop_home;
       |  string tmp = noop_home_s + "/build/${objectName}.txt";
       |  cf.open(tmp.c_str(), ios::in);
       |  if(cf.good()){
       |    while (cf >> tmp >> num) {
       |      constantinMap[tmp] = num;
       |    }
       |  }else{
       |    cout << "[WARNING] " << tmp << " does not exist, so all constants default to initialized values." << endl;
       |  }
       |  cf.close();
       |
       |}
       |""".stripMargin
  }

  def getPreProcessFromStdInCpp: String = {
    s"""
       |#include <iostream>
       |#include <map>
       |#include <string>
       |#include <cstdlib>
       |#include <stdint.h>
       |using namespace std;
       |
       |extern map<string, uint64_t> constantinMap;
       |
       |void constantinLoad() {
       |  constantinInit();
       |  uint64_t num;
       |  string tmp;
       |  uint64_t total_num;
       |  cout << "please input total constant number" << endl;
       |  cin >> total_num;
       |  cout << "please input each constant ([constant name] [value])" << endl;
       |  for(int i=0; i<total_num; i++) {
       |    cin >> tmp >> num;
       |    constantinMap[tmp] = num;
       |  }
       |
       |}
       |""".stripMargin
  }

  def getCpp(constName: String): String = {
    s"""
       |#include <map>
       |#include <string>
       |#include <stdint.h>
       |#include <assert.h>
       |using namespace std;
       |extern map<string, uint64_t> constantinMap;
       |extern "C" uint64_t ${getdpicFunc(constName)}() {
       |  static int a = -1;
       |  if(a != constantinMap["${constName}"]){
       |    cout << "${constName}" << " = " << constantinMap["${constName}"] << endl;
       |    a = constantinMap["${constName}"];
       |  }
       |  return constantinMap["${constName}"];
       |}
       |""".stripMargin
  }

  def getTXT: String = {
    initMap.map({a => a._1 + s" ${a._2}\n"}).foldLeft("")(_ + _)
  }

  def addToFileRegisters = {
    FileRegisters.add(s"${objectName}.hpp", getCHeader)
    var cppContext = getInitCpp
    if (AutoSolving) {
      cppContext += getPreProcessFromStdInCpp
    } else {
      cppContext += getPreProcessCpp
    }
    cppContext += initMap.map({a => getCpp(a._1)}).foldLeft("")(_ + _)
    FileRegisters.add(s"${objectName}.cpp", cppContext)
    FileRegisters.add(s"${objectName}.txt", getTXT)
  }

}
