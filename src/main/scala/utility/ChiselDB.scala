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
import chisel3.experimental.StringParam
import chisel3.util._

trait HasTableUtils {
  case class RefPort(ref: String, width: Int)

  case class Column(field: String, refPort: RefPort, vExpr: String)

  def get_columns(in: Data, prefix: String): List[Column] = {
    in match {
      case rc: Record =>
        rc.elements
          .flatMap({
            case (s, d) =>
              val newPrefix = if (prefix == "") s else prefix + "_" + s
              get_columns(d, newPrefix)
          })
          .toList
      case vec: Vec[_] =>
        vec.zipWithIndex
          .flatMap({
            case (elm, i) =>
              val newPrefix = prefix + "_" + i
              get_columns(elm, newPrefix)
          })
          .toList
      case elm: Element =>
        require(elm.widthKnown, s"width of ${elm.name} is unknown!")
        val ui = elm.asUInt

        /*
          if ui is larger than 64-bit, we need to split it
          eg: io.data.a <=> UInt(256.W)
              split it into 4 columns:
              Column(a_0, io_data_a, 256, io_data_a[0, 63])
              Column(a_1, io_data_a, 256, io_data_a[64, 127])
              Column(a_2, io_data_a, 256, io_data_a[128, 191])
              Column(a_3, io_data_a, 256, io_data_a[192, 255])
         */
        def split_ui(w: Int, begin: Int = 0): Seq[Range] = w match {
          case _ if w <= 64 =>
            Seq(Range(begin, begin + w))
          case _ if w > 64 =>
            Range(begin, begin + 64) +: split_ui(w - 64, begin + 64)
        }

        val ranges = split_ui(ui.getWidth)
        val refPort = RefPort("data_" + prefix, ui.getWidth)
        if (ranges.size == 1) {
          List(Column(prefix, refPort, refPort.ref))
        } else {
          ranges.zipWithIndex
            .map({
              case (r, i) =>
                Column(prefix + "_" + i, refPort, s"${refPort.ref}[${r.end - 1} : ${r.start}]")
            })
            .toList
        }
    }
  }
}

class Table[T <: Record](val envInFPGA: Boolean, val tableName: String, val hw: T) extends HasTableUtils {

  def genCpp: (String, String) = {
    val cols = get_columns(hw, "").map(_.field)
    val init =
      s"""
         |void init_db_$tableName() {
         |  // create table
         |  if (!enable_dump_$tableName) return;
         |
         |  const char *sql = "CREATE TABLE $tableName(" \\
         |    "ID INTEGER PRIMARY KEY AUTOINCREMENT," \\
         |    ${cols.map(c => "\"" + c.toUpperCase + " INT NOT NULL,\" \\").mkString("", "\n    ", "")}
         |    "STAMP INT NOT NULL," \\
         |    "SITE TEXT);";
         |  rc = sqlite3_exec(mem_db, sql, callback, 0, &zErrMsg);
         |  if(rc != SQLITE_OK) {
         |    printf("SQL error: %s\\n", zErrMsg);
         |    exit(0);
         |  } else {
         |    printf("%s table created successfully!\\n", "$tableName");
         |  }
         |}
         |""".stripMargin
    val insert =
      s"""
         |extern "C" void ${tableName}_write(
         |  ${cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  uint64_t stamp,
         |  char *site
         |) {
         |  if(!dump || !enable_dump_$tableName) return;
         |
         |  const char *format = "INSERT INTO $tableName(${cols.map(_.toUpperCase).mkString(",")}, STAMP, SITE) " \\
         |                  "VALUES(${cols.map(_ => "%#lx").mkString(", ")}, %#lx, '%s');";
         |  char *sql = (char *)malloc(${(cols.size + 1) * 2} * sizeof(uint64_t) + (strlen(format)+strlen(site)) * sizeof(char));
         |  sprintf(sql,
         |    format,
         |    ${cols.mkString(",")}, stamp, site
         |  );
         |  rc = sqlite3_exec(mem_db, sql, callback, 0, &zErrMsg);
         |  free(sql);
         |  if(rc != SQLITE_OK) {
         |    printf("SQL error: %s\\n", zErrMsg);
         |    exit(0);
         |  };
         |}
         |""".stripMargin
    val init_dummy =
      s"""
         |void init_db_$tableName() {}
         |""".stripMargin
    val insert_dummy =
      s"""
         |extern "C" void ${tableName}_write(
         |  ${cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  uint64_t stamp,
         |  char *site
         |) {}
         |""".stripMargin
    if (envInFPGA) (init_dummy, insert_dummy) else (init, insert)
  }

  def log(data: T, en: Bool, site: String = "", clock: Clock, reset: Reset): Unit = {
    if(!envInFPGA){
      val writer = Module(new TableWriteHelper[T](tableName, hw, site))
      val cnt = RegInit(0.U(64.W))
      cnt := cnt + 1.U
      writer.io.clock := clock
      writer.io.reset := reset
      writer.io.en := en
      writer.io.stamp := cnt
      writer.io.data := data
    }
  }

  def log(data: Valid[T], site: String, clock: Clock, reset: Reset): Unit = {
    log(data.bits, data.valid, site, clock, reset)
  }

  def log(data: DecoupledIO[T], site: String, clock: Clock, reset: Reset): Unit = {
    log(data.bits, data.fire, site, clock, reset)
  }

}

private class TableWriteHelper[T <: Record](tableName: String, hw: T, site: String)
    extends BlackBox(
      Map(
        "site" -> StringParam(site)
      )
    )
    with HasBlackBoxInline
    with HasTableUtils {
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val en = Input(Bool())
    val stamp = Input(UInt(64.W))
    val data = Input(hw.cloneType)
  })

  val moduleName = s"${tableName}Writer"
  val dpicFunc = s"${tableName}_write"

  // construct dpic api and verilog module ports
  val table = get_columns(io.data, prefix = "")
  val dpicInputs = table.map(_.field)
  val verilogModulePorts = table.map(_.refPort).distinct.map(r => s"input [${r.width - 1}:0] ${r.ref}")

  val verilog =
    s"""
       |import "DPI-C" function void $dpicFunc
       |(
       |${dpicInputs.map(x => "input longint " + x).mkString("  ", ",\n  ", ",")}
       |  input longint stamp,
       |  input string site
       |);
       |
       |module $moduleName(
       |  input clock,
       |  input reset,
       |  input en,
       |${verilogModulePorts.mkString("  ", ",\n  ", ",")}
       |  input [63:0] stamp
       |);
       |  parameter string site = "undefined";
       |
       |  always@(posedge clock) begin
       |    if(en && !reset) begin
       |      $dpicFunc(${table.map(_.vExpr).mkString("", ", ", ", stamp, site")});
       |    end
       |  end
       |endmodule
       |""".stripMargin

  setInline(s"$moduleName.v", verilog)

  override def desiredName: String = moduleName

}

object ChiselDB {

  private val table_map = scala.collection.mutable.Map[String, Table[_]]()
  private var enable = false

  def init(enable: Boolean): Unit = {
    // Not needed at the moment
    this.enable = enable
  }

  def createTable[T <: Record](tableName: String, hw: T, basicDB: Boolean = false): Table[T] = {
    getTable(tableName, hw)
      .getOrElse({
        val t = new Table[T](!(basicDB & this.enable), tableName, hw)
        table_map += (tableName -> t)
        t
      })
  }

  def getTable(tableName: String): Option[Table[_]] = {
    table_map.get(tableName)
  }

  def getTable[T <: Record](tableName: String, hw: T): Option[Table[T]] = {
    table_map
      .get(tableName)
      .map(old => {
        require(old.hw.getClass.equals(hw.getClass), s"table name conflict: $tableName" +
          s" with different hw types of ${old.hw.getClass} and ${hw.getClass}")
        old.asInstanceOf[Table[T]]
      })
  }

  def getCHeader: String = {
    """
      |#ifndef __CHISEL_DB_H__
      |#define __CHISEL_DB_H__
      |
      |#include <cstdio>
      |#include <cstring>
      |#include <cstdlib>
      |#include <cassert>
      |#include <cstdint>
      |#include <cerrno>
      |#include <unistd.h>
      |#include <sqlite3.h>
      |
      |void init_db(bool en, bool select_enable, const char *select_db);
      |void save_db(const char *filename);
      |
      |#endif
      |""".stripMargin
  }

  def getCpp: String = {

    val save =
      s"""
         |void save_db(const char *zFilename) {
         |  printf("saving memdb to %s ...\\n", zFilename);
         |  sqlite3 *disk_db;
         |  sqlite3_backup *pBackup;
         |  rc = sqlite3_open(zFilename, &disk_db);
         |  if(rc == SQLITE_OK){
         |    pBackup = sqlite3_backup_init(disk_db, "main", mem_db, "main");
         |    if(pBackup){
         |      (void)sqlite3_backup_step(pBackup, -1);
         |      (void)sqlite3_backup_finish(pBackup);
         |    }
         |    rc = sqlite3_errcode(disk_db);
         |  }
         |  sqlite3_close(disk_db);
         |}
         |""".stripMargin

    val (tables_init, tables_insert) = table_map.values.map(_.genCpp).unzip

    def select_enable_assign(str: String) =
      s"""
         |    {
         |      const char *table_name = "$str";
         |      for (int idx = 0; idx < select_db_num; idx++) {
         |        char *str_p = select_db_list[idx];
         |        int s_idx = 0;
         |        bool match = true;
         |        for (; (str_p[s_idx] != '\\0') && (table_name[s_idx] != '\\0'); s_idx ++) {
         |          if (str_p[s_idx] != table_name[s_idx]) {
         |            match = false;
         |            break;
         |          }
         |        }
         |        if (!match || (str_p[s_idx] != '\\0'))  continue;
         |
         |        enable_dump_$str = true;
         |        break;
         |      }
         |    }
         |""".stripMargin

    s"""
       |#include"chisel_db.h"
       |#include <string.h>
       |#include <stdbool.h>
       |
       |bool dump;
       |sqlite3 *mem_db;
       |char *zErrMsg;
       |int rc;
       |
       |${table_map.keys.map(t => "bool enable_dump_" + t + " = true;\n").mkString("","\n","\n")}
       |
       |static int callback(void *NotUsed, int argc, char **argv, char **azColName){
       |  return 0;
       |}
       |
       |${tables_init.mkString("  ", "\n  ", "\n")}
       |${tables_insert.mkString("  ", "\n  ", "\n")}
       |
       |void init_db(bool en, bool select_enable, const char *select_db){
       |  dump = en;
       |  if(!en) return;
       |  rc = sqlite3_open(":memory:", &mem_db);
       |  if(rc) {
       |    printf("Can't open database: %s\\n", sqlite3_errmsg(mem_db));
       |    exit(0);
       |  } else {
       |    printf("Open database successfully\\n");
       |  }
       |
       |  if (select_enable) {
       |    char *select_db_list[256];
       |    int select_db_num = 0;
       |
       |    const char *split_word = " ";
       |    char select_db_copy[1024];
       |    select_db_copy[0] = '\\0';
       |    strncpy(select_db_copy, select_db, 1024);
       |    select_db_copy[1023] = '\\0';
       |    char *str_p = strtok(select_db_copy, split_word);
       |    while (str_p) {
       |      select_db_list[select_db_num] = str_p;
       |      select_db_num ++;
       |      str_p = strtok(NULL, split_word);
       |    }
       |${table_map.keys.map(t => "    enable_dump_" + t + " = false;").mkString("\n")}
       |${table_map.keys.map(t => select_enable_assign(t)).mkString("\n")}
       |  }
       |
       |${table_map.keys.map(t => "  init_db_" + t + "();").mkString("\n")}
       |
       |}
       |$save
       |
       |""".stripMargin
  }
  def addToFileRegisters = {
    FileRegisters.add("chisel_db.h", getCHeader)
    FileRegisters.add("chisel_db.cpp", getCpp)
  }

}
