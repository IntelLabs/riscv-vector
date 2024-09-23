/***************************************************************************************
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import chisel3.util.HasBlackBoxInline

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  val verilog =
    """
      |module ClockGate (
      |  input  wire TE,
      |  input  wire E,
      |  input  wire CK,
      |  output wire Q
      |);
      |
      |  wire clk_en;
      |  reg  clk_en_reg;
      |
      |  assign clk_en = E | TE;
      |
      |`ifndef VERILATOR_LEGACY
      |  always @(CK or clk_en) begin
      |    if (CK == 1'b0)
      |      clk_en_reg <= clk_en;
      |  end
      |`else
      |  always @(posedge CK) begin
      |    clk_en_reg = clk_en;
      |  end
      |`endif // VERILATOR_LEGACY
      |
      |  assign Q = CK & clk_en_reg;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("ClockGate.v", verilog)
}

object ClockGate {
  def apply(TE: Bool, E: Bool, CK: Clock) : Clock = {
    val clock_gate = Module(new ClockGate).io
    clock_gate.TE := TE
    clock_gate.E  := E
    clock_gate.CK := CK
    clock_gate.Q
  }
}
