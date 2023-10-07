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

package xiangshan

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
// import xiangshan.backend.execute.exublock.ExuParameters
// import xiangshan.backend.dispatch.DispatchParameters
// import xiangshan.cache.DCacheParameters
// import xiangshan.cache.prefetch._
// import xiangshan.frontend.{BasePredictor, BranchPredictionResp, FTB, FakePredictor, RAS, Tage, ITTage, Tage_SC, FauFTB}
// import xiangshan.frontend.icache.ICacheParameters
// import xiangshan.cache.mmu.{L2TLBParameters, TLBParameters}
// import freechips.rocketchip.diplomacy.AddressSet
// import system.SoCParamsKey
// import huancun._
// import coupledL2._
// import huancun.debug._
// import xiangshan.mem.prefetch.{PrefetcherParams, SMSParams}

// import scala.math.{min, max}
// import xiangshan.vector.VectorParameters

// case object XSTileKey extends Field[Seq[XSCoreParameters]]

case object XSCoreParamsKey extends Field[XSCoreParameters]

case class XSCoreParameters
(
  
  RobSize: Int = 256,
  
)