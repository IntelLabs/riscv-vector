/** ************************************************************************************* Copyright (c) 2020-2021
  * Institute of Computing Technology, Chinese Academy of Sciences Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2. You can use this software according to the terms and conditions of the
  * Mulan PSL v2. You may obtain a copy of Mulan PSL v2 at: http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
  * BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  */

package coincreekDCache.util

import firrtl.annotations.{Annotation, ModuleName, Named, SingleTargetAnnotation}
import chisel3._
import chisel3.experimental.ChiselAnnotation

case class SRAMClkDivBy2Annotation(mod: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  override val target: ModuleName = mod

  override def duplicate(n: ModuleName): Annotation = this.copy(n)
}

case class SRAMSpecialDepthAnnotation(mod: ModuleName) extends SingleTargetAnnotation[ModuleName] {
  override val target: ModuleName = mod

  override def duplicate(n: ModuleName): Annotation = this.copy(n)
}

object CustomAnnotations {

  def annotateClkDivBy2(mod: Module) =
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = SRAMClkDivBy2Annotation(mod.toNamed)
    })

  def annotateSpecialDepth(mod: Module) =
    chisel3.experimental.annotate(new ChiselAnnotation {
      override def toFirrtl: Annotation = SRAMSpecialDepthAnnotation(mod.toNamed)
    })

}
