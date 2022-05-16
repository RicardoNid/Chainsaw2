package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

/** transform with repetition and reuse
 *
 */
case class transformMesh(transform: TransformConfig, repetition: Repetition) {

  def ⊗(factor: Int, step: Int = -1) = transformMesh(transform, repetition.⊗(factor, step))

  def ∏(factor:Int) = transformMesh(transform, repetition.∏(factor))


}
