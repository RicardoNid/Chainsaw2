package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

/** transform with repetition and reuse
 *
 */
case class transformMesh(transform: TransformConfig, repetition: Repetition) {

  def ⊗(parallel: Int, step: Int = -1) = transformMesh(transform, repetition.⊗(parallel, step))

  def ^(iterative: Int) = transformMesh(transform, repetition.^(iterative))


}
