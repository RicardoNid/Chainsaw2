package org.datenlord

import dfg.{RingDag, RingPort, RingVertex}

import spinal.core.{UInt, Vec}

abstract class TransformBase extends TransformConfig {

  // override when folding is available/natural, otherwise, just leave it here
  val spaceFold: Int = 1

  def spaceFolds: Seq[Int] = Seq(1)

  val timeFold: Int = 1

  def timeFolds: Seq[Int] = Seq(1)

  // repetition operations
  def ⊗(factor: Int, step: Int = -1) = TransformMesh(this, Repetition(Seq(SpaceRepetition(factor, step)), TimeRepetition(1)), Reuse.unit)

  def ∏(factor: Int) = TransformMesh(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(factor)), Reuse.unit)

  def toTransformMesh = TransformMesh(this, Repetition.unit, Reuse.unit)

  def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int): TransformBase = throw new IllegalArgumentException("this transform is not foldable")

  def flowFormat = MeshFormat(this, Repetition.unit, Reuse(1, 1, spaceFold, timeFold))
}
