package org.datenlord

abstract class TransformBase extends TransformConfig {

  // override when folding is available/natural, otherwise, just leave it here
  val spaceFold: Int = 1

  def spaceFolds: Seq[Int] = Seq(1)

  val timeFold: Int = 1

  def timeFolds: Seq[Int] = Seq(1)

  // repetition operations
  def ⊗(factor: Int, step: Int = -1) = TransformMesh(this, Repetition(Seq(SpaceRepetition(factor, step)), TimeRepetition(1)))

  def ∏(factor: Int) = TransformMesh(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(factor)))

  def toTransformMesh = TransformMesh(this, Repetition.unit)

  def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int): TransformBase = this

  def flowFormat = PeriodicFlow(this, Repetition.unit, Reuse(1, 1, spaceFold, timeFold))
}
