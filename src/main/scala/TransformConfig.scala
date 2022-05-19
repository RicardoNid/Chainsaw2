package org.datenlord

abstract class TransformConfig {

  // fold-independent attributes
  def impl(dataIn: Seq[Any]): Seq[Any]

  // user-defined
  val size: (Int, Int) // input/output size of impl

  def latency: Int

  def flowFormat: PeriodicFlow

  def implH: TransformModule[_, _]

  // auto
  def inputFlow: DataFlow = flowFormat.inputFlow

  def outputFlow: DataFlow = flowFormat.outputFlow

  def inputWidth = inputFlow.portWidth

  def outputWidth = inputFlow.portWidth
}
