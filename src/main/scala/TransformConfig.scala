package org.datenlord

import scala.reflect.ClassTag
import scala.util.Random

abstract class TransformConfig {

  // fold-independent attributes
  def impl(dataIn: Seq[Any]): Seq[Any]

  // user-defined
  val size: (Int, Int) // input/output size of impl

  def latency: Int

  def flowFormat: MeshFormat

  def implH: TransformModule[_, _]

  // auto
  def inputFlow: DataFlow = flowFormat.inputFlow

  def outputFlow: DataFlow = flowFormat.outputFlow

  def inputWidth = inputFlow.portWidth

  def outputWidth = inputFlow.portWidth

  def getRandomDataIn[T:ClassTag](randGen: () => T): Seq[T] =
    Random.RandomSequence(10 * inputFlow.rawDataCount, randGen)

}
