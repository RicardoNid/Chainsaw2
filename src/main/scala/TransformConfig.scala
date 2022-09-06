package org.datenlord

import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}

import scala.reflect.ClassTag
import scala.util.Random

abstract class TransformConfig {

  // fold-independent attributes
  def impl(dataIn: Seq[Any]): Seq[Any]

  val implMode: ImplMode = Comb

  // user-defined
  val size: (Int, Int) // input/output size of impl

  def latency: Int

  def flowFormat: MeshFormat

  def implH: TransformModule[_, _]

  // auto
  def inputFlow: DataFlow = flowFormat.inputFlow

  def outputFlow: DataFlow = flowFormat.outputFlow

  def inputPortWidth = inputFlow.portWidth

  def outputPortWidth = outputFlow.portWidth

  def getRandomDataIn[T: ClassTag](randGen: () => T): Seq[T] =
    Random.RandomSequence(10 * inputFlow.rawDataCount, randGen)

  // util estimation of the transform, it is not set by default
  def utilRequirement: VivadoUtil = VivadoUtilRequirement()

}
