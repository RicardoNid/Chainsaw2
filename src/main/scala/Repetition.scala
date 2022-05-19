package org.datenlord

import scala.reflect.ClassTag

case class SpaceRepetition(factor: Int, step: Int = -1) {

  def expand(size: (Int, Int)) = {
    val (inputSize, outputSize) = size
    if (step == -1) (inputSize * factor, outputSize * factor)
    else (step * (factor - 1) + inputSize, outputSize * factor)
  }

  def divide[T: ClassTag](vec: Seq[T]) =
    if (step == -1) vec.grouped(vec.length / factor).toSeq
    else vec.sliding(vec.length - step * (factor - 1), step).toSeq

}

case class TimeRepetition(factor: Int)

case class Repetition(space: Seq[SpaceRepetition], time: TimeRepetition) {

  def spaceFactor = space.map(_.factor).product

  def timeFactor = time.factor

  def ⊗(factor: Int, step: Int = -1) = {
    if (step == -1) {
      if (space.last.step == -1) Repetition(space.init :+ SpaceRepetition(space.last.factor * factor), time)
      else Repetition(space :+ SpaceRepetition(factor), time)
    }
    else Repetition(space :+ SpaceRepetition(factor, step), time)
  }

  def ∏(factor: Int) = Repetition(space, TimeRepetition(time.factor * factor))

  def getSizeExpanded(size: (Int, Int)) = {
    var init = size
    space.foreach(rep => init = rep.expand(init))
    init
  }

  def getSegmentsExpanded(size: (Int, Int)) = {
    val expandedSize = getSizeExpanded(size)
    val segmentsIn = divideInput(0 until expandedSize._1)
    val segmentsOut = (0 until expandedSize._2).divide(spaceFactor)
    (segmentsIn, segmentsOut)
  }

  def divideInput[T: ClassTag](dataIn: Seq[T]) = {
    var segments = Seq(dataIn)
    space.reverse.foreach { rep =>
      segments = segments.flatMap(segment => rep.divide(segment))
    }
    segments
  }

  def getImplExpanded(impl: Seq[Any] => Seq[Any]) = (dataIn: Seq[Any]) => {
    divideInput(dataIn).flatMap { segment =>
      var temp = segment
      (0 until timeFactor).foreach(_ => temp = impl(temp))
      temp
    }
  }

}

object Repetition {
  def unit = Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1))
}