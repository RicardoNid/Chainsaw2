package org.datenlord

import scala.reflect.ClassTag

case class SpaceRepetition(factor: Int, step: Int = -1) {

  def expand(size: (Int, Int)) = {
    val (inputSize, outputSize) = size
    if (step == -1) (inputSize * factor, outputSize * factor)
    else (step * (factor - 1) + inputSize, outputSize * factor)
  }

  def divide[T:ClassTag](vec: Seq[T]) =
    if(step == -1) vec.grouped(vec.length / factor).toSeq
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

  def expand(size: (Int,Int)) = {
    var init = size
    space.foreach(rep => init = rep.expand(init))
    init
  }

  def divide[T:ClassTag](dataIn: Seq[T]) = {
    var segments = Seq(dataIn)
    space.reverse.foreach { rep =>
      segments = segments.map(segment => rep.divide(segment)).flatten
    }
    segments
  }

}

object Repetition {
  def unit = Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1))
}