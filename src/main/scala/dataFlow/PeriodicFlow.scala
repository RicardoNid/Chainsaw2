package org.datenlord
package dataFlow

import dsl.{Repetition, Reuse}

case class PeriodicFlow(size: (Int, Int), repetition: Repetition, reuse: Reuse) {

  def getBubble(data: Array[Array[Int]]) = data.map(_.map(_ => -1))

  type Slice = Array[Int]

  val inPortWidth = size._1 * repetition.spaceFactor / reuse.spaceReuse / reuse.fold
  val outPortWidth = size._2 * repetition.spaceFactor / reuse.spaceReuse / reuse.fold

  val inputSize = repetition.expand(size)._1
  val outputSize = repetition.expand(size)._2

  val inputRange = (0 until inputSize).toArray
  val outputRange = (0 until outputSize).toArray
  val inputSegments: Array[Slice] = repetition.divide(inputRange)
  val outputSegments: Array[Slice] = outputRange.divide(repetition.spaceFactor)

  def segments2Iteration(segments: Array[Slice], portWidth: Int) = {
    segments
      .divide(reuse.spaceReuse)
      .map { slices =>
        val subSlices: Array[Array[Slice]] = slices.map(_.divide(reuse.fold))
        val reordered = (0 until reuse.fold).map(i => subSlices.map(_.apply(i)).flatten).toArray
        reordered
      }.flatten.padTo(reuse.iterationLatency, Array.fill(portWidth)(-1))
  }

  def iteration2Sequence(iteration: Array[Slice]) = {
    (iteration +: Array.fill(reuse.timeReuse - 1)(getBubble(iteration))).reduce(_ ++ _)
  }

  val inputSequence = iteration2Sequence(segments2Iteration(inputSegments, inPortWidth))
  val outputSequence = iteration2Sequence(segments2Iteration(outputSegments, outPortWidth))

  val inputFlow = BasicDataFlow(inputSequence.map(_.toSeq).toSeq)
  val outputFlow = BasicDataFlow(outputSequence.map(_.toSeq).toSeq)

  def drawInput = inputFlow.generateWaveform("input", "x")

  def drawOutput = outputFlow.generateWaveform("output", "y")

}

object PeriodicFlow { // examples

  def main(args: Array[String]): Unit = {


  }

}
