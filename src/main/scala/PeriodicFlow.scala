package org.datenlord

case class PeriodicFlow(transform: TransformConfig, repetition: Repetition, reuse: Reuse) {

  type Slice = Array[Int]

  // repetition: base vector -> expanded vector

  import transform._

  val inPortWidth = size._1 * repetition.spaceFactor / reuse.spaceReuse / reuse.foldReuse
  val outPortWidth = size._2 * repetition.spaceFactor / reuse.spaceReuse / reuse.foldReuse

  val inputSize = repetition.expand(size)._1
  val outputSize = repetition.expand(size)._2

  val inputRange = (0 until inputSize).toArray
  val outputRange = (0 until outputSize).toArray

  val inputSegments: Array[Slice] = repetition.divide(inputRange)
  val outputSegments: Array[Slice] = outputRange.divide(repetition.spaceFactor)

  // reuse - vector -> array
  val iterationLatency = repetition.timeFactor / reuse.timeReuse * latency

  def segments2Iteration(segments: Array[Slice], portWidth: Int) = {
    segments
      .divide(reuse.spaceReuse).flatMap { slices =>
      val subSlices: Array[Array[Slice]] = slices.map(_.divide(reuse.foldReuse))
      val reordered = (0 until reuse.foldReuse).map(i => subSlices.flatMap(_.apply(i))).toArray
      reordered
    }.padTo(iterationLatency, Array.fill(portWidth)(-1))
  }

  def getBubble(data: Array[Array[Int]]) = data.map(_.map(_ => -1))

  def iteration2Sequence(iteration: Array[Slice]) = {
    (iteration +: Array.fill(reuse.timeReuse - 1)(getBubble(iteration))).reduce(_ ++ _)
  }

  val inputSequence = iteration2Sequence(segments2Iteration(inputSegments, inPortWidth))
  val outputSequence = iteration2Sequence(segments2Iteration(outputSegments, outPortWidth))

  def inputFlow = BasicDataFlow(inputSequence.map(_.toSeq).toSeq)
  def outputFlow = BasicDataFlow(outputSequence.map(_.toSeq).toSeq)

  def drawInput(): Unit = inputFlow.generateWaveform("input", "x")

  def drawOutput(): Unit = outputFlow.generateWaveform("output", "y")

}

object PeriodicFlow { // examples

  def main(args: Array[String]): Unit = {

    val config0 = TransformConfigForTest((2, 2), 3)
    val config1 = TransformConfigForTest((2, 2), 4)
    val config2 = TransformConfigForTest((2, 2), 5)

    val size = (2, 2)
    val repeat = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(2))
    val reuse = Reuse(2, 2, 2)
    // no bubble, fifo
    println(PeriodicFlow(config0, repeat, reuse).inputFlow)
    println(PeriodicFlow(config0, repeat, reuse).outputFlow)
    // no bubble, no fifo
    println(PeriodicFlow(config1, repeat, reuse).inputFlow)
    println(PeriodicFlow(config1, repeat, reuse).outputFlow)
    // bubble, fifo
    println(PeriodicFlow(config2, repeat, reuse).inputFlow)
    println(PeriodicFlow(config2, repeat, reuse).outputFlow)
  }

}
