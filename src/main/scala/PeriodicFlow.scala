package org.datenlord

case class PeriodicFlow(transform: TransformConfig, repetition: Repetition, reuse: Reuse) {

  type Slice = Array[Int]

  // repetition: base vector -> expanded vector

  import transform._

  val inPortWidth = size._1 * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold
  val outPortWidth = size._2 * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold

  val inputSize = repetition.expand(size)._1
  val outputSize = repetition.expand(size)._2

  val inputRange = (0 until inputSize).toArray
  val outputRange = (0 until outputSize).toArray

  val inputSegments: Array[Slice] = repetition.divide(inputRange)
  val outputSegments: Array[Slice] = outputRange.divide(repetition.spaceFactor)

  // reuse - vector -> array
  val iterationLatency = {
    val wait = repetition.timeFactor / reuse.timeReuse * latency
    val queue = reuse.spaceReuse * reuse.timeFold * reuse.spaceFold
    // when queue > wait, need fifo
    // util = queue / wait
    wait max queue
  }

  def segments2Iteration(segments: Array[Slice], portWidth: Int) = {
    val noBubble = segments
      .divide(reuse.spaceReuse).flatMap { slices => // building the queue to fill in the iteration latency
      if (reuse.spaceFold != 1) {
        val subSlices: Array[Array[Slice]] = slices.map(_.divide(reuse.spaceFold))
        val reordered: Array[Slice] = (0 until reuse.spaceFold).map(i => subSlices.flatMap(_.apply(i))).toArray
        reordered
      } else {
        val bubble = Array.fill(reuse.timeFold - 1)(getBubble(slices)).flatten
        val ret: Array[Slice] = (slices ++ bubble).grouped(slices.length).toSeq.map(_.flatten).toArray
        ret
      }
    }
    if (reuse.timeReuse > 1) noBubble.padTo(iterationLatency, Array.fill(portWidth)(-1))
    else noBubble
  }

  def getBubble(data: Array[Slice]) = data.map(_.map(_ => -1))

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

    // basic
    val basic = TransformConfigForTest((2, 2), 1)
    val spaceFold = Reuse(1, 1, 2, 1)
    val timeFold = Reuse(1, 1, 1, 2)
    val noReuse = Reuse.unit
    val noRepeat = Repetition.unit

    println("basic")
    println(PeriodicFlow(basic, noRepeat, noReuse).inputFlow)
    println(PeriodicFlow(basic, noRepeat, spaceFold).inputFlow)
    println(PeriodicFlow(basic, noRepeat, timeFold).outputFlow)

    // complex
    val config0: TransformConfig = TransformConfigForTest((2, 2), 3)
    val config1: TransformConfig = TransformConfigForTest((2, 2), 4)
    val config2: TransformConfig = TransformConfigForTest((2, 2), 5)

    val repeat = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(2))
    val reuse0 = Reuse(2, 2, 2, 1)
    val reuse1 = Reuse(2, 2, 1, 2)
    val reuse2 = Reuse(2, 2, 1, 1)
    // no bubble, fifo
    println(PeriodicFlow(config0, repeat, reuse0).inputFlow)
    println(PeriodicFlow(config0, repeat, reuse0).outputFlow)
    // no bubble, no fifo
    println(PeriodicFlow(config1, repeat, reuse0).inputFlow)
    println(PeriodicFlow(config1, repeat, reuse0).outputFlow)
    // bubble, fifo
    println(PeriodicFlow(config2, repeat, reuse0).inputFlow)
    println(PeriodicFlow(config2, repeat, reuse0).outputFlow)
    // no bubble, fifo
    println(PeriodicFlow(config0, repeat, reuse1).inputFlow)
    println(PeriodicFlow(config0, repeat, reuse1).outputFlow)
    // no bubble, no fifo
    println(PeriodicFlow(config1, repeat, reuse1).inputFlow)
    println(PeriodicFlow(config1, repeat, reuse1).outputFlow)
    // bubble, fifo
    println(PeriodicFlow(config2, repeat, reuse1).inputFlow)
    println(PeriodicFlow(config2, repeat, reuse1).inputFlow)
  }

}
