package org.datenlord

case class PeriodicFlow(transform: TransformBase, repetition: Repetition, reuse: Reuse) {

  type Slice = Seq[Int]

  // repetition: base vector -> expanded vector

  val baseSizeIn = transform.size._1
  val baseSizeOut = transform.size._2
  val inPortWidth = baseSizeIn * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold
  val outPortWidth = baseSizeOut * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold

  val (inputSegments, outputSegments) = repetition.getSegmentsExpanded(transform.size)

  val queue = repetition.timeFactor / reuse.timeReuse * transform.latency
  val inQueue = reuse.spaceReuse * reuse.timeFold * reuse.spaceFold
  // when inQueue > queue, need fifo
  val iterationLatency = inQueue max queue
  val fifoLength = (inQueue - queue) max 0
  // when inQueue < queue, util < 1
  val util = inQueue.toDouble / iterationLatency
  val latency = reuse.timeReuse * iterationLatency

  def segments2Iteration(segments: Seq[Slice], portWidth: Int) = {
    val noBubble = segments
      .divide(reuse.spaceReuse).flatMap { slices => // building the queue to fill in the iteration latency
      if (reuse.spaceFold != 1) {
        val subSlices = slices.map(_.divide(reuse.spaceFold))
        val reordered = (0 until reuse.spaceFold).map(i => subSlices.flatMap(_.apply(i)))
        reordered
      } else {
        val bubble = Seq.fill(reuse.timeFold - 1)(getBubble(slices)).flatten
        val ret = (slices ++ bubble).grouped(slices.length).toSeq.map(_.flatten)
        ret
      }
    }
    if (reuse.timeReuse > 1) noBubble.padTo(iterationLatency, Seq.fill(portWidth)(-1))
    else noBubble
  }

  def getBubble(data: Seq[Slice]) = data.map(_.map(_ => -1))

  def iteration2Sequence(iteration: Seq[Slice]) = {
    (iteration +: Seq.fill(reuse.timeReuse - 1)(getBubble(iteration))).reduce(_ ++ _)
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
    val config0 = TransformConfigForTest((2, 2), 3)
    val config1 = TransformConfigForTest((2, 2), 4)
    val config2 = TransformConfigForTest((2, 2), 5)

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
