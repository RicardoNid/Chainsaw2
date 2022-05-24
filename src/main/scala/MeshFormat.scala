package org.datenlord

case class MeshFormat(baseSize: (Int, Int), baseLatency: Int, repetition: Repetition, reuse: Reuse) {

  type Slice = Seq[Int]

  // repetition: base vector -> expanded vector
  val baseSizeIn = baseSize._1
  val baseSizeOut = baseSize._2
  val inPortWidth = baseSizeIn * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold
  val outPortWidth = baseSizeOut * repetition.spaceFactor / reuse.spaceReuse / reuse.spaceFold

  val (inputSegments, outputSegments) = repetition.getSegmentsExpanded(baseSize)

  val queue = repetition.timeFactor / reuse.timeReuse * baseLatency
  val inQueue = reuse.spaceReuse * reuse.timeFold * reuse.spaceFold
  // when inQueue > queue, need fifo
  val iterationLatency = inQueue max queue
  val fifoLength = (inQueue - queue) max 0
  // when inQueue < queue, util < 1
  val util = if (reuse.timeReuse > 1) inQueue.toDouble / iterationLatency else 1
  val throughput = util / (reuse.timeReuse * reuse.spaceReuse * reuse.timeFold * reuse.spaceFold)
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

  def inputFlow = BasicDataFlow(inputSequence.map(_.toSeq))

  def outputFlow = BasicDataFlow(outputSequence.map(_.toSeq))

  // TODO: better definition
  def period = inputFlow.period

  def drawInput(): Unit = inputFlow.generateWaveform("input", "x")

  def drawOutput(): Unit = outputFlow.generateWaveform("output", "y")
}

object MeshFormat {
  def apply(transform: TransformBase, repetition: Repetition, reuse: Reuse): MeshFormat =
    MeshFormat(transform.size, transform.latency, repetition, reuse)

  def dontCare = MeshFormat((1,1), 1, Repetition.unit, Reuse.unit)
}