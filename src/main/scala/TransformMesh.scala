package org.datenlord

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.reflect.ClassTag

/** transform with repetition
 *
 */
case class TransformMesh(trans: TransformBase, repetition: Repetition, reuse: Reuse)
  extends TransformConfig {

  def ⊗(factor: Int, step: Int = -1) = TransformMesh(trans, repetition.⊗(factor, step), reuse)

  def ∏(factor: Int) = TransformMesh(trans, repetition.∏(factor), reuse)

  def °(that: TransformMesh) = TransformSystem(Seq(that, this))

  def toSystem = TransformSystem(Seq(this))

  def withReuse(reuse: Reuse): TransformMesh = TransformMesh(trans, repetition, reuse)

  require(trans.spaceFolds.contains(reuse.spaceFold), s"invalid space fold: candidates {${trans.spaceFolds.mkString(" ")}} doesn't contain ${reuse.spaceFold}")
  require(trans.timeFolds.contains(reuse.timeFold), s"invalid time fold: candidates {${trans.timeFolds.mkString(" ")}} doesn't contain ${reuse.timeFold}")

  val base = trans.getConfigWithFoldsChanged(reuse.spaceFold, reuse.timeFold)

  override def impl(dataIn: Seq[Any]) = repetition.getImplExpanded(base.impl)(dataIn)

  override val size = repetition.getSizeExpanded(base.size)

  override def flowFormat = MeshFormat(base, repetition, reuse)

  override def latency = flowFormat.latency

  override def implH: TransformModule[Bits, Bits] = {
    val meshConfig = this
    new TransformModule[Bits, Bits] {
      // TODO: "valid free" mode
      //  val counterForBase = CounterFreeRun(reuse.spaceFold * reuse.timeFold)
      //  prove that valid is not necessary for control, valid is for clearness
      //  only one valid for each transform
      //  only one group of counters for each transform
      val coresInRow = repetition.timeFactor / reuse.timeReuse
      val coresInColumn = repetition.spaceFactor / reuse.spaceReuse
      logger.info(s"mesh size: $coresInColumn, $coresInRow")
      val mesh = Seq.tabulate(coresInColumn, coresInRow)((_, _) => base.implH)
      val example = mesh.head.head
      val wrappers = mesh.map(_.map(_.getWrapper()))

      // I/O
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(example.inBitWidth bits), flowFormat.inPortWidth))
      override val dataOut = master Flow Fragment(Vec(Bits(example.outBitWidth bits), flowFormat.outPortWidth))

      // datapath
      // the mesh
      val meshInput = cloneOf(dataIn.fragment)
      meshInput.setName("meshInput")
      val meshOutput = cloneOf(dataOut.fragment)
      val meshIns = meshInput.divide(coresInColumn)
      wrappers.zip(meshIns).foreach { case (row, data) =>
        val segment = ChainsawFlow(Vec(data.map(_.asBits)), dataIn.valid, dataIn.last)
        row.head._1 << segment
        row.prevAndNext { case (prev, next) => next._1 << prev._2 }
      }
      // TODO: use ram-based FIFO for BigData
      meshOutput := Vec(wrappers.flatMap(_.last._2.fragment)).d(flowFormat.fifoLength)
      // the feedback path
      if (reuse.timeReuse > 1) {
        // controls
        val counterForIter = CounterFreeRun(flowFormat.iterationLatency)
        val counterForMux = Counter(reuse.timeReuse)
        when(dataIn.last) {
          counterForIter.clear()
          counterForMux.clear()
        }
        when(counterForIter.willOverflow)(counterForMux.increment())
        meshInput := Mux(counterForMux.value === 0, dataIn.fragment, meshOutput)
      } else {
        meshInput := dataIn.fragment
      }
      dataOut.fragment := meshOutput
      logger.info(s"iteration latency ${flowFormat.iterationLatency}")
      logger.info(s"total latency $latency")
      autoLast()
      autoValid()
    }
  }

  def implForTest[Tin <: Data : ClassTag, TOut <: Data : ClassTag](typeIn: HardType[Tin], typeOut: HardType[TOut]) = {
    val meshConfig = this
    new TransformModule[Tin, TOut] {
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(typeIn, flowFormat.inPortWidth))
      override val dataOut = master Flow Fragment(Vec(typeOut, flowFormat.inPortWidth))

      val core: TransformModule[Bits, Bits] = implH
      core.dataIn.assignFromBits(dataIn.asBits)
      dataOut.assignFromBits(core.dataOut.asBits)
    }
  }

  def fitTo(targetThroughput: Double) = {
    // brute-force method
    val spaceSize = trans.spaceFolds.length * trans.timeFolds.length *
      factors(repetition.spaceFactor).length * factors(repetition.timeFactor).length
    logger.info(s"DSE space size = $spaceSize")

    val dim0 = trans.spaceFolds.length
    val dim1 = trans.timeFolds.length
    val dim2 = factors(repetition.spaceFactor).length
    val dim3 = factors(repetition.timeFactor).length
    val candidates = Seq.tabulate(dim0, dim1, dim2, dim3) { (a, b, c, d) =>
      val spaceFold = trans.spaceFolds(a)
      val timeFold = trans.timeFolds(b)
      val spaceReuse = factors(repetition.spaceFactor)(c)
      val timeReuse = factors(repetition.timeFactor)(d)
      MeshFormat(trans, repetition, Reuse(spaceReuse, timeReuse, spaceFold, timeFold))
    }.flatten.flatten.flatten
    println(candidates.map(_.reuse).mkString(" "))
    println(candidates.map(_.throughput).mkString(" "))
    println(candidates.filter(_.throughput >= targetThroughput).length)
    val winner = candidates.filter(_.throughput >= targetThroughput)
      .sortBy(_.reuse.timeFold)
      .sortBy(_.reuse.spaceFold)
      .sortBy(_.reuse.timeReuse)
      .sortBy(_.reuse.spaceReuse).last
    withReuse(winner.reuse)
  }
}