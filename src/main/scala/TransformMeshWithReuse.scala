package org.datenlord

import breeze.linalg.DenseMatrix
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.Random

/** transform with repetition
 *
 */
case class TransformMeshWithReuse(trans: TransformBase, repetition: Repetition, reuse: Reuse)
  extends TransformConfig {

  val base = trans.getConfigWithFoldsChanged(reuse.spaceFold, reuse.timeFold)

  override def impl(dataIn: Seq[Any]) = repetition.getImplExpanded(base.impl)(dataIn)

  override val size = repetition.getSizeExpanded(base.size)

  override def flowFormat = PeriodicFlow(base, repetition, reuse)

  override def latency = flowFormat.latency

  override def implH = {
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
      val wrappers = mesh.map(_.map(_.getWrapper()))
      val inBitWidth = wrappers.head.head._1.fragment.head.getBitsWidth
      val outBitWidth = wrappers.head.head._2.fragment.head.getBitsWidth

      // I/O
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(inBitWidth bits), flowFormat.inPortWidth))
      override val dataOut = master Flow Fragment(Vec(Bits(outBitWidth bits), flowFormat.outPortWidth))

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
}

object TransformMeshWithReuse {
  def main(args: Array[String]): Unit = {

    // space reuse + time reuse + space fold
    val data0 = Random.RandomSequences(1, 12, Random.nextComplex).head
    val dataType0 = HardType(ComplexFix(3 exp, -12 exp))
    val coeff0 = Random.RandomSequences(1, 2, Random.nextComplex).head
    val config0 = arithmetic.DiagonalConfig(coeff0, dataType0, dataType0, spaceFold = 1)

    // fifo, no bubble
    val repetition0 = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(2))
    val reuse0 = Reuse(4, 2, 2, 1)
    val mesh0 = TransformMeshWithReuse(config0, repetition0, reuse0)
    assert(mesh0.flowFormat.queue < mesh0.flowFormat.inQueue)
    TransformTest.test(mesh0.implForTest(dataType0, dataType0), data0, Metric.ComplexAbs(1e-2))
    // bubble, no fifo
    val repetition1 = Repetition(Seq(SpaceRepetition(2, 1), SpaceRepetition(2)), TimeRepetition(4))
    val reuse1 = Reuse(2, 2, 2, 1)
    val mesh1 = TransformMeshWithReuse(config0, repetition1, reuse1)
    assert(mesh1.flowFormat.queue > mesh1.flowFormat.inQueue)
    assert(mesh1.flowFormat.util < 1)
    TransformTest.test(mesh1.implForTest(dataType0, dataType0), data0, Metric.ComplexAbs(1e-2))

    // space reuse + time reuse + time fold
    val data1 = Random.RandomSequences(1, 20, Random.nextDouble).head
    val dataType1 = HardType(SFix(4 exp, -11 exp))
    val coeff1 = DenseMatrix.rand[Double](4, 4)
    val config1 = arithmetic.AlgebraConfig(coeff1, dataType1, dataType1, timeFold = 1) // latency = 3

    // fifo, no bubble
    val reuse2 = Reuse(4, 2, 1, 2)
    val mesh2 = TransformMeshWithReuse(config1, repetition0, reuse2)
    assert(mesh2.flowFormat.queue < mesh2.flowFormat.inQueue)
    TransformTest.test(mesh2.implForTest(dataType1, dataType1), data1, Metric.DoubleAbs(1e-2))
    // bubble, no fifo
    val reuse3 = Reuse(2, 2, 1, 2)
    val mesh3 = TransformMeshWithReuse(config1, repetition1, reuse3)
    assert(mesh3.flowFormat.queue > mesh3.flowFormat.inQueue)
    assert(mesh3.flowFormat.util < 1)
    TransformTest.test(mesh3.implForTest(dataType1, dataType1), data1, Metric.DoubleAbs(1e-1))


  }
}


