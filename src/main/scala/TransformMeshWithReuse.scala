package org.datenlord

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.reflect.ClassTag

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
    val config0 = flowConverters.PermutationByRamConfig(Seq(1, 2, 3, 0), 4, 4)
    val data0 = Seq(0, 1, 2, 3, 4, 5, 6, 7).map(BigInt(_))
    val mesh0 = TransformMeshWithReuse(config0, Repetition(Seq(SpaceRepetition(2)), TimeRepetition(2)), Reuse.unit)
    val mesh1 = TransformMeshWithReuse(config0, Repetition(Seq(SpaceRepetition(2)), TimeRepetition(2)), Reuse(2, 2, 1, 1))

    // test for a 2 * 2 mesh
    //    TransformTest.test(mesh0.implForTest(UInt(4 bits), UInt(4 bits)), data0)
    TransformTest.test(mesh1.implForTest(UInt(4 bits), UInt(4 bits)), data0)
  }
}


