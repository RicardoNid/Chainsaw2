package org.datenlord

import spinal.core._
import spinal.lib._

abstract class TransformConfig {

  val size: (Int, Int)

  // override only when folding is available
  val timeFold: Int = 1
  val spaceFold: Int = 1

  def latency: Int

  def flowFormat = PeriodicFlow(this, Repetition.unit, Reuse(1, 1, spaceFold, timeFold))

  def inputFlow: DataFlow = flowFormat.inputFlow

  def outputFlow: DataFlow = flowFormat.outputFlow

  def inputWidth = inputFlow.portWidth

  def outputWidth = inputFlow.portWidth

  def implH: TransformModule[_, _]

  def implHBits: TransformModule[Bits, Bits]

  def impl(dataIn: Seq[_]): Seq[_] = dataIn

  def toTransformMesh = TransformMesh(this, Repetition.unit)

  def ⊗(factor: Int, step: Int = -1) = TransformMesh(this, Repetition(Seq(SpaceRepetition(factor, step)), TimeRepetition(1)))

  def ∏(factor: Int) = TransformMesh(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(factor)))
}

/** Transform with no implementations, for dataflow analysis only
 *
 */
object TransformConfigForTest {
  def apply(theSize: (Int, Int), theLatency: Int) = new TransformConfig {
    override val size = theSize

    override def latency = theLatency

    override def inputFlow = null

    override def outputFlow = null

    override def implH = null

    override def implHBits = null
  }
}

abstract class TransformModule[TIn <: Data, TOut <: Data] extends Component {

  val config: TransformConfig
  val dataIn: Flow[Fragment[Vec[TIn]]]
  val dataOut: Flow[Fragment[Vec[TOut]]]

  def autoValid(): Unit = dataOut.valid := dataIn.valid.validAfter(config.latency)

  def autoLast(): Unit = dataOut.last := dataIn.last.validAfter(config.latency)

  def skipControl() = {
    dataIn.valid.assignDontCare()
    dataIn.last.assignDontCare()
    dataIn.valid.allowPruning()
    dataIn.last.allowPruning()
  }

  def autoInputCounter() = {
    val counter = Counter(config.inputFlow.period)
    when(dataIn.valid)(counter.increment())
    when(dataIn.last)(counter.clear())
    counter
  }
}

object TransformBitsWrapper {
  def apply[TIn <: Data, TOut <: Data](original: => TransformModule[TIn, TOut]) = {
    new TransformModule[Bits, Bits] {
      override val config = original.config
      val core = original
      val inWidth = core.dataIn.fragment.head.getBitsWidth
      val outWidth = core.dataOut.fragment.head.getBitsWidth

      import config._

      override val dataIn = slave Flow Fragment(Vec(Bits(inWidth bits), inputWidth))
      override val dataOut = master Flow Fragment(Vec(Bits(outWidth bits), outputWidth))

      core.dataIn.fragment.assignFromBits(dataIn.fragment.asBits)
      core.dataIn.valid := dataIn.valid
      core.dataIn.last := dataIn.last
      dataOut.fragment.assignFromBits(core.dataOut.fragment.asBits)
      dataOut.valid := core.dataOut.valid
      dataOut.last := core.dataOut.last
    }
  }
}
