package org.datenlord

import spinal.core._
import spinal.lib._

abstract class Transform {

  val size: (Int, Int)
  val timeFolds: Seq[Int]
  val spaceFolds: Seq[Int]

  def impl(dataIn: Seq[Any]): Seq[Any] = dataIn

  def getConfig(timeFold: Int, spaceFold: Int): TransformConfig
}

abstract class TransformConfig {

  // hardware-independent attributes
  val size: (Int, Int)

  def impl(dataIn: Seq[Any]): Seq[Any] = dataIn

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

  def getWrapper() = {
    val inWidth = dataIn.fragment.length
    val inBitWidth = dataIn.fragment.head.getBitsWidth
    val outWidth = dataOut.fragment.length
    val outBitWidth = dataOut.fragment.head.getBitsWidth
    val dataInBits = Flow Fragment Vec(Bits(inBitWidth bits), inWidth)
    val dataOutBits = Flow Fragment Vec(Bits(outBitWidth bits), outWidth)
    dataIn.fragment.assignFromBits(dataInBits.fragment.asBits)
    dataIn.valid := dataInBits.valid
    dataIn.last := dataInBits.last
    dataOutBits.fragment.assignFromBits(dataOut.fragment.asBits)
    dataOutBits.valid := dataOut.valid
    dataOutBits.last := dataOut.last
    (dataInBits, dataOutBits)
  }

}

// TODO: make generated code shorter
object TransformBitsWrapper {
  def apply[TIn <: Data, TOut <: Data](original: => TransformModule[TIn, TOut]) = {
    logger.info("wrapper invoked")
    new TransformModule[Bits, Bits] {
      val core = original
      override val config = core.config
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
