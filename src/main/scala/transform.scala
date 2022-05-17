package org.datenlord

import spinal.core._
import spinal.lib._

abstract class TransformConfig {

  val size: (Int, Int)

  def latency: Int

  def inputFlow: DataFlow

  def outputFlow: DataFlow

  def inputWidth = inputFlow.portWidth

  def outputWidth = inputFlow.portWidth

  def implH: TransformModule[_, _]

  def impl(dataIn: Seq[_]): Seq[_] = dataIn

  //  def toTransformMesh = TransformMesh(this, Repetition.unit)
  //
  //  def ⊗(factor: Int, step: Int = -1) = TransformMesh(this, Repetition(Seq(SpaceRepetition(factor, step)), TimeRepetition(1)))
  //
  //  def ∏(factor: Int) = TransformMesh(this, Repetition(Seq(SpaceRepetition(1)), TimeRepetition(factor)))
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
}
