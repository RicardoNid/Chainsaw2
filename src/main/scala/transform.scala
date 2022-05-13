package org.datenlord

import breeze.math._
import spinal.core._
import spinal.lib._

abstract class TransformConfig {

  def latency: Int

  def inputFlow: DataFlow

  def outputFlow: DataFlow

  def bitTransform(dataIn: Seq[BigInt]): Seq[BigInt] = dataIn

  def complexTransform(dataIn: Seq[Complex]): Seq[Complex] = dataIn

  def bit2ComplexTransform(dataIn: Seq[BigInt]): Seq[Complex] = Seq(Complex(0, 0))

//  def impl: TransformModule[_, _]
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
