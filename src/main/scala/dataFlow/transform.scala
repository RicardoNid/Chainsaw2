package org.datenlord
package dataFlow

import spinal.core._
import spinal.lib._

abstract class TransformConfig {

  def latency: Int

  def inputFlow: DataFlow

  def outputFlow: DataFlow

  def transform(dataIn: Seq[BigInt]): Seq[BigInt]

}

abstract class TransformModule[TIn <: BaseType, TOut <: BaseType] extends Component {

  val config: TransformConfig
  val dataIn: Flow[Fragment[Vec[TIn]]]
  val dataOut: Flow[Fragment[Vec[TOut]]]

  def autoValid(): Unit = dataOut.valid := dataIn.valid.validAfter(config.latency)

  def autoLast(): Unit = dataOut.last := dataIn.last.validAfter(config.latency)

}
