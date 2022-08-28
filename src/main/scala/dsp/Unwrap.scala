package org.datenlord
package dsp

import matlab.MatlabFeval

import breeze.numerics.constants.Pi
import spinal.core._
import spinal.lib._

/** unwrap
 *
 */
case class UnwrapConfig(dataType: HardType[SFix]) extends TransformBase {
  override def impl(dataIn: Seq[Any]): Seq[Double] = {
    val data = dataIn.asInstanceOf[Seq[Double]].map(_ * Pi).toArray
    MatlabFeval[Array[Double]]("unwrap", 0, data).map(_ / Pi).tail
  }

  override val size = (2, 1)

  override def latency = 2

  override def implH = Unwrap(this)
}

case class Unwrap(config: UnwrapConfig)
  extends TransformModule[SFix, SFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(dataType(), 2))
  override val dataOut = master Flow Fragment(Vec(dataType(), 1))

  val fraction = -dataType().minExp

  // stage 0
  val Seq(prev, next) = dataIn.fragment // TODO: full-width prev data is not necessary
  val (m, l0) = prev.asBits.splitAt(fraction)
  val (n, l1) = next.asBits.splitAt(fraction)

  // 0 -> 1
  val mux0 = Mux(l1.asUInt > l0.asUInt, m.asSInt - 1, m.asSInt + 1).d(1)
  // 1 -> 2
  val mux1 = Mux((m.lsb === n.lsb).d(1), m.asSInt.d(1), mux0).d(1)

  dataOut.fragment.head.assignFromBits(mux1 ## l1.d(2))

  autoValid()
  autoLast()
}
