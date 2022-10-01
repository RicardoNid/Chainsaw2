package org.datenlord
package dsp

import matlab.MatlabFeval

import breeze.numerics.constants.Pi
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

/** unwrap
 *
 */
case class UnwrapConfig(typeStored: HardType[SFix], typeFull: HardType[SFix]) extends TransformBase {
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

  override val dataIn = slave Flow Fragment(Vec(typeStored(), typeFull()))
  override val dataOut = master Flow Fragment(Vec(typeFull(), 1))

  val fractionStored = -typeStored().minExp
  val fractionFull = -typeFull().minExp
  require(fractionFull >= fractionStored)

  // stage 0
  val Seq(prev, next) = dataIn.fragment // TODO: full-width prev data is not necessary
  val (m, l0) = prev.asBits.splitAt(fractionStored)
  val (n, l1) = next.asBits.splitAt(fractionFull)
  val l1Main = l1.takeHigh(fractionStored)

  val random = CounterFreeRun(13)

  // 0 -> 1
  val mux0 = SInt(m.getBitsWidth bits)
  when(l1Main.asUInt > l0.asUInt)(mux0 := m.asSInt - 1)
    .elsewhen(l1Main.asUInt < l0.asUInt)(mux0 := m.asSInt + 1)
    .elsewhen(l1Main.asUInt === l0.asUInt && random.value.lsb)(mux0 := m.asSInt - 1)
    .otherwise(mux0 := m.asSInt + 1)
  // 1 -> 2
  val mux1 = Mux((m.lsb === n.lsb).d(1), m.asSInt.d(1), mux0).d(1)
  val ret = typeFull()
  ret.assignFromBits(mux1 ## l1.d(2))

  dataOut.fragment.head.assignFromBits(mux1 ## l1.d(2))

  autoValid()
  autoLast()
}

object Unwrap {
  def pointwiseUnwrap(prev: Double, next: Double) = {
    val m = prev.floor
    val candidates = Seq(-1, 0, 1).map(_ + m + (next - next.floor))
    candidates.find(ret => (ret - prev).abs <= 1 && (ret - next).abs.round % 2 == 0).get
  }

  def unwrap(seq: Seq[Double]) = seq.tail.scan(seq.head)(pointwiseUnwrap)

  def main(args: Array[String]): Unit = {
    Random.setSeed(42)
    val data = Seq.fill(100)(Random.nextDouble() * 3 - 3)
    val golden = MatlabFeval[Array[Double]]("unwrap", 0, data.toArray.map(_ * Pi)).map(_ / Pi)
    val yours = unwrap(data)
    val diff = yours.zip(golden).map { case (y, g) => (y - g).abs }
    assert(diff.forall(_ < 1e-2))
  }
}
