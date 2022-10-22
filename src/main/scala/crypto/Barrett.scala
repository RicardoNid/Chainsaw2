package org.datenlord
package crypto

import org.datenlord.{TransformBase, TransformModule, logger}
import spinal.core._
import spinal.lib._

case class BarrettConfig(k: Int, M: BigInt, MPrime: BigInt) extends TransformBase {

  require(M.bitLength == k, s"${M.bitLength} != $k")

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val Seq(a, b) = data
    Seq((a * b) % M)
  }

  override val size = (2, 1)

  override def latency = 1

  override def implH = Barrett(this)
}

case class Barrett(config: BarrettConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(k bits), inputPortWidth))

  val Seq(a, b) = dataIn.fragment

  val N = a * b
  val NLow = N.takeLow(k + 2).asUInt
  val NHigh = N.takeHigh(k + 1).asUInt
  val u = NHigh * MPrime
  val E = u.takeHigh(k + 1).asUInt
  val ME = (E * M).takeLow(k + 2).asUInt

  require(ME.getBitsWidth == k + 2)

  val T = (NLow - ME).intoSInt
  val sub0 = T -^ M
  val sub1 = sub0 -^ M

  val temp = Mux(sub1.msb, sub0, sub1)
  val R = Mux(sub0.msb, T, sub0).asUInt

  logger.info(s"R bits = ${R.getBitsWidth}")

  override val dataOut = master Flow Fragment(Vec(UInt(k bits), outputPortWidth))

  dataOut.fragment.head := R.resize(k).d(1)

  autoValid()
  autoLast()
}