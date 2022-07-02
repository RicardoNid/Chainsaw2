package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math.ceil

/**
 * @param addWidth
 * @param baseWidth
 */
case class PipelinedBigAdderConfig(addWidth: Int, baseWidth: Int = 127, minus: Boolean = false) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    if (!minus) Seq(bigInts.sum)
    else {
      assert(bigInts.head > bigInts.last, s"this module is for UInt and thus works only when x > y, for x - y")
      Seq(bigInts.head - bigInts.last)
    }
  }

  override val size = (2, 1)

  override def latency = (addWidth + baseWidth - 1) / baseWidth

  override def implH = PipelinedBigAdder(this)

  def asOperator = (x: UInt, y: UInt) => {
    val core = implH
    core.dataIn.fragment := Vec(x.resized, y.resized)
    core.skipControl()
    core.dataOut.fragment.head
  }
}

case class PipelinedBigAdder(config: PipelinedBigAdderConfig) extends TransformModule[UInt, UInt] {

  import config._

  val N = ceil(addWidth.toDouble / baseWidth).toInt

  val dataIn = slave Flow Fragment(Vec(UInt(addWidth bits), 2))
  val dataOut = master Flow Fragment(Vec(UInt(addWidth + 1 bits)))

  val Seq(x, y) = dataIn.fragment

  val xLow2High = x.subdivideIn(baseWidth bits, strict = false)
  val yLow2High = y.subdivideIn(baseWidth bits, strict = false)

  val xDelayed = xLow2High.zipWithIndex.map { case (x, delay) => x.d(delay) }
  val yDelayed = yLow2High.zipWithIndex.map { case (y, delay) => y.d(delay) }

  val carrys = ArrayBuffer[Bool](False)
  val sums = ArrayBuffer[UInt]()

  xDelayed.zip(yDelayed).foreach { case (x, y) =>
    val ret = ((x +^ y) + carrys.last.asUInt(1 bits)).d(1)
    carrys += ret.msb
    sums += ret.takeLow(ret.getBitsWidth - 1).asUInt
  }

  val alignedSums = sums.reverse.zipWithIndex.map { case (segment, i) => segment.d(i) } // high2low
  val ret = carrys.last.asUInt(1 bits) @@ alignedSums.reduce(_ @@ _)

  dataOut.fragment := Vec(ret)
  autoValid()
  autoLast()
}