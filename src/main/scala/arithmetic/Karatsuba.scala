package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class KaratusbaConfig(width: Int, baseWidth: Int = 34, baseMult: (UInt, UInt) => UInt, baseMultLatency: Int) extends TransformBase {

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  override val size = (2, 1)

  override def latency = {
    val limit = log2Up(width) * 2
    // layers of recursion
    val layers = Array.iterate(width, limit)(width => (width + 1) / 2 + 1).count(_ > baseWidth) + 1
    // latency for addition in each layer
    val additionLatency = 3
    baseMultLatency + (layers - 1) * additionLatency
  }
  override def implH = KaratsubaBigMultiplier(this)
}

case class KaratsubaBigMultiplierConfig(width: Int, baseWidth: Int = 34,
                                        baseMult: (UInt, UInt) => UInt, baseMultLatency: Int)

case class KaratsubaBigMultiplier(config: KaratusbaConfig) extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(width bits), 2))
  val dataOut = master Flow Fragment(Vec(UInt(width * 2 bits), 1))

  val Seq(x, y) = dataIn.fragment

  def recursiveTask(width: Int, x: UInt, y: UInt): UInt = {
    if (width <= baseWidth) baseMult(x, y)
    else {
      // split x,y into high & low, according to a multiple of basewidth
      val split = (width + 1) / 2
      val widthNext = split + 1
      val (xHighBits, xLowBits) = x.splitAt(split)
      val (yHighBits, yLowBits) = y.splitAt(split)
      val Seq(xHigh, xLow, yHigh, yLow) = Seq(xHighBits, xLowBits, yHighBits, yLowBits).map(_.asUInt)

      val aPlusB = (xHigh +^ xLow).d(1)
      val cPlusD = (yHigh +^ yLow).d(1)

      val ac = recursiveTask(widthNext, xHigh, yHigh)
      val bd = recursiveTask(widthNext, xLow, yLow)
      val all = recursiveTask(widthNext, aPlusB, cPlusD)

      bd.setName(s"bd_${bd.getWidth}", weak = true)
      all.setName(s"all_${all.getWidth}", weak = true)
      ac.setName(s"ac_${ac.getWidth}", weak = true)

      val adbc = (all -^ ac.d(1) -^ bd.d(1)).d(1)

      val ret = (ac.d(2) << (split * 2)) +^ ((adbc << split) +^ bd.d(2))
      ret.d(1)
    }
  }

  dataOut.fragment := Vec(recursiveTask(width, x, y).resized)
  autoValid()
  autoLast()
}