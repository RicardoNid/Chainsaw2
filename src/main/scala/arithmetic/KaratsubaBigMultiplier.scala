package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class KaratsubaBigMultiplierConfig(width: Int, baseWidth: Int = 34,
                                        baseMult: (UInt, UInt) => UInt, baseMultLatency: Int)

case class KaratsubaBigMultiplier(config: KaratsubaBigMultiplierConfig) extends Component {

  import config._

  val dataIn = slave Flow Vec(UInt(width bits), 2)
  val dataOut = master Flow UInt(width * 2 bits)

  val Seq(x, y) = dataIn.payload

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

  dataOut.payload := recursiveTask(width, x, y).resized
  dataOut.valid := dataIn.valid.validAfter(KaratsubaBigMultiplier.latency(config))
}

object KaratsubaBigMultiplier {

  def latency(config: KaratsubaBigMultiplierConfig) = {
    import config._
    val limit = log2Up(width) * 2
    val layers = Array.iterate(width, limit)(width => (width + 1) / 2 + 1).count(_ > baseWidth) + 1
    val additionLatency = 3
    baseMultLatency + (layers - 1) * additionLatency
  }
}
