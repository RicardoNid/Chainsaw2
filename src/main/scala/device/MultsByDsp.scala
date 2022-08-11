package org.datenlord
package device

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import arithmetic.MultplierMode._

/** Dedicated pipeline for 34 * 52 multipliers
 */
case class MultiplicationByDspConfig(mode: MultiplierMode)
  extends TransformBase {

  val baseWidth = mode match {
    case FULL => 32
    case FULL34 => 34
    case HALFLOW => 34
    case SQUARE => 34
  }

  val opWidth = baseWidth / 2

  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val prod = bigInts.product
    mode match {
      case HALFLOW => Seq(prod % (BigInt(1) << baseWidth))
      case _ => Seq(prod)
    }
  }

  override val size = (2, 1)

  //  override def latency = 8
  override def latency = mode match {
    case FULL => 7
    case FULL34 => 7
    case HALFLOW => 6
    case SQUARE => 5
  }

  override def implH = MultiplicationByDsp(this)
}

case class MultiplicationByDsp(config: MultiplicationByDspConfig) extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(baseWidth bits), 2))

  val dataOut = master Flow Fragment(Vec(UInt(baseWidth * 2 bits)))

  val Seq(x, y) = dataIn.fragment
  val Seq(a, b) = x.subdivideIn(2 slices).reverse // xHigh, xLow
  val Seq(c, d) = y.subdivideIn(2 slices).reverse // yHigh, yLow

  val ret = mode match {
    case FULL =>
      // 0-1
      val cPlusD = (c +^ d).d(1)
      // 0-2
      val dsp0 = Dsp48.ab(a, c)
      val dsp1 = Dsp48.ab(b, d)
      // 2-5
      val dsp2 = Dsp48.adbc(a = a.d(2), b = cPlusD.d(1), c = dsp0, d = b.d(2), postMinus = true)

      dsp0.addAttribute("use_dsp", "yes")
      dsp1.addAttribute("use_dsp", "yes")
      dsp2.addAttribute("use_dsp", "yes")

      // 5-6
      val adbc = (dsp2 - dsp1.d(3)).d(1)
      adbc.addAttribute("use_dsp", "no")
      // 6-7
      val (high, mid, low) = (dsp0.d(4), adbc, dsp1.d(4))
      val ret = ((high @@ low) + (mid << opWidth)).d(1)
      ret
    case HALFLOW =>
      // 0-2
      val dsp0 = Dsp48.ab(b, c)
      val dsp0Low = dsp0.takeLow(opWidth).asUInt
      // 2-4
      val dsp1 = Dsp48.abc(a.d(2), d.d(2), dsp0Low)
      val dsp1Low = dsp1.takeLow(opWidth).asUInt << opWidth
      // 4-6
      val dsp2 = Dsp48.abc(b.d(4), d.d(4), dsp1Low)
      val ret = dsp2.takeLow(baseWidth).asUInt

      dsp0.addAttribute("use_dsp", "yes")
      dsp1.addAttribute("use_dsp", "yes")
      dsp2.addAttribute("use_dsp", "yes")

      ret
    case SQUARE =>
      // 0-2
      val dsp0 = Dsp48.ab(a, a)
      val dsp1 = Dsp48.ab(b, b)
      val long = dsp0 @@ dsp1 // 68 bits
      val (high, mid, low) = (long.takeHigh(4).asUInt, long(63 downto 18), long.takeLow(18).asUInt)
      // 2-4
      val dsp2 = Dsp48.abc(b.d(2), a.d(2), mid)

      dsp0.addAttribute("use_dsp", "yes")
      dsp1.addAttribute("use_dsp", "yes")
      dsp2.addAttribute("use_dsp", "yes")

      // 4-5
      val finalHigh = (high.d(2) + dsp2.msb.asUInt).d(1)
      val finalMid = dsp2.takeLow(46).asUInt.d(1)
      // 5-7
      val ret = finalHigh @@ finalMid @@ low.d(3)
      //      ret.d(2)
      ret
  }

  val defName = mode match {
    case FULL => "FullMult32"
    case HALFLOW => "LowMult34"
    case SQUARE => "SquareMult34"
  }

  setDefinitionName(defName)

  dataOut.fragment := Vec(ret.resize(baseWidth * 2))
  autoValid()
  autoLast()

}