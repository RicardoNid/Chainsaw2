package org.datenlord
package device

import org.datenlord.xilinx.{VivadoUtil, VivadoUtilRequirement}
import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps
import scala.util.Random

// TODO: add parser for this
object Dsp48 {
  // when pre-addition is on, coeffMax - 1
  // when using unsigned operands, all widths - 1
  private def widthCheck(a: Int, b: Int, c: Int, d: Int, isUnsigned: Boolean, preOn: Boolean): Unit = {
    var dataMax = 27
    var coeffMax = 18
    var aluMax = 47
    if (isUnsigned) {
      dataMax -= 1
      coeffMax -= 1
      aluMax -= 1
    }
    if (preOn) coeffMax -= 1
    assert(a <= coeffMax && d <= coeffMax && b <= dataMax && c <= aluMax)
  }

  /** (a ± d) * b ± c, latency = 3, using adreg + mreg + preg */
  def adbc(a: UInt, b: UInt, c: UInt, d: UInt, preMinus: Boolean = false, postMinus: Boolean = false, postInverse: Boolean = false) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, c.getBitsWidth, d.getBitsWidth, isUnsigned = true, preOn = true)
    val preAdd = if (preMinus) a -^ d else a +^ d
    val prod = (preAdd.d(1) * b.d(1)).d(1)
    val postOps = if (!postInverse) (prod, c.d(2)) else (c.d(2), prod)
    if (!postMinus) (postOps._1 +^ postOps._2).d(1)
    else (postOps._1 -^ postOps._2).d(1)
    //
    //    (((a.d(1) +^ d.d(1)).d(1) * b.d(1)).d(1) +^ c.d(1)).d(1)

    //    val preAdd = if (preMinus) a -^ d else a +^ d
    //    if (!postMinus) ((preAdd.d(1) * b.d(1)).d(1) +^ c.d(2)).d(1)
    //    else ((preAdd.d(1) * b.d(1)).d(1) -^ c.d(2)).d(1)
  }

  /** a * b ± c, latency = 2, using mreg + preg */
  def abc(a: UInt, b: UInt, c: UInt, postMinus: Boolean = false) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, c.getBitsWidth, 0, isUnsigned = true, preOn = false)
    if (!postMinus) ((a * b).d(1) +^ c.d(1)).d(1)
    else ((a * b).d(1) -^ c.d(1)).d(1)
  }

  /** a * b, latency = 2, using mreg + preg */
  def ab(a: UInt, b: UInt) = {
    widthCheck(a.getBitsWidth, b.getBitsWidth, 0, 0, isUnsigned = true, preOn = false)
    (a * b).d(2)
  }

  def karaBase(aHigh: UInt, aLow: UInt, bHigh: UInt, bLow: UInt) = {
    logger.info(s"karabase width: ${aHigh.getBitsWidth},${aLow.getBitsWidth},${bHigh.getBitsWidth},${bLow.getBitsWidth}")
    require(aHigh.getBitsWidth == aLow.getBitsWidth && bHigh.getBitsWidth == bLow.getBitsWidth)
    // TODO: absorb aHigh - aLow in pre-adder, using blackbox?
    // 0-1
    val aMerge = (aHigh -^ aLow).asSInt.d(1) // outside
    val bMerge = (bHigh -^ bLow).asSInt.d(1) // outside
    // 0-2
    val high = (aHigh.intoSInt * bHigh.intoSInt).d(2) // dsp1
    val low = (aLow.intoSInt * bLow.intoSInt).d(2) // dsp2
    // 2-3, dsp0
    val mid0 = (aMerge.d(1) * bMerge.d(1)).d(1)
    // 3-4
    val mid1 = (high.d(1) - mid0).d(1) // 4-5 post-adder
    // 4-5
    val mid = (low.d(2) + mid1).d(1)
    val widthOut = aHigh.getBitsWidth + bHigh.getBitsWidth
    Seq(high.d(3).asUInt.resize(widthOut), mid.asUInt.resize(widthOut + 1), low.asUInt.d(3).resize(widthOut))
  }
}

// TODO: Merge this in MultiplicationByDspConfig
case class KaraBaseConfig() extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val Seq(aHigh, aLow, bHigh, bLow) = dataIn.asInstanceOf[Seq[BigInt]]
    Seq(aHigh * bHigh, aHigh * bLow + aLow * bHigh, aLow * bLow)
  }

  override val size = (4, 3)

  override def latency = 5

  override def implH = KaraBase(this)
}

case class KaraBase(config: KaraBaseConfig) extends TransformModule[UInt, UInt] {
  override val dataIn = slave Flow Fragment(Vec(UInt(16 bits), UInt(16 bits), UInt(24 bits), UInt(24 bits)))
  override val dataOut = master Flow Fragment(Vec(UInt(40 bits), UInt(41 bits), UInt(40 bits)))

  val Seq(aHigh, aLow, bHigh, bLow) = dataIn.fragment
  val ret = Dsp48.karaBase(aHigh, aLow, bHigh, bLow)
  dataOut.fragment := ret
  autoValid()
  autoLast()
}

object KaraBase extends App {
  val data = (0 until 10000).flatMap(_ => Seq(Random.nextBigInt(16), Random.nextBigInt(16), Random.nextBigInt(24), Random.nextBigInt(24)))
  println(s"data ${data.take(4).mkString(" ")}")
  val config = KaraBaseConfig()
  TransformTest.test(config.implH, data, name = "karabase")
  val requirement = VivadoUtilRequirement(dsp = 3)
  VivadoSynth(config.implH, "karabase").require(requirement, 800 MHz)
}