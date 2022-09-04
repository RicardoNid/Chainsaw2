package org.datenlord
package arithmetic

import dfg.ArithInfo

import breeze.numerics._
import org.datenlord.device.TernaryAdderSignedConfig
import spinal.core._
import spinal.lib._

/** ternary adder tree for unsigned ints, subtractions are allowed, but negative result is not allowed
 *
 * @param infos
 * @param widthOut
 */
case class TernaryAdderTreeConfig(infos: Seq[ArithInfo]) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val ret = dataIn.asInstanceOf[Seq[BigInt]].zip(infos).map { case (int, info) =>
      val sign = if (info.sign) 1 else -1
      (int << info.weight) * sign
    }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  override def latency = ceil(log(3.0, infos.length.toDouble)).toInt

  override def implH = TernaryAdderTree(this)
}

case class TernaryAdderTree(config: TernaryAdderTreeConfig) extends TransformModule[SInt, SInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => SInt(info.width bits))))
  val widthOut = 1
  override val dataOut = slave Flow Fragment(Vec(SInt(widthOut bits), 1))

  var ternaryCost = 0

  val signedOperands = dataIn.fragment.zip(infos).map { case (int, info) => if (info.sign) int else -int }
  val operands: Seq[(SInt, ArithInfo)] = dataIn.fragment.zip(infos).sortBy(_._2.weight)

  def rec(operands: Seq[(SInt, ArithInfo)]) = {

    val groups = operands.grouped(3).toSeq
    val init = groups.init.map { group =>
      val Seq(a, b, c) = group
      val baseShift = group.map(_._2.weight).min
      val adderWidth = group.map(_._2).map(info => info.width + info.weight - baseShift).max + 1
      val config = TernaryAdderSignedConfig(adderWidth)
      val ret = config.implH.asFunc(group.map{ case (int, info) => int << info.weight - baseShift})
    }


  }


}