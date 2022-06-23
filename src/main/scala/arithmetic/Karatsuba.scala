package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class KaratsubaConfig(width: Int, baseWidth: Int = 34,
                           baseMult: (UInt, UInt) => UInt, baseMultLatency: Int,
                           mode: String = "mult", constant: BigInt = null) extends TransformBase {

  override def impl(dataIn: Seq[Any]) = {

    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    mode match {
      case "mult" => Seq(bigInts.product)
      case "square" => Seq(bigInts.head * bigInts.head)
      case "low" => Seq(bigInts.product % (BigInt(1) << width))
      case "constant" => Seq(bigInts.head * constant)
    }
  }

  override val size = mode match {
    case "mult" => (2, 1)
    case "square" => (1, 1)
    case "low" => (2, 1)
    case "constant" => (1, 1)
  }

  // TODO: not sure
  override def latency = {
    val limit = log2Up(width) * 2
    // layers of recursion
    val layers = Array.iterate(width, limit)(width => (width + 1) / 2 + 1).count(_ > baseWidth) + 1
    // latency for addition in each layer
    //    val additionLatency = 3
    val additionLatency = 0
    baseMultLatency + (layers - 1) * additionLatency
  }

  override def implH = KaratsubaBigMultiplier(this)
}

case class KaratsubaBigMultiplier(config: KaratsubaConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  var multTimes = 0

  def doBaseMult(a: UInt, b: UInt) = {
    multTimes += 1
    //    println(multTimes)
    baseMult(a, b)
  }

  val dataIn = slave Flow Fragment(Vec(UInt(width bits), config.size._1))
  val widthOut = if(mode == "low") width else width * 2
  val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), config.size._2))

  val x = dataIn.fragment.head
  val y = mode match {
    case "mult" => dataIn.fragment.last
    case "square" => x
    case "low" => dataIn.fragment.last
    case "constant" => U(constant).resize(width)
  }

  def recursiveTask(width: Int, x: UInt, y: UInt, mode: String): UInt = {
    if (width <= baseWidth) doBaseMult(x, y)
    else {
      // split x,y into high & low, according to a multiple of basewidth
      val split = (width + 1) / 2
      val widthNext = split + 1
      val (xHighBits, xLowBits) = x.splitAt(split)
      val (yHighBits, yLowBits) = y.splitAt(split)
      val Seq(xHigh, xLow, yHigh, yLow) = Seq(xHighBits, xLowBits, yHighBits, yLowBits).map(_.asUInt)

      // TODO: implement pipelining for all modes

      //      val aPlusB = (xHigh +^ xLow).d(1)
      //      val cPlusD = (yHigh +^ yLow).d(1)
      //
      //      val ac = recursiveTask(widthNext, xHigh, yHigh)
      //      val bd = recursiveTask(widthNext, xLow, yLow)
      //      val all = recursiveTask(widthNext, aPlusB, cPlusD)
      //
      //      bd.setName(s"bd_${bd.getWidth}", weak = true)
      //      all.setName(s"all_${all.getWidth}", weak = true)
      //      ac.setName(s"ac_${ac.getWidth}", weak = true)
      //
      //      val adbc = (all -^ ac.d(1) -^ bd.d(1)).d(1)
      //
      //      val ret = (ac.d(2) << (split * 2)) +^ ((adbc << split) +^ bd.d(2))
      //      ret.d(1)

      val ret = if (mode == "mult" || mode == "constant") {
        val aPlusB = xHigh +^ xLow
        val cPlusD = yHigh +^ yLow
        val ac = recursiveTask(widthNext, xHigh, yHigh, "mult")
        val bd = recursiveTask(widthNext, xLow, yLow, "mult")
        val all = recursiveTask(widthNext, aPlusB, cPlusD, "mult")
        val adbc = all -^ ac -^ bd
        (ac << (split * 2)) +^ ((adbc << split) +^ bd)
      } else if (mode == "square") {
        val bd = recursiveTask(widthNext, xLow, xLow, "square")
        val cb = recursiveTask(widthNext, xHigh, xLow, "mult")
        val ac = recursiveTask(widthNext, xHigh, xHigh, "square")
        (ac << (split * 2)) +^ (cb << (split + 1)) +^ bd
      } else { // mode == "low"
        val bd = recursiveTask(widthNext, xLow, yLow, "mult")
        val cb = recursiveTask(widthNext, yHigh, xLow, "low")
        val ad = recursiveTask(widthNext, xHigh, yLow, "low")
        ((cb +^ ad) << split) +^ bd
      }

      ret
    }
  }

  dataOut.fragment := Vec(recursiveTask(width, x, y, mode).resized)
  autoValid()
  autoLast()
}