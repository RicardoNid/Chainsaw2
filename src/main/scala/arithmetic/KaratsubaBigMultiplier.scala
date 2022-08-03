package org.datenlord
package arithmetic

import device.MultiplicationByDspConfig

import spinal.core._
import spinal.lib._

import scala.language.postfixOps



import arithmetic.MultplierMode._

// TODO: implement low + constant
case class KaratsubaConfig(width: Int, mode: MultiplierMode, constant: BigInt = null, byDsp: Boolean = true) extends TransformBase {

  override def impl(dataIn: Seq[Any]) = {

    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = if (constant == null) bigInts.product else bigInts.head * constant
    mode match {
      case Full => Seq(ret)
      case Low => Seq(ret % (BigInt(1) << width))
      case Square => Seq(ret * ret)
    }
  }

  override val size = mode match {
    case Full => if (constant == null) (2, 1) else (1, 1)
    case Low => if (constant == null) (2, 1) else (1, 1)
    case Square => (1, 1)
  }

  val baseWidth = if (byDsp) 34 else 31

  val postAddLatencies = Seq(11, 7, 4, 3)
  val preAddLatencies = Seq(2, 1, 1, 1)

  val addLatency = 4 // latency for additions(pre and post stage) in each layer
  val multLatency = MultiplicationByDspConfig(mode).latency // keep same latency for different implementation

  def getNearestSplit(width: Int) = {
    var current = baseWidth

    def next = (current - 1) * 2

    while (next < width) current = next
    current
  }

  override def latency = {
    val limit = log2Up(width) * 2
    // layers of recursion
    val layers = Array.iterate(width, limit)(width => (width + 1) / 2 + 1).count(_ > baseWidth) + 1
    multLatency + (layers - 1) * addLatency + 2
    //    multLatency + (0 until layers - 1).map(i => postAddLatencies(i) + preAddLatencies(i)).sum + 2
  }

  override def implH = KaratsubaBigMultiplier(this)

  logger.info(s"Karatsuba multiplier latency = $latency")
}

case class KaratsubaBigMultiplier(config: KaratsubaConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  var multTimes = 0

  val dataIn = slave Flow Fragment(Vec(UInt(width bits), config.size._1))

  val widthOut = mode match {
    case Low => width
    case _ => width * 2
  }

  val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), config.size._2))

  val x = dataIn.fragment.head.d(1)
  val y = (if (constant == null) dataIn.fragment.last else U(constant).resize(width)).d(1)

  def recursiveTask(width: Int, x: UInt, y: UInt, c: BigInt, mode: MultiplierMode, layer: Int): UInt = {

    val multByLut = (x: UInt, y: UInt) => {
      val product = x * y
      product.addAttribute("use_dsp", "no")
      product.d(multLatency)
    }
    val ret = if (width <= baseWidth) {
      val baseMult = if (byDsp) MultiplicationByDspConfig(mode).asOperator
      else multByLut
      if (constant != null) println(s"$c -> ${c.toString(2).count(_ == '1')}")
      multTimes += 1
      baseMult(x, y)
    } else {
      // split x,y into high & low, according to a multiple of base width
      val split = (width + 1) / 2
      //      val split = getNearestSplit(width)
      val widthHigh = width - split
      val widthLow = split
      val widthCorss = widthHigh max widthLow

      val (xHighBits, xLowBits) = x.splitAt(split)
      val (yHighBits, yLowBits) = y.splitAt(split)
      val Seq(xHigh, xLow, yHigh, yLow) = Seq(xHighBits, xLowBits, yHighBits, yLowBits).map(_.asUInt)

      val (cHigh, cLow) = BigIntUtil(c).split(split)

      mode match {
        case Full =>
          val aPlusB = (xHigh +^ xLow).d(1)
          val cPlusD = (yHigh +^ yLow).d(1)

          val ac = recursiveTask(widthHigh, xHigh, yHigh, cHigh, Full, layer + 1)
          val bd = recursiveTask(widthLow, xLow, yLow, cLow, Full, layer + 1)
          val all = recursiveTask(widthCorss + 1, aPlusB, cPlusD, cHigh + cLow, Full, layer + 1)

          val partial = (all - ac.d(1)).d(1)
          val adbc = (partial - bd.d(2)).d(1)
          ((ac.d(3) @@ bd.d(3)) + (adbc << split)).d(1)
        case Low =>
          val bd = recursiveTask(widthLow, xLow, yLow, cLow, Full, layer + 1)
          val cb = recursiveTask(widthCorss, xLow, yHigh, cHigh, Low, layer + 1)
          val ad = recursiveTask(widthCorss, xHigh, yLow, cLow, Low, layer + 1)
          val partial = (cb.takeLow(widthCorss).asUInt + ad.takeLow(widthCorss).asUInt).d(1)
          val ret = ((partial << split) + bd.d(1)).d(1)
          ret.d(2)
        case Square =>
          val bd = recursiveTask(widthLow, xLow, xLow, cLow, Square, layer + 1)
          val cb = recursiveTask(widthCorss, xHigh, xLow, cLow, Full, layer + 1)
          val ac = recursiveTask(widthHigh, xHigh, xHigh, cLow, Square, layer + 1)
          val ret = ((ac @@ bd) + (cb << (split + 1))).d(1)
          ret.d(3)
      }
    }
    ret.resize(width * 2)
  }

  val c = if (constant == null) BigInt(0) else constant
  dataOut.fragment := Vec(recursiveTask(width, x, y, c, mode, 1).d(1).resized)
  logger.info(s"multTimes = $multTimes")
  autoValid()
  autoLast()
}