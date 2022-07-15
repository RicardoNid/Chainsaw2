package org.datenlord
package arithmetic

import arithmetic.MultplierMode.{Full, Low, MultiplierMode, Square}

import spinal.core._
import spinal.lib._

case class KaratsubaByGraphConfig(width: Int, mode: MultiplierMode) extends TransformBase {

  val graph = dfg.ArithmeticGraphs.karatsubaGraph(width, 0, mode).validate

  override def impl(dataIn: Seq[Any]) = {

    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.product
    mode match {
      case Full => Seq(ret)
      case Low => Seq(ret % (BigInt(1) << width))
      case Square => Seq(ret * ret)
    }
  }

  override val size = (2, 1)

  override def latency = graph.latency + 1
  //  override def latency = 27 + 1

  override def implH = KaratsubaByGraph(this)

  logger.info(s"Karatsuba multiplier latency = $latency")
}

case class KaratsubaByGraph(config: KaratsubaByGraphConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(width bits), config.size._1))
  val widthOut = mode match {
    case Low => width
    case _ => width * 2
  }
  val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), config.size._2))

  val ret = graph.evaluateH(dataIn.fragment.d(1))
  dataOut.fragment := Vec(ret)

  autoValid()
  autoLast()
}
