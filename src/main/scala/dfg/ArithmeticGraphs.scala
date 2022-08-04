package org.datenlord
package dfg

import scala.collection.mutable.ArrayBuffer
import arithmetic.MultplierMode._


import scala.collection.JavaConversions._

object ArithmeticGraphs {

  def addGraph(addWidth: Int, shift: Int, baseWidth: Int = 127) = {

    implicit val graph: RingDag = new RingDag
    val x = graph.addInput("addX", ArithInfo(addWidth, shift))
    val y = graph.addInput("addY", ArithInfo(addWidth, shift))
    val z = graph.addOutput("addZ", ArithInfo(addWidth + 1, shift))

    val splitPoints = (0 until (addWidth - 1) / baseWidth).reverse.map(i => (i + 1) * baseWidth)
    val xs = if (splitPoints.isEmpty) Seq(x) else x.split(splitPoints).reverse // low -> high
    val ys = if (splitPoints.isEmpty) Seq(y) else y.split(splitPoints).reverse

    val carries = ArrayBuffer[RingPort]()
    val sums = ArrayBuffer[RingPort]()

    xs.zip(ys).foreach { case (x, y) =>
      val (carry, sum) =
        if (carries.nonEmpty) x.+^(y, carries.last)
        else x +^ y
      carries += carry
      sums += sum
    }

    val ret = carries.last.merge(sums.reverse) // high -> low
    graph.addEdge(ret, z)

    graph
  }

  // this subtraction has no carry out, so make sure that when you use it for a - b, a is always greater than b
  def subGraph(addWidth: Int, shift: Int, baseWidth: Int = 127) = {

    implicit val graph: RingDag = new RingDag
    val x = graph.addInput("bigSubX", ArithInfo(addWidth, shift))
    val y = graph.addInput("bigSubY", ArithInfo(addWidth, shift))
    val z = graph.addOutput("bigSubZ", ArithInfo(addWidth + 1, shift))

    val splitPoints = (0 until (addWidth - 1) / baseWidth).reverse.map(i => (i + 1) * baseWidth)
    val xs = if (splitPoints.isEmpty) Seq(x) else x.split(splitPoints).reverse // low -> high
    val ys = if (splitPoints.isEmpty) Seq(y) else y.split(splitPoints).reverse

    val carries = ArrayBuffer[RingPort]()
    val sums = ArrayBuffer[RingPort]()

    xs.zip(ys).foreach { case (x, y) =>
      val (carry, sum) =
        if (carries.nonEmpty) x.-^(y, carries.last)
        else x -^ y
      carries += carry
      sums += sum
    }

    val ret = carries.last.merge(sums.reverse)
    graph.addEdge(ret, z)
    graph
  }

  def karatsubaGraph(width: Int, shift: Int, mode: MultiplierMode) = {

    val stages = Seq(32, 62, 121, 240)
    // TODO: achieve the following stages by reimplement full mode
    //    val stages = Seq(34, 66, 130, 258)

    implicit val graph: RingDag = new RingDag
    val x = graph.addInput(s"bigMultX", ArithInfo(width, shift))
    val y = graph.addInput("bigMultY", ArithInfo(width, shift))
    val widthOut = mode match {
      case HALF => width
      case _ => width * 2
    }
    val z = graph.addOutput(s"bigMultZ_$mode", ArithInfo(widthOut, shift))

    def recursiveTask(width: Int, x: RingPort, y: RingPort, mode: MultiplierMode): RingPort = {

      val baseWidth = mode match {
        case FULL => 32
        case HALF => 34
        case SQUARE => 34
      }

      val ret = if (width <= baseWidth) {
        x.multByMode(y, mode)
      }
      else {
        val split = stages.filter(_ < width).max - 1

        require(x.width == width, s"${x.width} != $width")
        require(y.width == width, s"${y.width} != $width")

        val widthHigh = width - split
        val widthLow = split
        require(widthLow >= widthHigh) // or, you cannot recursively call low-bit sub-problems
        val widthCross = widthHigh max widthLow

        //        println(s"$width -> $widthHigh, $widthCross, $widthLow")

        val Seq(xHigh, xLow) = x.split(Seq(split))
        val Seq(yHigh, yLow) = y.split(Seq(split))

        mode match {
          case FULL =>
            val aPlusB = xHigh +:+^ xLow
            val cPlusD = yHigh +:+^ yLow
            logger.info(s"high ${xHigh.shift}, low: ${xLow.shift}")

            val ac = recursiveTask(widthHigh, xHigh, yHigh, FULL)
            val bd = recursiveTask(widthLow, xLow, yLow, FULL)
            val all = recursiveTask(widthCross + 1, aPlusB, cPlusD, FULL)

            val partial = all -:- ac
            val adbc = partial -:- bd

            val full = ac @@ bd
            if (split >= full.width) logger.warn(s"problem: $split >= ${full.width}")
            val (high, low) = full.splitAt(split)
            val highSum = high +:+ adbc
            highSum @@ low

          case HALF =>
            val bd = recursiveTask(widthLow, xLow, yLow, FULL)
            val cb = recursiveTask(widthCross, xLow.resize(widthCross), yHigh.resize(widthCross), HALF)
            val ad = recursiveTask(widthCross, xHigh.resize(widthCross), yLow.resize(widthCross), HALF)
            val partial = cb.resize(widthCross) +:+ ad.resize(widthCross)
            if (split >= bd.width) logger.warn(s"problem: $split >= ${bd.width}")
            val (high, low) = bd.splitAt(split)
            val highSum = high +:+ partial
            highSum @@ low

          case SQUARE =>
            val bd = recursiveTask(widthLow, xLow, xLow, SQUARE)
            val cb = recursiveTask(widthCross, xHigh.resize(widthCross), xLow.resize(widthCross), FULL)
            val ac = recursiveTask(widthHigh, xHigh, xHigh, SQUARE)

            val full = ac @@ bd
            if (split + 1 >= full.width) logger.warn(s"problem: ${split + 1} >= ${full.width}")
            val (high, low) = full.splitAt(split + 1)
            val highSum = high +:+ cb
            highSum @@ low
        }
      }
      ret.resize(if (mode == HALF) width else width * 2)
    }

    val ret: RingPort = recursiveTask(width, x, y, mode)
    graph.addEdge(ret, z)
    logger.info(s"$mode mult graph built")
    graph
  }

  def montgomeryGraph(width: Int, shift: Int, modulus: BigInt, square: Boolean = false, byLUT: Boolean = false) = {

    require(modulus.bitLength <= width)

    implicit val graph: RingDag = new RingDag
    val x = graph.addInput("montX", ArithInfo(width, shift))
    val y = graph.addInput("montY", ArithInfo(width, shift))
    val modulusInput = graph.addInput("modulus", ArithInfo(width, shift))
    val nprimeInput = graph.addInput("np", ArithInfo(width, shift))
    val z = graph.addOutput("montRet", ArithInfo(width + 1, shift))

    val T = if (!square) x *:* y else x bigSquare y
    val TLow = T.resize(width)

    val m = TLow *%:*% nprimeInput
    val prod = m *:* modulusInput
    val full = prod +:+^ T
    val t = full.splitAt(width)._1


    graph.addEdge(t, z)
    logger.info(s"mont graph built")
    println(graph.vertexSet().filter(_.inDegree == 0).mkString(" "))
    graph
  }

}
