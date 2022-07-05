package org.datenlord
package dfg

import scala.collection.mutable.ArrayBuffer
import arithmetic.MultplierMode._

object ArithmeticGraphs {

  def addGraph(addWidth: Int, baseWidth: Int = 127) = {

    implicit val graph: RingDag = new RingDag
    val x = graph.setInput("x", addWidth)
    val y = graph.setInput("y", addWidth)
    val z = graph.setOutput("z", addWidth + 1)

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
  def subGraph(addWidth: Int, baseWidth: Int = 127) = {

    implicit val graph: RingDag = new RingDag
    val x = graph.setInput("x", addWidth)
    val y = graph.setInput("y", addWidth)
    val z = graph.setOutput("z", addWidth)

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

    val ret = if(splitPoints.isEmpty) sums.last else sums.last.merge(sums.init.reverse) // high -> low
    graph.addEdge(ret, z)

    graph
  }

  def karatsubaGraph(width: Int, mode: MultiplierMode) = {

    val stages = Seq(32, 62, 121, 240)
    // TODO: achieve the following stages by reimplement full mode
    //    val stages = Seq(34, 66, 130, 258)

    implicit val graph: RingDag = new RingDag
    val x = graph.setInput("x", width)
    val y = graph.setInput("y", width)
    val widthOut = mode match {
      case Low => width
      case _ => width * 2
    }
    val z = graph.setOutput("z", widthOut)

    def recursiveTask(width: Int, x: RingPort, y: RingPort, mode: MultiplierMode): RingPort = {

      val baseWidth = mode match {
        case Full => 32
        case Low => 34
        case Square => 34
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
          case Full =>
            val aPlusB = xHigh +:+^ xLow
            val cPlusD = yHigh +:+^ yLow

            val ac = recursiveTask(widthHigh, xHigh, yHigh, Full)
            val bd = recursiveTask(widthLow, xLow, yLow, Full)
            val all = recursiveTask(widthCross + 1, aPlusB, cPlusD, Full)

            val partial = all -:- ac
            val adbc = partial -:- bd

            val full = ac @@ bd
            if (split >= full.width) logger.warn(s"problem: $split >= ${full.width}")
            val (high, low) = full.splitAt(split)
            val highSum = high +:+ adbc
            highSum @@ low

          case Low =>
            val bd = recursiveTask(widthLow, xLow, yLow, Full)
            val cb = recursiveTask(widthCross, xLow.resize(widthCross), yHigh.resize(widthCross), Low)
            val ad = recursiveTask(widthCross, xHigh.resize(widthCross), yLow.resize(widthCross), Low)
            val partial = cb.resize(widthCross) +:+ ad.resize(widthCross)
            if (split >= bd.width) logger.warn(s"problem: $split >= ${bd.width}")
            val (high, low) = bd.splitAt(split)
            val highSum = high +:+ partial
            highSum @@ low

          case Square =>
            val bd = recursiveTask(widthLow, xLow, xLow, Square)
            val cb = recursiveTask(widthCross, xHigh.resize(widthCross), xLow.resize(widthCross), Full)
            val ac = recursiveTask(widthHigh, xHigh, xHigh, Square)

            val full = ac @@ bd
            if (split + 1 >= full.width) logger.warn(s"problem: ${split + 1} >= ${full.width}")
            val (high, low) = full.splitAt(split + 1)
            val highSum = high +:+ cb
            highSum @@ low
        }
      }
      ret.resize(if (mode == Low) width else width * 2)
    }

    val ret: RingPort = recursiveTask(width, x, y, mode)
    graph.addEdge(ret, z)
    logger.info(s"graph built")
    graph
  }

}
