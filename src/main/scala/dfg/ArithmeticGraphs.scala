package org.datenlord
package dfg

import scala.collection.mutable.ArrayBuffer
import arithmetic.MultplierMode._


import scala.collection.JavaConversions._

object ArithmeticGraphs {

  def addGraph(addWidth: Int, baseWidth: Int = 127) = {

    val addGolden = (data: Seq[BigInt]) => Seq(data.sum)

    implicit val graph: RingDag = new RingDag("pipelinedAdder", addGolden)
    val x = graph.addInput("bigAddX", addWidth)
    val y = graph.addInput("bigAddY", addWidth)
    val z = graph.addOutput("bigAddZ", addWidth + 1)

    val splitPoints = (0 until (addWidth - 1) / baseWidth).reverse.map(i => (i + 1) * baseWidth)
    val xs = if (splitPoints.isEmpty) Seq(x) else x.split(splitPoints).reverse // low -> high
    val ys = if (splitPoints.isEmpty) Seq(y) else y.split(splitPoints).reverse

    require(xs.forall(_.width <= baseWidth))
    require(ys.forall(_.width <= baseWidth))

    val carries = ArrayBuffer[RingPort]()
    val sums = ArrayBuffer[RingPort]()

    xs.zip(ys).foreach { case (x, y) =>
      val (carry, sum) =
        if (carries.nonEmpty) x.+<(y, carries.last)
        else x +< y
      carries += carry
      sums += sum
    }

    val ret = carries.last.merge(sums.reverse) // high -> low
    graph.addEdge(ret, z)

    graph
  }

  // this subtraction has no carry out, so make sure that when you use it for a - b, a is always greater than b
  def subGraph(addWidth: Int, baseWidth: Int = 127) = {

    val subGolden = (data: Seq[BigInt]) => Seq(data(0) - data(1))

    implicit val graph: RingDag = new RingDag("pipelinedSubtractor", subGolden)
    val x = graph.addInput("bigSubX", addWidth)
    val y = graph.addInput("bigSubY", addWidth)
    val z = graph.addOutput("bigSubZ", addWidth + 1)

    val splitPoints = (0 until (addWidth - 1) / baseWidth).reverse.map(i => (i + 1) * baseWidth)
    val xs = if (splitPoints.isEmpty) Seq(x) else x.split(splitPoints).reverse // low -> high
    val ys = if (splitPoints.isEmpty) Seq(y) else y.split(splitPoints).reverse

    val carries = ArrayBuffer[RingPort]()
    val sums = ArrayBuffer[RingPort]()

    xs.zip(ys).foreach { case (x, y) =>
      val (carry, sum) =
        if (carries.nonEmpty) x.-<(y, carries.last)
        else x -< y
      carries += carry
      sums += sum
    }

    val ret = carries.last.merge(sums.reverse)
    graph.addEdge(ret, z)
    graph
  }

  def karatsubaGraph(width: Int,  mode: MultiplierMode, noWidthGrowth: Boolean = true) = {

    val useCompressorTree = true

    val baseWidth = 32
    val stageRule = (x: Int) => if (noWidthGrowth) 2 * x else 2 * (x - 1)
    logger.info(s"stages: ${Seq.iterate(baseWidth, 10)(stageRule).mkString(" ")} ")

    def getSplit(width: Int) = {
      val bound = Seq.iterate(baseWidth, 10)(stageRule).filter(_ < width).last
      if (noWidthGrowth) bound else bound - 1
    }

    val golden = mode match {
      case FULL => (data: Seq[BigInt]) => Seq(data.product)
      case HALFLOW => (data: Seq[BigInt]) => Seq(data.product % (BigInt(1) << width))
      case SQUARE => (data: Seq[BigInt]) => Seq(data.head * data.head)
    }

    implicit val graph: RingDag = new RingDag(s"karatsubaGraph_$mode", golden)
    val x = graph.addInput("bigMultX", width)
    val y = if (mode != SQUARE) graph.addInput("bigMultY", width) else null
    val widthOut = mode match {
      case HALFLOW => width
      case _ => width * 2
    }

    def recursiveTask(width: Int, x: RingPort, y: RingPort, mode: MultiplierMode): RingPort = {

      val ret = if (width <= baseWidth) x.mult(y, mode)
      else {
        val lowWidth = getSplit(width)
        val crossWidth = lowWidth
        val highWidth = width - lowWidth
        val doubleWidth = lowWidth * 2

        require(x.width == width, s"${x.width} != $width")
        if (mode != SQUARE) require(y.width == width, s"${y.width} != $width")
        val (xHigh, xLow) = x.splitAt(lowWidth)
        val (yHigh, yLow) = if (mode != SQUARE) y.splitAt(lowWidth) else (null, null)

        mode match {
          case FULL =>
            val aPlusB = xHigh +:+^ xLow
            val cPlusD = yHigh +:+^ yLow
            val ac = recursiveTask(highWidth, xHigh, yHigh, FULL)
            val bd = recursiveTask(lowWidth, xLow, yLow, FULL)
            val all =
              if (!noWidthGrowth) recursiveTask(crossWidth + 1, aPlusB, cPlusD, FULL)
              else {
                val (abMsb, abMain) = aPlusB.splitAt(crossWidth)
                val (cdMsb, cdMain) = cPlusD.splitAt(crossWidth)

                val abMuxed = abMain muxBy cdMsb
                val cdMuxed = cdMain muxBy abMsb
                val allTop = abMsb & cdMsb

                val allMain = recursiveTask(crossWidth, abMain, cdMain, FULL)

                // combine four parts of all
                if (!useCompressorTree) {
                  val (allMainHigh, allMainLow) = allMain.splitAt(lowWidth)
                  val muxedSum = abMuxed +:+^ cdMuxed
                  val allHighTemp = muxedSum +:+^ allMainHigh
                  val (allHigh, allMid) = allHighTemp.splitAt(lowWidth)
                  (allHigh +:+^ allTop) @@ (allMid @@ allMainLow)
                }
                else allMain +^ ((abMuxed +^ cdMuxed) << lowWidth) + (allTop << doubleWidth)
              }

            if (!useCompressorTree) {
              val adbc = all -:- ac -:- bd
              val full = ac @@ bd
              val (high, low) = full.splitAt(lowWidth)
              val highSum = high +:+ adbc
              highSum @@ low
            }
            else {
              val adbc = all - ac - bd
              bd +^ (adbc << lowWidth) + (ac << doubleWidth)
            }

          case HALFLOW =>
            val bd = recursiveTask(lowWidth, xLow, yLow, FULL)
            val cb = recursiveTask(crossWidth, xLow.resize(crossWidth), yHigh.resize(crossWidth), HALFLOW)
            val ad = recursiveTask(crossWidth, xHigh.resize(crossWidth), yLow.resize(crossWidth), HALFLOW)
            if (!useCompressorTree) {
              val partial = cb.resize(crossWidth) +:+ ad.resize(crossWidth)
              if (lowWidth >= bd.width) logger.warn(s"problem: $lowWidth >= ${bd.width}")
              val (high, low) = bd.splitAt(lowWidth)
              val highSum = high +:+ partial
              highSum @@ low
            }
            else bd + ((ad + cb) << lowWidth)

          case SQUARE =>
            val bd = recursiveTask(lowWidth, xLow, xLow, SQUARE)
            val cb = recursiveTask(crossWidth, xHigh.resize(crossWidth), xLow.resize(crossWidth), FULL)
            val ac = recursiveTask(highWidth, xHigh, xHigh, SQUARE)

            if (!useCompressorTree) {
              val full = ac @@ bd
              if (lowWidth + 1 >= full.width) logger.warn(s"problem: ${lowWidth + 1} >= ${full.width}")
              val (high, low) = full.splitAt(lowWidth + 1)
              val highSum = high +:+ cb
              highSum @@ low
            }
            else bd +^ (cb << (lowWidth + 1)) + (ac << doubleWidth)
        }
      }
      ret.resize(if (mode == HALFLOW) width else width * 2)
    }

    val ret: RingPort = recursiveTask(width, x, y, mode)
    val z = graph.addOutput(s"bigMultZ_$mode", widthOut)
    graph.addEdge(ret, z)
    logger.info(s"$mode mult graph built")
    graph
  }

  // TODO: implement constant in graphs, remove input port for NPrime
  def montgomeryGraph(width: Int,  modulus: BigInt, square: Boolean = false, byLUT: Boolean = false) = {

    require(modulus.bitLength <= width)

    val R = BigInt(1) << modulus.bitLength
    val RInverse = R.modInverse(modulus)

    val montMultGolden = (data: Seq[BigInt]) => {
      val Seq(x, y, modulus, nprime) = data
      val ret = (x * y * RInverse) % modulus
      Seq(ret)
    }

    val montSquareGolden = (data: Seq[BigInt]) => {
      val Seq(x, modulus, nprime) = data
      val ret = (x * x * RInverse) % modulus
      Seq(ret)
    }

    val golden = if (square) montSquareGolden else montMultGolden

    implicit val graph: RingDag = new RingDag("MontgomeryGraph", golden)
    val x = graph.addInput("montX", width)
    val y = if (!square) graph.addInput("montY", width) else null
    val modulusInput = graph.addInput("modulus", width)
    val nprimeInput = graph.addInput("np", width)
    val z = graph.addOutput("montRet", width)

    val T = if (!square) x *:* y else x bigSquare x
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
