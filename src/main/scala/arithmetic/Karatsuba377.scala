package org.datenlord
package arithmetic

import dfg._

object Karatsuba377 {

  def apply() = {
    val golden = (data: Seq[BigInt]) => Seq(data.product)
    //    val golden = (data: Seq[BigInt]) => Seq.fill(18)(BigInt(0))
    implicit val graph: RingDag = new RingDag(s"karatsubaGraph96", golden)

    val a = graph.addInput("Mult377A", 378)
    val b = graph.addInput("Mult377B", 378)

    def rec(x: RingPort, y: RingPort, width: Int): RingPort = {
      if (width == 96) buildKara96(x, y)(graph)
      else {

        val (xHigh, xLow) = x.splitAt(width / 2)
        val (yHigh, yLow) = y.splitAt(width / 2)

        val xMerge = xHigh +:+^ xLow
        val yMerge = yHigh +:+^ yLow

        val widthNext = width / 2 + 1
        val high = rec(xHigh.resize(widthNext), yHigh.resize(widthNext), widthNext)
        val low = rec(xLow.resize(widthNext), yLow.resize(widthNext), widthNext)
        val all = rec(xMerge, yMerge, widthNext)

        val mid = all - high - low
        (high << width) +^ (mid << width / 2) +^ low
      }
    }

    val ret = rec(a, b, 378)
    val resized = ret.resize(756)
    val z = graph.addOutput(s"Mult377Z", 756)
    graph.addEdge(resized, z)
    graph.toPng("karatsuba377")
    graph
  }

  def buildKara96(x: RingPort, y: RingPort)(implicit dag: RingDag) = {
    val splitsA = (1 until 6).map(_ * 16).reverse
    val splitsB = (1 until 4).map(_ * 24).reverse

    val aWords = x.split(splitsA).reverse // low to high
    val bWords = y.split(splitsB).reverse // low to high

    val tiles = Seq.tabulate(6, 4) { (i, j) =>
      val (aWord, bWord) = (aWords(i), bWords(j))
      val (aPos, bPos) = (i * 16, j * 24)
      (aWord, bWord, aPos, bPos)
    }.flatten

    val partials = tiles
      .groupBy(tuple => tuple._3 + tuple._4).toSeq
      .filter(_._2.size == 2)
      .flatMap { case (pos, group) =>
        val Seq(up, down) = group.sortBy(_._4)
        val (aWord0, bWord0, aPos0, bPos0) = up
        val (aWord1, bWord1, aPos1, bPos1) = down
        val (aHigh, aLow, bHigh, bLow) = (aWord0, aWord1, bWord1, bWord0)
        val (high, mid, low) = aHigh.karaWith(aLow, bHigh, bLow) // high to low
        val posHigh = aPos0 + bPos1
        val posLow = aPos1 + bPos0
        Seq((high, posHigh), (mid, pos), (low, posLow))
      }

    val ret = partials.sortBy(_._2).map { case (port, shift) => port << shift }.reduce(_ +^ _)
    ret.resize(192)
  }
}
