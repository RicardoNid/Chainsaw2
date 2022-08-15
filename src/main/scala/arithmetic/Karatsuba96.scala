package org.datenlord
package arithmetic

import dfg._

object Karatsuba96 {

  def apply() = {
    val golden = (data: Seq[BigInt]) => Seq(data.product)
    //    val golden = (data: Seq[BigInt]) => Seq.fill(18)(BigInt(0))
    implicit val graph: RingDag = new RingDag(s"karatsubaGraph96", golden)

    val a = graph.addInput("Mult96A", 96)
    val b = graph.addInput("Mult96B", 96)

    val splitsA = (1 until 6).map(_ * 16).reverse
    val splitsB = (1 until 4).map(_ * 24).reverse

    val aWords = a.split(splitsA).reverse // low to high
    val bWords = b.split(splitsB).reverse // low to high

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
        logger.info(s"pos $posHigh, $pos, $posLow")
        assert(posHigh - pos == 48 && pos - posLow == 48)
        Seq((high, posHigh), (mid, pos), (low, posLow))
      }

    val ret = partials.sortBy(_._2).map { case (port, shift) => port << shift }.reduce(_ +^ _)
    val resized = ret.resize(192)

    val z = graph.addOutput(s"Mult96Z", 192)
    graph.addEdge(resized, z)

    logger.info(graph.toString)
    graph.toPng("kara96_before")
    logger.info(s"karatsuba96 graph built")
    graph
  }
}
