package org.datenlord
package algos

import breeze.numerics.ceil

case class Decomposition(shape: (Int, Int), part: Int) {

  // 2MN * 2MN tiles

  def scale: Int = shape._1 * shape._2 * part // 2MN

  def multiple = part * (part + 1) / 2 * shape._1 * shape._2

  def karaBaseCount = part * (part - 1) / 2

  // TODO: formula for pre-addition and post-addition cost

}

object KaraSearch {

  val shapes = Seq((1, 1), (2, 3), (2, 5), (3, 4), (3, 5), (4, 5))
  val parts = Seq(2, 3, 4, 5)

  val decompositions = Seq.tabulate(shapes.length, parts.length) { (i, j) => Decomposition(shapes(i), parts(j)) }.flatten


  def apply(width: Int) = {

    var bestDspCost = Int.MaxValue
    var bestPath = Seq[Decomposition]()
    var pathCount = 0
    var baseWidth = (0, 0)

    // TODO: implement LUT cost
    def searchRec(width: (Int, Int), path: Seq[Decomposition], dspCost: Int, lutCost: Int): Unit = {
      if (width._1 <= 17 && width._2 <= 26 || width._2 <= 17 && width._1 <= 26) {
        pathCount += 1
        val pass = (dspCost < bestDspCost) || ((dspCost == bestDspCost) && (path.length < bestPath.length))
        if (pass) {
          bestDspCost = dspCost
          bestPath = path
          baseWidth = width
        }
      }
      else {
        decompositions.foreach { decomp =>
          val base = ceil((width._1 max width._2).toDouble / decomp.scale).toInt
          val newWidth = (decomp.shape._1 * base, decomp.shape._2 * base)
          val newCost = dspCost * decomp.multiple
          val newPath = path :+ decomp
          searchRec(newWidth, newPath, newCost, lutCost)
        }
      }
    }

    searchRec((width, width), Seq[Decomposition](), 1, 0)
    logger.info(s"best path: ${bestPath.mkString(" ")}, baseWidth: $baseWidth")
    logger.info(s"best cost: $bestDspCost dsps")
    bestDspCost
  }

  def main(args: Array[String]): Unit = {
    //    (1 until 512).foreach { i =>
    //      println(i)
    //      KaraSearch(i)
    //    }
    KaraSearch(377)
    //    KaraSearch(1024)
  }
}

object KaratsubaAlgo {

  // TODO: DO THIS LATER AS ZPRIZE(377) NEEDS 2-PART ONLY
  def doKPart(a: BigInt, b: BigInt, width: (Int, Int), decomp: Decomposition) = {

    val base = ceil((width._1 max width._2).toDouble / decomp.scale).toInt

    val aSplitPoints = (1 until decomp.scale / decomp.shape._1).map(i => i * base * decomp.shape._1)
    val bSplitPoints = (1 until decomp.scale / decomp.shape._2).map(i => i * base * decomp.shape._2)

    val aSplits = a.split(aSplitPoints)
    val bSplits = b.split(bSplitPoints)

    Seq.tabulate(aSplits.length, bSplits.length) { (i, j) =>
      val aSplit = aSplits(i)
      val bSplit = bSplits(j)

    }

  }


}
