package org.datenlord
package algos

// TODO: cost & count for different Karatsuba Mode
case class Decomposition(split: Int, row: Int, col: Int) {

  val w = gcd(row, col)
  val a = row / w
  val b = col / w
  val karaCount = split * (split - 1) / 2 * a * b // number of KaraBase Structure

  val compressorEff = 1.0

  def widthNext = split * a * b * w

  def *(split: Int) = Decomposition(split, widthNext, widthNext)

  def subMultCount = split * (split + 1) / 2 * a * b

  def splitCost = karaCount * (row + col)

  def mergeCost = {
    val minusCost = karaCount * 2 * (row + col) // reduce high & low
    val plusCost = (subMultCount - karaCount) * (row + col) + karaCount * (row + col + 2) - (widthNext * 2) // reduce = all - remained
    (minusCost + plusCost) / compressorEff
  }

  def clbCost = splitCost + mergeCost
}

case class KaraSolution(dsp: (Int, Int), splits: Seq[Int]) {

  val head: Decomposition = Decomposition(splits.head, row = dsp._1, col = dsp._2)

  val all: Seq[Decomposition] = Seq.iterate((head, 0), splits.length) { case (decomposition, i) => (decomposition * splits(i + 1), i + 1) }
    .map(_._1)

  val layer = splits.length

  def width = all.last.widthNext

  def dspCost = all.map(_.subMultCount).product

  def splitCost = all.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.splitCost
  }.sum

  def mergeCost = all.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.mergeCost
  }.sum

  def clbCost = all.tails.toSeq.init.map { chain =>
    val multiple = chain.tail.map(_.subMultCount).product
    multiple * chain.head.clbCost
  }.sum

  def showCosts = {
    println("----Karatsuba report----")
    println(s"layer = $layer")
    println(s"width = $width")
    println(s"dspCost = $dspCost")
    println(s"splitCost = $splitCost")
    println(s"mergeCost = $mergeCost")
    println(s"clbCost = $clbCost")
  }

}

object KaraSolution extends App {
  val kara64A = KaraSolution((16, 16), Seq(2, 2))
  val kara64B = KaraSolution((16, 16), Seq(4))

  val kara377A = KaraSolution((16, 24), Seq(2, 2, 2))
  val kara377B = KaraSolution((16, 16), Seq(2, 2, 2, 3))
  val kara377C = KaraSolution((16, 16), Seq(3, 2, 2, 2))
  val kara377D = KaraSolution((16, 24), Seq(2,4))

  val kara256 = KaraSolution((16, 16), Seq(2,2,2,2))
  val kara1024 = KaraSolution((16, 16), Seq(8,8))

  kara64A.showCosts
  kara64B.showCosts
  kara377A.showCosts
  kara377B.showCosts
  kara377C.showCosts
  kara377D.showCosts
  kara256.showCosts
  kara1024.showCosts
}