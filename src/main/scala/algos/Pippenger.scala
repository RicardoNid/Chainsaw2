package org.datenlord
package algos

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @param N       number of all scalars
 * @param W       bit width of a scalar
 * @param w       word width of a scalar
 * @param add     add operator
 * @param dbl     dbl operator
 * @param zero    zero generator
 * @param latency latency of EC adder
 */
case class Pippenger[T](N: Int, W: Int, w: Int, add: (T, T) => T, dbl: T => T, zero: T, latency: Int = 300) {

  var addCount = 0
  var dblCount = 0

  def doAdd(a: T, b: T) = {
    addCount += 1
    add(a, b)
  }

  def doDbl(a: T) = {
    dblCount += 1
    dbl(a)
  }


  def clear() = {
    addCount = 0
    dblCount = 0
  }

  def doMult(scalar: BigInt, point: T) = {
    var temp = if (scalar == 0) zero else point
    scalar.toString(2).tail.foreach { bit =>
      temp = doDbl(temp)
      if (bit == '1') temp = doAdd(temp, point)
    }
    temp
  }

  def doGolden(scalars: Seq[BigInt], points: Seq[T]) = scalars.zip(points).map { case (scalar, point) => doMult(scalar, point) }.reduce(add)

  def doPippenger(scalars: Seq[BigInt], points: Seq[T]) = {
    val golden = doGolden(scalars, points)
    clear()
    val groupsCount = (W - 1) / w + 1
    val bucketsCount = (1 << w) - 1
    val buckets = Seq.tabulate(groupsCount, bucketsCount)((_, _) => ArrayBuffer[T]())
    // distribution
    scalars.zip(points).foreach { case (scalar, point) =>
      scalar.toWords(w).zipWithIndex.filter(_._1 != 0)
        .foreach { case (word, i) => buckets(i)((word - 1).toInt) += point }
    }
    // stage1
    val bucketsSums = buckets.map(_.map(bucket => if (bucket.nonEmpty) bucket.reduce(doAdd) else zero))
    val costStage1 = addCount + dblCount
    // stage2
    val groupsSums = bucketsSums.map(group => group.zipWithIndex.map { case (bucketSum, i) => doMult(BigInt(i + 1), bucketSum) }.reduce(doAdd))
    val costStage2 = addCount + dblCount - costStage1
    // stage3
    val ret = groupsSums.reverse.reduce((high, low) => doAdd(doMult(BigInt(1) << w, high), low))
    assert(ret == golden)
    (addCount + dblCount, costStage1, costStage2)
  }

  //  def doPippengerWithBuckets: Unit ={
  //
  //    val groupCount = ceil()
  //    val bucketCount = (1 << w) - 1
  //    val c = ceil(latency.toDouble / )
  //
  //    case class Address(c:Int, g:Int, b:Int)
  //
  //    val mem = Seq.fill(W)
  //    val fifo = mutable.Queue[(BigInt, Address)](BigInt(0), )
  //    fifo.enqueue()
  //
  //    var time = 0
  //
  //
  //  }

}

object Pippenger {
  def estimateWorkload(N: Int, W: Int, w: Int) = {
    val scale = BigDecimal(1) / w
    val frac1 = (BigDecimal(2).pow(w) - 1) / BigDecimal(2).pow(w) * scale
    val frac2 = (3 * w - 4).toDouble / 2 * scale
    val costStage1 = frac1 * N * W - BigDecimal(W) / w * ((1 << w) - 1)
    val costStage2 = frac2 * BigDecimal(2).pow(w) * W
    val amount = costStage1 + costStage2
    val freq = 600 * 1000 * 1000 // 600MHz
    //    println(s"amount: ${amount / 1000000}M")
    //    val buckets = (W / w) * (1 << w - 1) + (1 << (W % w))
    //    val BRAM36 = buckets.toDouble * 377 * 2 / 36 / 1000
    //    println(s"w = $w, $buckets buckets required, $BRAM36 BRAM36 required")
    logger.info(s"when w = $w, ${amount / freq} sec needed with 1 PADD module running at 600MHz")
    //    println(s"stage1: ${stage1Cost / amount}, stage2: ${stage2Cost / amount}")
    (amount, costStage1, costStage2)
  }

}