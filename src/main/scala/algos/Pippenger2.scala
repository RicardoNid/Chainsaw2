package org.datenlord
package algos

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Pippenger2[T](N: Int, W: Int, w: Int, add: (T, T) => T, dbl: T => T, zero: T) {

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
}

object Pippenger2 {
  def main(args: Array[String]): Unit = {
    val W = 253
    val add = (a: BigInt, b: BigInt) => a + b
    val dbl = (a: BigInt) => 2 * a

    //    Seq.tabulate(1, 1) { (a, b) =>
    //      val N = (a + 1) * 100000
    //      val w = b + 12
    //      val scalars = (0 until N).map(_ => Random.nextBigInt(253))
    //      val points = (0 until N).map(_ => Random.nextBigInt(8))
    //      val cost = Pippenger2(N, W, w, add, dbl, BigInt(0)).doPippenger(scalars, points)
    //      val costEstimated = ZPrizeMSM.estimateWorkload(N, W, w)
    //      val err1 = (costEstimated._1 - cost._1).abs.toDouble / cost._1
    //      val err2 = (costEstimated._2 - cost._2).abs.toDouble / cost._2
    //      val err3 = (costEstimated._3 - cost._3).abs.toDouble / cost._3
    //
    //      println(s"err      : $err1, stage1: $err2, stage2: $err3")
    //      println(s"estimated: ${costEstimated._1}, stage1: ${costEstimated._2}, stage2: ${costEstimated._3}")
    //      println(s"actual   : ${cost._1}, stage1: ${cost._2}, stage2: ${cost._3}")
    //    }

    println(ZPrizeMSM.estimateWorkload(1 << 26, 253, 10))
    println(ZPrizeMSM.estimateWorkload(1 << 26, 253, 12))
  }
}
