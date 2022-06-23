package org.datenlord
package algos

import scala.util.Random

case class Performance(add: Int, dbl: Int, storage: Double) {
  val dblFactor = 1
  val operationCost = add + dbl * 0.7
  val storageCost = storage
  val ratio = dbl / add.toDouble
}

case class PippengerProblem[T](scalars: Seq[BigInt], points: Seq[T], add: (T, T) => T, dbl: T => T, zero: T) {

  var addCount, dblCount = 0

  def clear() = {
    addCount = 0
    dblCount = 0
  }

  def doAdd(a: T, b: T) = {
    addCount += 1;
    add(a, b)
  }

  def doDbl(a: T) = {
    dblCount += 1;
    dbl(a)
  }

  def mult(scalar: BigInt, point: T) = {
    var temp = if (scalar == 0) zero else point
    scalar.toString(2).tail.foreach { bit =>
      temp = doDbl(temp)
      if (bit == '1') temp = doAdd(temp, point)
    }
    temp
  }

  def solveByOriginal = {

    val result = scalars.zip(points).map { case (scalar, point) => mult(scalar, point) }.reduce(doAdd)

    val ret = (result, Performance(addCount, dblCount, 1))
    clear()
    ret
  }

  def solveByPippenger(bitWindow: Int, pointWindow: Int, lookup: Int) = {

    val scalarBitLength = scalars.map(_.bitLength).max
    val bitBatchCount = (scalarBitLength + bitWindow - 1) / bitWindow
    val pointBatchCount = (points.length + pointWindow - 1) / pointWindow

    val GMatrix = Seq.tabulate(pointBatchCount, bitBatchCount) { (pointBatchIndex, bitBatchIndex) =>
      val (pointStart, pointEnd) = (pointWindow * pointBatchIndex, pointWindow * (pointBatchIndex + 1))
      val (bitStart, bitEnd) = (bitWindow * bitBatchIndex, bitWindow * (bitBatchIndex + 1))

      val pointsInBatch = points.slice(pointStart, pointEnd)
      val scalarsInBatch = scalars.slice(pointStart, pointEnd)
      val slicesInBatch = scalarsInBatch.map(_.toString(2).reverse.padTo(scalarBitLength, '0').slice(bitStart, bitEnd).reverse).map(BigInt(_, 2))

      val Bs =
        slicesInBatch.zip(pointsInBatch)
          .groupBy(_._1).filter(_._1 != 0)
          .map { case (slice, seq) =>
            val B = seq.map(_._2)
              .grouped(lookup).toSeq
              .map(_.reduce(add))
              .reduce(doAdd)
            mult(slice, B)
          }
      val G = (Bs.toSeq :+ zero).reduce(doAdd) // in case of empty
      G
    }

    val result = GMatrix.map { rowOfGs =>
      var temp = rowOfGs.last // G_max
      rowOfGs.reverse.tail.foreach { next =>
        (0 until bitWindow).foreach(_ => temp = doDbl(temp))
        temp = doAdd(temp, next)
      }
      temp
    }.reduce(doAdd)

    val storage = if (lookup == 1) 1 else (1 << lookup).toDouble / lookup

    val ret = (result, Performance(addCount, dblCount, storage))
    clear()
    ret
  }

  val base = solveByOriginal

  def testWindow(bitWindow: Int, pointWindow: Int, lookup: Int) = {
    val solution = solveByPippenger(bitWindow, pointWindow, lookup)
    val cost = solution._2.operationCost / base._2.operationCost
    val storage = solution._2.storage
    val ratio = solution._2.ratio
    println(s"$bitWindow, $pointWindow, $lookup -> cost: $cost, storage: $storage, ratio: $ratio")
    assert(solution._1 == base._1, s"${solution._1}, ${base._1}")
    (bitWindow, pointWindow, lookup, cost, ratio)
  }
}

object PippengerProblem {
  def main(args: Array[String]): Unit = {

    val add = (a: BigInt, b: BigInt) => a + b
    val dbl = (a: BigInt) => a * 2

    def search() = {
      val randBig: Seq[BigInt] = (0 until 1000).map(_ => Random.nextBigInt(10))
      val randScalar: Seq[BigInt] = (0 until 1000).map(_ => Random.nextBigInt(253) % MSM.scalarModulus)
      val problem = PippengerProblem(randScalar, randBig, add, dbl, BigInt(0))
      val bitCandidate = 1 to 8
      val pointCandidate = (1 to 10).map(_ * 100)
      val lookupCandidates = 4 to 4

      val notByLut = Seq.tabulate(bitCandidate.length, pointCandidate.length, lookupCandidates.length) {
        (i, j, k) =>
          val bit = bitCandidate(i)
          val point = pointCandidate(j)
          val lut = lookupCandidates(k)
          problem.testWindow(bit, point, lut)
      }.flatten.flatten

      println(notByLut.minBy(_._4))
    }

    def evaluate(size: Int, lut: Int) = {
      val randBig: Seq[BigInt] = (0 until size).map(_ => Random.nextBigInt(10))
      val randScalar: Seq[BigInt] = (0 until size).map(_ => Random.nextBigInt(253) % MSM.scalarModulus)
      val problem = PippengerProblem(randScalar, randBig, add, dbl, BigInt(0))
      problem.testWindow(3, size, lut)
    }

    //    evaluate(1 << 10, 1)
    //    evaluate(1 << 14, 10)
    search()
  }
}
