package org.datenlord
package dataFlow

import dataFlow.StreamPermutationConfig.getDecomposition

import breeze.linalg._
import scala.collection.mutable
import StreamPermutationConfig._

import scala.collection.mutable.ArrayBuffer

/**
 * @param permutation
 * @param streamWidth
 */
case class StreamPermutationConfig(permutation: Array[Int], streamWidth: Int) {

  val n = permutation.length
  val w = streamWidth

  val mappingMatrixContent = Array.tabulate(streamWidth, streamWidth) { (i, j) =>
    permutation.zipWithIndex.count { case (out, in) =>
      (out % streamWidth == j) && (in % streamWidth == i)
    }
  }

  val mappingMatrix = new DenseMatrix(streamWidth, streamWidth, mappingMatrixContent.flatten)
  val permutationsAndTimes = getDecomposition(mappingMatrix)

  val permutations = ArrayBuffer[Seq[Int]]()
  val readAddr = ArrayBuffer[Seq[Int]]() // read addr of M_0
  val writeAddr = ArrayBuffer[Seq[Int]]() // write addr of M_1

  val elems = mutable.Set(0 until n: _*)
  permutationsAndTimes.foreach { case (perm, time) =>
    val elem = (0 until w).map(i => elems.find(x => (x % w == i) && (permutation(x) % w == perm(i))).get)
    elems --= elem
    permutations ++= Array.fill(time)(perm)
    readAddr ++= Array.fill(time)(elem.map(_ / streamWidth))
    writeAddr ++= Array.fill(time)(elem.map(permutation(_) / streamWidth))
  }

  def perm(data: Array[Seq[Int]], perms: ArrayBuffer[Seq[Int]]) = {
    data.zip(perms).map { case (d, perm) => (0 until streamWidth).map(i => d(perm(i))) }
  }

  def readByAddr(data: Array[Seq[Int]], readAddr: ArrayBuffer[Seq[Int]]) = {
    readAddr.zipWithIndex.map{ case (addrs, cycle) => addrs.map(addr => data(cycle)(addr))}
  }

  def writeByAddr(data: Array[Int], writeAddr: ArrayBuffer[Seq[Int]]) = {

  }

  def sim() = {
    val data = (0 until n).toArray

  }

}

object StreamPermutationConfig {
  def main(args: Array[String]): Unit = {

    val perm = Array(3, 7, 1, 2, 6, 0, 11, 9, 4, 10, 8, 5)
    StreamPermutationConfig(perm, 3)

    val testCases = (0 until 100).map(_ => scala.util.Random.shuffle((0 until 12).toList).toArray)
    testCases.foreach(testCase => StreamPermutationConfig(testCase, 3))
  }

  def perm2Matrix(perm: Seq[Int]) = {
    val n = perm.length
    val content = Array.tabulate(n, n)((i, j) => if (perm(i) == j) 1 else 0)
    new DenseMatrix(n, n, content.flatten)
  }

  // brute force algo for decomposition
  def getDecomposition(array: DenseMatrix[Int]) = {
    require(array.rows == array.cols)
    val n = array.rows

    def perms(n: Int) = (0 until n).permutations

    var current = array
    val ret = ArrayBuffer[(Seq[Int], Int)]()

    perms(n).foreach { perm =>
      var times = 0
      val permMatrix = perm2Matrix(perm)
      while ((current - permMatrix).forall(_ >= 0)) {
        current = current - permMatrix
        times += 1
      }
      if (times > 0) ret += ((perm, times))
    }

    assert(current.forall(_ == 0), s"brute force decomposition failed")
    ret.toArray
  }
}
