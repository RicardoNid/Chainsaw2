package org.datenlord
package dataFlow

import breeze.linalg._

object SpatialPermutation { // permutation implemented by hardwired

  def apply[T](dataIn: Seq[T], perm: Seq[Int]): Seq[T] = {
    require(dataIn.length == perm.length)
    perm.map(i => dataIn(i))
  }

  def apply[T](dataIn: Seq[T], perm: DenseMatrix[Int]): Seq[T] = {
    val N = perm.rows
    val indices = (0 until N).map(i => perm(i, ::).t.toArray.indexWhere(_ == 1))
    indices.map(i => dataIn(i))
  }

  def main(args: Array[String]): Unit = { // example
    val perm = Seq(2, 1, 0)
    val matrix = algos.Matrices.permutation[Int](perm)
    println(SpatialPermutation(Seq(0,1,2), matrix).mkString(" "))
  }
}
