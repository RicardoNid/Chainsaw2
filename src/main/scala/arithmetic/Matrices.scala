package org.datenlord
package arithmetic

import breeze.linalg._
import breeze.math.Semiring
import org.datenlord.{logR, powR}

import scala.reflect.ClassTag

object Matrices {

  def diagonal[T: ClassTag](coeff: Seq[T])(implicit ring: Semiring[T]) = {
    val n = coeff.length
    DenseMatrix.tabulate(n, n)((i, j) => if (i == j) coeff(i) else ring.zero)
  }

  def permutation[T: ClassTag](perm: Seq[Int])(implicit ring: Semiring[T]) = {
    val n = perm.length
    DenseMatrix.tabulate(n, n)((i, j) => if (perm(i) == j) ring.one else ring.zero)
  }

  /**
   * @param stride the original distance of adjacent elements after permutation
   */
  def stridePermutation[T: ClassTag](n: Int, stride: Int)(implicit ring: Semiring[T]) = {
    val perm = (0 until n).grouped(stride).toSeq.transpose.flatten
    permutation(perm)
  }

  def digitReversalPermutation[T: ClassTag](n: Int, radix: Int)(implicit ring: Semiring[T]) = {
    val t = logR(n, radix)
    val elems = (0 until t).map { i =>
      val perm = stridePermutation[T](powR(radix, i + 1), radix)
      kronecker(perm, powR(radix, t - i - 1))
    }
    elems.reduce(_ * _)
  }

  def kronecker[T: ClassTag](matrix: DenseMatrix[T], factor: Int)(implicit ring: Semiring[T]) = {
    val m = matrix.rows
    val n = matrix.cols
    DenseMatrix.tabulate(m * factor, n * factor) { (i, j) =>
      val onDiagonal = (i / m) == (j / n)
      val (r, c) = (i % m, j % n)
      if (onDiagonal) matrix(r, c) else ring.zero
    }
  }

  def kronecker[T: ClassTag](a: DenseMatrix[T], b: DenseMatrix[T])(implicit ring: Semiring[T]) = {
    val m = b.rows
    val n = b.cols
    DenseMatrix.tabulate(m * a.rows, n * a.cols) { (i, j) =>
      val (ra, ca) = (i / m, j / n)
      val (rb, cb) = (i % m, j % n)
      a(ra, ca) * b(rb, cb)
    }
  }

  def main(args: Array[String]): Unit = { // examples

    // permutation matrix do permutation on a sequence by matrix-vector multiplication
    val perm = permutation[Int](Seq(1, 0, 2))
    val data = DenseVector(0, 1, 2)
    println(perm * data)

    val stridePerm = stridePermutation[Int](6, 3)
    val strideData = DenseVector(0, 1, 2, 3, 4, 5)
    println(stridePerm * strideData)

    // kronecker product of a matrix extends its transformation vertically
    println(perm * data)
    val expandedData = DenseVector(0, 1, 2, 0, 1, 2)
    println(kronecker(perm, 2) * expandedData)

    // show the concept "digit reverse"
    val bitReverse = digitReversalPermutation[Int](9, 3)
    val bitData = DenseVector(0, 1, 2, 3, 4, 5, 6, 7, 8)
    println(bitData.map(BigInt(_).toString(3).padToLeft(3, '0')))
    val ret = bitReverse * bitData
    println(ret.map(BigInt(_).toString(3).padTo(3, '0')))

  }

}
