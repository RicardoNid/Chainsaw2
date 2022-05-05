package org.datenlord

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.reflect.ClassTag

package object algos {

  implicit class BigIntUtil(bi: BigInt) {

    /**
     * @example 10100.splitAt(3) = (10,100), not (101,11)
     */
    def splitAt(lowWidth: Int) = (bi >> lowWidth, bi % (BigInt(1) << lowWidth))

    def toWords(wordWidth: Int) = bi.toString(2)
      .reverse.grouped(wordWidth).toSeq
      .map(digits => BigInt(digits.reverse, 2)).toArray
  }

  implicit class ArrayUtil[T](array: Array[T]) {
    def padToLeft(len: Int, elem: T) = array.reverse.padTo(len, elem).reverse
  }

  implicit class DMUtil[T:ClassTag](matrix: DenseMatrix[T]) {
    def diag = {
      require(matrix.rows == matrix.cols)
      val n = matrix.rows
      new DenseVector((0 until n).map(i => matrix(i, i)).toArray)
    }
  }

  implicit class StringUtil(s: String) {
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse
  }


}
