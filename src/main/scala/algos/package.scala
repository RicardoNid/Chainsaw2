package org.datenlord

package object algos {

  implicit class BigIntUtil(bi: BigInt) {

    /**
     * @example 10100.splitAt(3) = (10,100), not (101,11)
     */
    def splitAt(lowWidth: Int) = (bi >> lowWidth, bi % (BigInt(1) << lowWidth))

    def toWords(wordWidth: Int) = bi.toString(2)
      .reverse.grouped(wordWidth).toSeq
      .map(digits =>  BigInt(digits.reverse, 2)).toArray
  }

  implicit class ArrayUtil[T](array: Array[T]) {
    def padToLeft(len: Int, elem: T) = array.reverse.padTo(len, elem).reverse
  }



}
