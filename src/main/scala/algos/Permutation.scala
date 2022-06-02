package org.datenlord
package algos

import scala.reflect.ClassTag

object Permutation {

  def strideByMPermutation[T:ClassTag](dataIn:Seq[T],m:Int) = dataIn.grouped(m).toSeq.transpose.flatten

  def main(args: Array[String]): Unit = {
    println(strideByMPermutation(0 until 8, 2).mkString(" "))
  }
}
