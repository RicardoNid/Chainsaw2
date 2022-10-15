package org.datenlord
package zprize

import breeze.math.Complex
import breeze.numerics.exp
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

object CommonAlgos {

  def WNnk(N: Int, nk: Int): Complex = exp(Complex(0, -2 * Pi * nk / N))

  def matIntrlv[T](row:Int, col:Int, dataIn: Seq[T]): Seq[T] = Seq.tabulate(col, row)((i, j) => j * col + i).flatten.map(dataIn(_))

}
