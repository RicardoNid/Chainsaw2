package org.datenlord
package arithmetic

import dfg._

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class BitMatrixCompressor[T](baseCompressor: Seq[Seq[T]] => Seq[T], pipeline: T => T, baseWidth: Int) {

  def compressOnce(matrix: BitMatrix[T]): (BitMatrix[T], Int) = {
    //    println(s"before compression\n$matrix")
    val table = matrix.table
    val bitsCountBefore = matrix.bitsCount
    val operands = ArrayBuffer[Seq[T]]()
    val infos = ArrayBuffer[ArithInfo]()
    var cost = 0
    val height = matrix.height
    while (matrix.height >= 3) {
      // get slice as long as possible
      val start = table.indexWhere(_.length >= 3)
      val strategy = 2
      val slice = strategy match {
        case 0 => val end = table.lastIndexWhere(_.length >= 3)
          table.slice(start, end + 1).take(baseWidth)
        case 1 => val end = table.lastIndexWhere(_.length >= 2)
          table.slice(start, end + 1).take(baseWidth)
        case 2 => if (matrix.height > 3) table.drop(start).takeWhile(_.length >= 3).take(baseWidth)
        else table.drop(start).takeWhile(_.length >= 2).take(baseWidth)
      }

      val width = slice.length + 2
      val inputs: Seq[ArrayBuffer[T]] = slice.map(_.take(3))
      slice.zip(inputs).foreach { case (whole, part) => whole --= part }
      val operand = baseCompressor(inputs)
      operands += operand
      infos += ArithInfo(width, matrix.shift + start)
      cost += slice.length
    }
    val pipelinedMatrix = BitMatrix(matrix.table.map(_.map(pipeline)), matrix.shift)
    val ret = BitMatrix.apply(operands, infos) + pipelinedMatrix
    val bitsCountAfter = ret.bitsCount
    val bitsReduction = bitsCountBefore - bitsCountAfter
    logger.info(s"height = $height, cost = $cost, bits reduction = $bitsReduction, ratio = ${cost.toDouble / bitsReduction}")
    (ret, cost)
  }

  def compressAll(matrix: BitMatrix[T]): (BitMatrix[T], Int) = {
    val bitsCountBefore = matrix.bitsCount
    var current = matrix
    var cost = 0
    while (current.height >= 3) {
      val (matrixNext, costOnce) = compressOnce(current)
      current = matrixNext
      cost += costOnce
    }
    val bitsReduction = bitsCountBefore - current.bitsCount
    logger.info(s"cost = $cost, bits reduction = $bitsReduction, ratio = ${cost.toDouble / bitsReduction}")
    (current, cost)
  }

}

case class BitMatrix[T](table: ArrayBuffer[ArrayBuffer[T]], shift: Int) {

  def +(that: BitMatrix[T]): BitMatrix[T] = {
    val newLow = this.shift min that.shift
    val newHigh = (this.table.length + this.shift) max (that.table.length + that.shift)
    val newWidth = newHigh - newLow
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    newTable.drop(this.shift - newLow).zip(this.table).foreach { case (a, b) => a ++= b }
    newTable.drop(that.shift - newLow).zip(that.table).foreach { case (a, b) => a ++= b }
    BitMatrix(newTable, newLow)
  }

  def height = table.map(_.length).max

  def bitsCount = table.map(_.length).sum

  override def toString = s"bit matrix starts from $shift:\n${table.map(_.length).mkString(",")}"
}


object BitMatrix {

  /** Build bit matrix from operands
   *
   * @param operands An operand is a low to high sequence of bits
   * @param infos    Infos record the width and position(shift) info of corresponding operands
   */
  def apply[T](operands: Seq[Seq[T]], infos: Seq[ArithInfo]): BitMatrix[T] = {

    // get the width of the table
    val positionHigh = infos.map(_.high).max
    val positionLow = infos.map(_.low).min
    val width = positionHigh - positionLow

    // build the table from operands
    val table = ArrayBuffer.fill(width)(ArrayBuffer[T]())
    operands.zip(infos).foreach { case (operand, info) =>
      val start = info.shift - positionLow
      // insert bits from low to high
      (start until start + info.width).foreach(i => table(i) += operand(i - start))
    }

    BitMatrix(table, positionLow)
  }
}
