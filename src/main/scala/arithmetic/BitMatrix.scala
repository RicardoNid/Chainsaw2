package org.datenlord
package arithmetic

import dfg._

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Compress a bitmatrix
 *
 * @param baseCompressor a basic compressor which will be repeatedly used to compress the matrix
 * @param pipeline       used to connect a stage with the stage after
 * @param baseWidth      length upper bound of a base compressor, making it carry-save
 * @example [[BMC]] is a hardware instance of bit matrix compressor
 */
case class BitMatrixCompressor[T](baseCompressor: Seq[Seq[T]] => Seq[T], pipeline: T => T, baseWidth: Int) {

  def compressOnce(matrix: BitMatrix[T]): (BitMatrix[T], Int) = {
    val table = matrix.table
    val bitsCountBefore = matrix.bitsCount
    val operands = ArrayBuffer[Seq[T]]()
    val infos = ArrayBuffer[ArithInfo]()
    var cost = 0
    val height = matrix.height
    //    logger.info(s"bit matrix before reduction: $matrix")
    while (matrix.height >= 3) {
      // get slice as long as possible
      val start = table.indexWhere(_.length >= 3)
      val strategy = 2
      val slice = strategy match {
        case 0 => val end = table.lastIndexWhere(_.length >= 3)
          table.slice(start, end + 1).take(baseWidth)
        case 1 => val end = table.lastIndexWhere(_.length >= 2)
          table.slice(start, end + 1).take(baseWidth)
        case 2 =>
          if (matrix.height > 3)
            table.drop(start).takeWhile(_.length >= 3).take(baseWidth)
          else
            table.drop(start).takeWhile(_.length >= 2).take(baseWidth)
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
    //    logger.info(s"height = $height, cost = $cost, bits reduction = $bitsReduction, ratio = ${cost.toDouble / bitsReduction}")
    (ret, cost)
  }

  def compressAll(matrix: BitMatrix[T]): (BitMatrix[T], Int, Int) = {
    val bitsCountBefore = matrix.bitsCount
    var current = matrix
    var cost = 0
    var stage = 0
    while (current.height >= 3) {
      val (matrixNext, costOnce) = compressOnce(current)
      current = matrixNext
      cost += costOnce
      stage += 1
    }
    val bitsReduction = bitsCountBefore - current.bitsCount
    logger.info(s"cost = $cost, bits reduction = $bitsReduction, ratio = ${cost.toDouble / bitsReduction}")
    (current, cost, stage)
  }
}

/** Storing information of a bit matrix, while providing util methods, making operations on bit matrix easier
 * @param table the bits
 * @param shift the base shift of the whole bit matrix, this is necessary as a bit matrix can merge with others
 */
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

  def getLatency(infos: Seq[ArithInfo], baseWidth: Int) = {
    def compressor: Seq[Seq[Char]] => Seq[Char] = (dataIn: Seq[Seq[Char]]) => {
      val width = dataIn.length
      val padded = dataIn.map(_.padTo(3, '0'))
      val sum = padded.transpose.map(operand2BigInt).sum
      bigInt2Operand(sum).padTo(width + 2, '0')
    }

    def pipeline(c: Char) = c

    def bigInt2Operand(value: BigInt) = value.toString(2).toCharArray.toSeq.reverse

    def operand2BigInt(operand: Seq[Char]) = BigInt(operand.mkString("").reverse, 2)

    val operands = infos.map(_.width).map(width => Random.nextBigInt(width - 1) + (BigInt(1) << (width - 1)))
    val original = BitMatrix(operands.map(bigInt2Operand), infos)
    BitMatrixCompressor(compressor, pipeline, baseWidth).compressAll(original)._3
  }
}