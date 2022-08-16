package org.datenlord
package arithmetic

import dfg._

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

abstract class Compressor[T] {

  // following methods take width as a parameter as we need to support row compressor which don't have a fixed size

  val isFixed: Boolean

  val widthLimit: Int

  /** number of bits in input columns, low to high
   */
  def inputFormat(width: Int): Seq[Int]

  /** number of bits in output columns, low to high
   */
  def outputFormat(width: Int): Seq[Int]

  /** number of LUTs
   */
  def cost(width: Int): Int

  def impl(bitsInt: BitHeap[T], width: Int): BitHeap[T]

  def inputBitsCount(width: Int) = outputFormat(width).sum

  def outputBitsCount(width: Int) = outputFormat(width).sum

  def reduction(width: Int): Int = inputBitsCount(width) - outputBitsCount(width)

  def efficiency(width: Int): Double = reduction(width).toDouble / cost(width)

  /** this is very beautiful, try it!
   */
  def toString(width: Int) = {
    val dotsIn = BitHeap.fromHeights(inputFormat(width)).toString
    val dotsOut = BitHeap.fromHeights(outputFormat(width)).toString
    val length = outputFormat(width).length
    val arrowLine = s"${" " * (length / 2) * 2}\u2193"
    val shiftedDotsIn = dotsIn.split("\n").map(_.padToLeft(length * 2 - 1, ' ')).mkString("\n")
    s"$shiftedDotsIn\n$arrowLine\n$dotsOut"
  }

}

/** Compress a bitmatrix
 *
 * @param baseCompressor a basic compressor which will be repeatedly used to compress the matrix
 * @param pipeline       used to connect a stage with the stage after
 * @param baseWidth      length upper bound of a base compressor, making it carry-save
 * @example [[Bmc]] is a hardware instance of bit matrix compressor
 */
case class BitMatrixCompressor[T](baseCompressor: (Seq[Seq[T]], Int) => Seq[T], pipeline: T => T, baseWidth: Int) {

  def compressOnce(matrix: BitHeap[T]): (BitHeap[T], Int) = {
    val table = matrix.bitHeap
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
      val operand = baseCompressor(inputs, slice.length)
      operands += operand
      infos += ArithInfo(width, matrix.weightLow + start)
      cost += slice.length
    }
    val pipelinedMatrix = BitHeap(matrix.bitHeap.map(_.map(pipeline)), matrix.weightLow)
    val ret = BitHeap.fromOperands(operands, infos) + pipelinedMatrix
    val bitsCountAfter = ret.bitsCount
    val bitsReduction = bitsCountBefore - bitsCountAfter
    logger.info(s"height = $height, cost = $cost, bits reduction = $bitsReduction, ratio = ${cost.toDouble / bitsReduction}")
    //    logger.info(s"bit matrix before reduction: $ret")
    (ret, cost)
  }

  def compressAll(matrix: BitHeap[T]): (BitHeap[T], Int, Int) = {
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

/** Storing information of a bit matrix(heap), while providing util methods, making operations on bit matrix easier
 *
 * @param bitHeap the bits, each array buffer in the table stands for a column, low to high
 * @example bitHeap(m)(n) is the (n+1)-th bit of (m+1)-th column
 * @param weightLow the base weight of the whole bit heap, this is necessary as a bit matrices can merge with each other
 * @see ''Brunie, Nicolas, Florent de Dinechin, Matei Iştoan, Guillaume Sergent, Kinga Illyes and Bogdan Popa. “Arithmetic core generation using bit heaps.” 2013 23rd International Conference on Field programmable Logic and Applications (2013): 1-8.''
 */
case class BitHeap[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int) {
  // merge two bit heaps
  def +(that: BitHeap[T]): BitHeap[T] = {
    // get size of the new table
    val newLow = this.weightLow min that.weightLow
    val newHigh = this.weightHigh max that.weightHigh
    val newWidth = newHigh + 1 - newLow
    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    // move bits
    newTable.drop(this.weightLow - newLow) // align
      .zip(this.bitHeap).foreach { case (a, b) => a ++= b } // move bits
    newTable.drop(that.weightLow - newLow) // align
      .zip(that.bitHeap).foreach { case (a, b) => a ++= b } // move bits
    BitHeap(newTable, newLow)
  }

  def heights = bitHeap.map(_.length)

  def height = bitHeap.map(_.length).max // height of the bit heap, compression ends when height is under some bound

  def width = bitHeap.length // number of columns in the bit heap

  def weightHigh: Int = weightLow + width - 1

  def bitsCount = bitHeap.map(_.length).sum

  def isEmpty = bitHeap.forall(_.isEmpty)

  // for a given compressor and a column, find the exact number of bits that can be covered
  def getExactBits(compressor: Compressor[_], width: Int, columnIndex: Int) = {
    val bits = compressor.inputFormat(width) // height of columns in input pattern
      .zip(heights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 } // overlap
      .sum
    //    logger.info(s"try ${compressor.getClass.getSimpleName} on width $width at column $columnIndex, bits = $bits")
    bits
  }

  def getExactEfficiency(compressor: Compressor[_], width: Int, columnIndex: Int): Double = {
    val eff = (getExactBits(compressor, width, columnIndex) - // bitsIn
      compressor.outputBitsCount(width)) / // bitsOut
      compressor.cost(width).toDouble // divided by cost
    //    logger.info(s"try ${compressor.getClass.getSimpleName} on width $width at column $columnIndex, eff = $eff")
    eff
  }

  // TODO: sort the compressors

  def doCompression(compressor: Compressor[T], width: Int, columnIndex: Int) = {
    val newTable: Seq[ArrayBuffer[T]] = compressor.inputFormat(width) // bit in each columns that you need
      .zip(bitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }
    compressor.impl(BitHeap.fromColumns(newTable, columnIndex), width)
  }

  /** get the most efficient compressor for current bit heap
   *
   * @param candidates a list of compressor whose maximum efficiencies are in decreasing order
   * @return
   */
  def getCompressor(candidates: Seq[Compressor[T]]) = {
    var bestEff = 0.0
    var bestWidth = -1
    val columnIndex = heights.indexWhere(_ == heights.max) // find the first(lowest weight) column with maximum height

    // TODO: replace this by a loop
    val result = candidates.takeWhile { compressor =>
      val (eff, width) =
        if (compressor.isFixed) (getExactEfficiency(compressor, -1, columnIndex), -1) // for GPC, get eff
        else { // for row compressor, try different widths, get the best one with its width
          // TODO: avoid trying all widths
          val widthMax = compressor.widthLimit min (this.width - columnIndex)
          if (widthMax >= 1) (1 to widthMax).map(w => (getExactEfficiency(compressor, w, columnIndex), w)).maxBy(_._1)
          else (-1.0, 0) // skip
        }
      val pass = eff >= bestEff
      if (pass) {
        bestEff = eff
        bestWidth = width
      }
      pass
    }.last

    (result, bestWidth, columnIndex)
  }

  def compressOneTime(candidates: Seq[Compressor[T]]) = {
    //    logger.info(s"after a compression:\n${this.toString}")
    val (compressor, width, columnIndex) = getCompressor(candidates)
    doCompression(compressor, width, columnIndex)
  }

  def compressOneStage(candidates: Seq[Compressor[T]]) = {
    //    logger.info(s"before compression:\n${this.toString}")
    val results = ArrayBuffer[BitHeap[T]]()
    while (!isEmpty) {
      results += compressOneTime(candidates)
    }
    val nextStage = results.reduce(_ + _)
    logger.info(s"after a stage:\n${nextStage.toString}")
    nextStage
  }

  def compressAll(candidates: Seq[Compressor[T]]) = {
    var current = this
    var times = 0
    while (current.height > 2 && times < 100) {
      current = current.compressOneStage(candidates)
      times += 1
    }
    current
  }

  def output(zero: () => T): Seq[Seq[T]] = {
    require(height <= 2)
    bitHeap.map(_.padTo(2, zero())).transpose
  }

  override def toString = {
    val dotDiagram = heights.map(columnHeight => Seq.fill(columnHeight)("\u2B24").padTo(this.height, " ")).reverse.transpose
      .map(_.mkString(" ")).mkString("\n")
    dotDiagram
  }
}

//case class SoftBitHeap(override val bitHeap: ArrayBuffer[ArrayBuffer[BigInt]], override val weightLow: Int)
//  extends BitHeap[BigInt](bitHeap, weightLow) {
//
//  def eval: BigInt = bitHeap.zipWithIndex.map { case (column, i) => column.sum << (i + weightLow) }.sum
//}

object BitHeap {

  def fromHeights(heights: Seq[Int]) = BitHeap[Int](ArrayBuffer(heights: _*).map(i => ArrayBuffer.fill(i)(1)), 0)

  /** Build bit matrix from operands
   *
   * @param operands An operand is a low to high sequence of bits
   * @param infos    Infos record the width and position(shift) info of corresponding operands
   */
  def fromOperands[T](operands: Seq[Seq[T]], infos: Seq[ArithInfo]): BitHeap[T] = {

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

    BitHeap(table, positionLow)
  }

  def fromColumns[T](columns: Seq[Seq[T]], weightLow: Int = 0) = {
    val table = ArrayBuffer.fill(columns.length)(ArrayBuffer[T]())
    table.zip(columns).foreach { case (buf, seq) => buf ++= seq }
    BitHeap(table, weightLow)
  }

  def getInfoOfCompressor(infos: Seq[ArithInfo], baseWidth: Int) = {
    def compressor: (Seq[Seq[Char]], Int) => Seq[Char] = (dataIn: Seq[Seq[Char]], width: Int) => {
      val width = dataIn.length
      val padded = dataIn.map(_.padTo(3, '0'))
      val sum = padded.transpose.map(operand2BigInt).sum
      bigInt2Operand(sum).padTo(width + 2, '0')
    }

    def pipeline(c: Char) = c

    def bigInt2Operand(value: BigInt) = value.toString(2).toCharArray.toSeq.reverse

    def operand2BigInt(operand: Seq[Char]) = BigInt(operand.mkString("").reverse, 2)

    val operands = infos.map(_.width).map(width => (BigInt(1) << width) - 1)
    //    val operands = infos.map(_.width).map(width => Random.nextBigInt(width - 1) + (BigInt(1) << (width - 1)))
    val original = BitHeap.fromOperands(operands.map(bigInt2Operand), infos)
    val (matrix, cost, latency) = BitMatrixCompressor(compressor, pipeline, baseWidth).compressAll(original)
    val widthOut = matrix.bitHeap.length + matrix.weightLow
    (widthOut, latency, cost)
  }
}