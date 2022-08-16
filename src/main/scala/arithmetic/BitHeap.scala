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

import scala.util.control.Breaks.break


/** Storing information of a bit matrix(heap), while providing util methods, making operations on bit matrix easier
 *
 * @tparam T a bit heap can be initialized by a non-hardware type, so it can be run outside a component
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

  def d(pipeline: T => T) = BitHeap(bitHeap.map(_.map(pipeline)), weightLow)

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
    eff
  }

  // TODO: sort the compressors

  var cost = 0

  def doCompression(compressor: Compressor[T], width: Int, columnIndex: Int) = {
    val newTable: Seq[ArrayBuffer[T]] = compressor.inputFormat(width) // bit in each columns that you need
      .zip(bitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }
    cost += compressor.cost(width)
    compressor.impl(BitHeap.fromColumns(newTable, columnIndex), width)
  }

  /** get the most efficient compressor for current bit heap
   *
   * @param compressors a list of compressor whose maximum efficiencies are in decreasing order,
   *                    the first one must be 1 to 1 compressor which won't do compression
   * @return
   */
  def getCompressor(compressors: Seq[Compressor[T]], softBound: Boolean) = {

    // the first one must be 1 to 1 compressor which won't do compression
    require(compressors.head.inputBitsCount(-1) == compressors.head.outputBitsCount(-1))

    var bestCompressor = compressors.head
    var bestEff = 0.0
    var bestWidth = -1
    val columnIndex = heights.indexWhere(_ == heights.max) // find the first(lowest weight) column with maximum height

    // sort by efficiency, high to low, besides the 1 to 1 compressor which appear as head
    val candidates = compressors.tail.sortBy(_.efficiency(width)).reverse

    candidates.foreach { compressor =>
      if (compressor.efficiency(width) >= bestEff) // skip when ideal efficiency is lower than current best efficiency
      {
        val (exactEff, width) = {
          if (compressor.isFixed) (getExactEfficiency(compressor, -1, columnIndex), -1) // for GPC, get eff
          else { // for row compressor, try different widths, get the best one with its width
            val widthMax = compressor.widthLimit min (this.width - columnIndex) // TODO: avoid trying all widths
            if (widthMax >= 1) (1 to widthMax).map(w => (getExactEfficiency(compressor, w, columnIndex), w)).maxBy(_._1)
            else (-1.0, 0) // skip
          }
        }
        if (exactEff >= bestEff && (exactEff >= 1.0 || softBound)) { // update if a better compressor is found
          bestEff = exactEff
          bestWidth = width
          bestCompressor = compressor
        }
      }
    }

    if (bestCompressor != compressors.head)
      logger.info(s"get ${bestCompressor.getClass} width $bestWidth efficiency $bestEff")

    (bestCompressor, bestWidth, columnIndex)
  }

  def compressOneTime(candidates: Seq[Compressor[T]], softBound:Boolean) = {
    val (compressor, width, columnIndex) = getCompressor(candidates, softBound)
    doCompression(compressor, width, columnIndex)
  }

  def compressOneStage(candidates: Seq[Compressor[T]], pipeline: T => T, softBound:Boolean) = {
    val results = ArrayBuffer[BitHeap[T]]()
    while (!isEmpty) {
      results += compressOneTime(candidates, softBound:Boolean)
    }
    val nextStage = results.reduce(_ + _)
    logger.info(s"after a stage:\n${nextStage.toString}")
    nextStage.d(pipeline)
  }

  // TODO: merge function to avoid passing parameters
  // TODO: passing cost

  def compressAll(candidates: Seq[Compressor[T]], pipeline: T => T) = {
    var current = this
    var times = 0
    while (current.height > 2 && times < 100) {
      current = current.compressOneStage(candidates, pipeline, softBound = current.height <= 3)
      times += 1
    }
    logger.info(s"cost in total: $cost")
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