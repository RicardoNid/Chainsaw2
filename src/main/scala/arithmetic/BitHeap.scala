package org.datenlord
package arithmetic

import dfg._

import scala.collection.mutable.ArrayBuffer

/** Storing information of a bit matrix(heap), while providing util methods, making operations on bit matrix easier
 *
 * @tparam T a bit heap can be initialized by a non-hardware type, so it can be run outside a component
 * @param bitHeap the bits, each array buffer in the table stands for a column, low to high
 * @example bitHeap(m)(n) is the (n+1)-th bit of (m+1)-th column
 * @param weightLow the base weight of the whole bit heap, this is necessary as a bit matrices can merge with each other
 * @see ''Brunie, Nicolas, Florent de Dinechin, Matei Iştoan, Guillaume Sergent, Kinga Illyes and Bogdan Popa. “Arithmetic core generation using bit heaps.” 2013 23rd International Conference on Field programmable Logic and Applications (2013): 1-8.''
 */
case class BitHeap[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int) {

  /** --------
   * utils
   * -------- */

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

  // TODO: implement truncation by these methods
  // x % (BigInt(1) << widthTake)
  def takeLow(widthTake:Int): BitHeap[T] = BitHeap.getHeapFromTable(bitHeap.take(widthTake - weightLow))

  // approximately = x / (BigInt(1) << widthDrop)
  def dropLow(widthDrop:Int): BitHeap[T] = BitHeap.getHeapFromTable(bitHeap.drop(widthDrop - weightLow), widthDrop)

  def d(pipeline: T => T): BitHeap[T] = BitHeap(bitHeap.map(_.map(pipeline)), weightLow) // pipeline the whole bit heap

  def heights: Seq[Int] = bitHeap.map(_.length)

  def height = bitHeap.map(_.length).max // height of the bit heap, compression ends when height is under some bound

  def width = bitHeap.length // number of columns in the bit heap

  def weightHigh: Int = weightLow + width - 1

  def bitsCount = bitHeap.map(_.length).sum

  def isEmpty = bitHeap.forall(_.isEmpty)

  /** --------
   * methods for compression
   * -------- */

  // these methods are designed for simulation, which can be run outside a component
  def fakeImpl(compressor: Compressor[_], width: Int, columnIndex: Int) =
    BitHeap.getFakeHeapFromHeights(compressor.outputFormat(width), columnIndex).asInstanceOf[BitHeap[T]]

  def fakePipeline(any: T): T = 0.asInstanceOf[T]


  /** get the exact(rather than maximum) efficiency of a compressor applied on current bit heap
   *
   * @param columnIndex column with lowest weight covered by the compressor
   */
  def getExactEfficiency(compressor: Compressor[_], width: Int, columnIndex: Int): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor.inputFormat(width) // height of columns in input pattern
      .zip(heights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 } // overlap
      .sum

    (bits - // bitsIn
      compressor.outputBitsCount(width)) / // bitsOut
      compressor.cost(width).toDouble // divided by cost
  }

  /** get the most efficient compressor for current bit heap and do compression with it
   *
   * @param compressors a list of available compressors,
   *                    the first one must be a 1 to 1 compressor which won't do compression
   * @param finalStage  the this flag is set, a lower efficiency threshold will be used
   * @tparam T0 the type is T0 rather than T, as we may want to use Compressors[Bool] on BitHeap[Int] for simulation
   * @return new bit heap generated by the compressor, and the LUT cost
   */
  def compressOneTime[T0](compressors: Seq[Compressor[T0]], finalStage: Boolean): (BitHeap[T], Int) = {

    val mark: T = bitHeap.filter(_.nonEmpty).head.head // for type match

    // TODO: better strategies on final stages
    require(compressors.head.inputBitsCount(-1) == compressors.head.outputBitsCount(-1))

    val effBound = if (finalStage) 0.0 else 1.0
    // when no qualified compressor can be found, the 1 to 1 compressor(no compression) will be chosen
    var bestCompressor = compressors.head
    var bestEff = 0.0
    val columnIndex = heights.indexWhere(_ == heights.max) // find the first(lowest weight) column with maximum height
    // number of continuous nonempty columns that 1 to 1 compressor can be applied on
    var bestWidth = bitHeap.drop(columnIndex).takeWhile(_.nonEmpty).length

    // sort by efficiency, high to low, besides the 1 to 1 compressor which appear as head
    val candidates = compressors.tail.sortBy(_.efficiency(width)).reverse

    candidates.foreach { compressor => // traverse all available compressors
      val widthMax = compressor.widthLimit min (this.width - columnIndex)
      val maximumEff = compressor.efficiency(widthMax)
      if (maximumEff >= bestEff) // skip when ideal efficiency is lower than current best efficiency
      {
        val (exactEff, width) = {
          if (compressor.isFixed) (getExactEfficiency(compressor, -1, columnIndex), -1) // for GPC, get eff
          else { // for row compressor, try different widths, get the best one with its width
            // TODO: avoid trying all widths
            if (widthMax >= 1) (1 to widthMax).map(w => (getExactEfficiency(compressor, w, columnIndex), w)).maxBy(_._1)
            else (-1.0, 0) // skip
          }
        }
        if (exactEff >= bestEff && (exactEff >= effBound)) { // update if a better compressor is found
          bestEff = exactEff
          bestWidth = width
          bestCompressor = compressor
        }
      }
    }

    //    if (bestCompressor != compressors.head)
    //      logger.info(s"get ${bestCompressor.getClass.getSimpleName} column=$columnIndex width=$bestWidth efficiency=$bestEff")

    val newTable = bestCompressor.inputFormat(bestWidth) // remove and get bits in each columns that you need
      .zip(bitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }

    val newHeap = mark match {
      case t: Int => fakeImpl(bestCompressor, bestWidth, columnIndex) // when T is Int, T0 != T
      case _ => // when T is Bool, T0 = T, generate the bit heap after compression
        bestCompressor.impl(BitHeap.getHeapFromTable(newTable.asInstanceOf[Seq[ArrayBuffer[T0]]], columnIndex + weightLow), bestWidth).asInstanceOf[BitHeap[T]]
    }

    val cost = bestCompressor.cost(bestWidth)
    (newHeap, cost)
  }

  /** do compression until all bits are covered and go to next stage
   *
   * @return new heap for the next stage, and the LUT cost
   */
  def compressOneStage[T0](compressors: Seq[Compressor[T0]], pipeline: T0 => T0, finalStage: Boolean): (BitHeap[T], Int) = {

    val bitsCountBefore = this.bitsCount
    val mark = bitHeap.filter(_.nonEmpty).head.head // for type match

    var stageCost = 0
    val results = ArrayBuffer[BitHeap[T]]()
    // compress until all bits are covered
    while (!isEmpty) {
      val (heap, cost) = compressOneTime(compressors, finalStage)
      results += heap
      stageCost += cost
    }

    val nextStage = mark match {
      case t: Int => results.reduce(_ + _).d(fakePipeline)
      case _ => results.asInstanceOf[Seq[BitHeap[T0]]].reduce(_ + _).d(pipeline).asInstanceOf[BitHeap[T]] // when T is Bool, T0 = T
    }

    val compressed = bitsCountBefore - nextStage.bitsCount
    logger.info(s"stage efficiency = ${compressed.toDouble / stageCost}, cost = $stageCost \n${nextStage.toString}")
    (nextStage, stageCost)
  }

  /** do compression until there's no more than two lines in the bit heap
   *
   * @return final bit heap and the key information of the compressor tree (latency, widthOut, etc.)
   */
  def compressAll[T0](candidates: Seq[Compressor[T0]], pipeline: T0 => T0 = null) = {
    logger.info(s"initial state:\n${this.toString}")
    val bitsInTotal = this.bitsCount
    var current = this
    var latency = 0
    var badLatency = 0
    var allCost = 0
    while (current.height > 2 && latency < 100) {
      if (current.height <= 4) badLatency += 1
      val (heap, cost) = current.compressOneStage(candidates, pipeline, finalStage = current.height <= 4)
      current = heap
      allCost += cost
      latency += 1
    }
    val allCompressed = bitsInTotal - current.bitsCount
    logger.info(s"compressor tree - cost in total: $allCost, efficiency in total = ${allCompressed.toDouble / allCost}")
    logger.info(s"latency: $latency, bad latency: $badLatency, widthOut: ${current.width}")
    (current, latency, current.width)
  }

  def output(zero: () => T): Seq[Seq[T]] = {
    require(height <= 2)
    (Seq.fill(weightLow)(ArrayBuffer[T]()) ++ bitHeap).map(_.padTo(2, zero())).transpose
  }

  override def toString = {
    val dotDiagram = heights.map(columnHeight => Seq.fill(columnHeight)("\u2B24").padTo(this.height, " ")).reverse.transpose
      .map(_.mkString(" ")).mkString("\n")
    dotDiagram
  }
}

object BitHeap {

  def getHeapFromTable[T](table: Seq[Seq[T]], weightLow: Int = 0) = {
    val tableForHeap = ArrayBuffer.fill(table.length)(ArrayBuffer[T]())
    tableForHeap.zip(table).foreach { case (buf, seq) => buf ++= seq }
    BitHeap(tableForHeap, weightLow)
  }

  /** build a BitHeap[Int] with a specified shape for simualtion
   */
  def getFakeHeapFromHeights(heights: Seq[Int], weightLow: Int = 0) =
    BitHeap[Int](ArrayBuffer(heights: _*).map(i => ArrayBuffer.fill(i)(0)), weightLow)

  /** build bit matrix from operands and their shifts
   *
   * @param infos    infos record the width and position(shift) info of corresponding operands
   * @param operands an operand is a low to high sequence of bits
   */
  def getHeapFromInfos[T](infos: Seq[ArithInfo], operands: Seq[Seq[T]]): BitHeap[T] = {

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

  def getFakeHeapFromInfos(infos: Seq[ArithInfo]) = {
    val operands = infos.map(info => Seq.fill(info.width)(0))
    getHeapFromInfos(infos, operands)
  }
}