package org.datenlord
package zprize

import dfg._
import org.datenlord.zprize.BitHeap.getHeapFromHeights
import spinal.core._

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.language.postfixOps
import scala.math._
import scala.util.control._
import org.json4s._
import org.json4s.jackson.Serialization._

/** @tparam T
  *   a bit heap can be initialized by a non-hardware type, so it can be run outside a component
  * @param bitHeap
  *   the bits, each array buffer in the table stands for a column, low to high
  * @example
  *   bitHeap.head(m)(n) is the (n+1)-th bit of (m+1)-th column in first sub-bitHeap
  * @param weightLow
  *   the base weight of the whole bit heap, this is necessary as a bit matrices can merge with each other
  * @param time
  *   the delay of each sub-bitHeap
  */
case class BitHeapConfigInfo[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int, time: Int) {
  def +(that: BitHeapConfigInfo[T]): BitHeapConfigInfo[T] = {
    require(this.time == that.time, "add two BitHeapConigInfo must have same time value !")
    val newLow   = this.weightLow min that.weightLow
    val newHigh  = (this.weightLow + this.bitHeap.length - 1) max (that.weightLow + that.bitHeap.length - 1)
    val newWidth = newHigh + 1 - newLow

    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    // move bits
    newTable
      .drop(this.weightLow - newLow) // align
      .zip(this.bitHeap)
      .foreach { case (a, b) => a ++= b } // move bits
    newTable
      .drop(that.weightLow - newLow) // align
      .zip(that.bitHeap)
      .foreach { case (a, b) => a ++= b } // move bits
    BitHeapConfigInfo(newTable, newLow, time)
  }
}

/** Storing information of a bit matrix(heap), while providing util methods, making operations on bit matrix easier
  *
  * @tparam T
  *   a bit heap can be initialized by a non-hardware type, so it can be run outside a component
  * @param bitHeapConfigInfo
  *   the config information of bitHeap
  * @see
  *   [[BitHeapCompressor]] for hardware implementation
  * @see
  *   ''Arithmetic core generation using bit heaps.'' [[https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6645544]]
  */
case class BitHeap[T](bitHeapConfigInfo: BitHeapConfigInfo[T]*) {
  val newBitHeapConfigInfo = bitHeapConfigInfo.groupBy(_.time).toSeq.map { case (time, infos) => infos.reduce(_ + _) }

  val bitHeaps   = ArrayBuffer(newBitHeapConfigInfo).flatten.map(_.bitHeap)
  val weightLows = ArrayBuffer(newBitHeapConfigInfo).flatten.map(_.weightLow)
  val times      = ArrayBuffer(newBitHeapConfigInfo).flatten.map(_.time)
  require(
    bitHeaps.length == weightLows.length && bitHeaps.length == times.length,
    s"The BitHeap info is illegal !\t length of bitHeaps : ${bitHeaps.length} length of weightLows : ${weightLows.length} length of times : ${times.length} "
  )

  /** -------- utils-------- */

  // merge two bit heaps
  def +(that: BitHeap[T]): BitHeap[T] = {
    that.times.foreach { t =>
      if (this.times.contains(t)) {
        val retIndex    = this.times.indexOf(t)
        val sourceIndex = that.times.indexOf(t)
        // get size of the new table
        val newLow   = this.weightLows(retIndex) min that.weightLows(sourceIndex)
        val newHigh  = this.weightHighs(retIndex) max that.weightHighs(sourceIndex)
        val newWidth = newHigh + 1 - newLow

        // initialization
        val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
        // move bits
        newTable
          .drop(this.weightLows(retIndex) - newLow) // align
          .zip(this.bitHeaps(retIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        newTable
          .drop(that.weightLows(sourceIndex) - newLow) // align
          .zip(that.bitHeaps(sourceIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        this.bitHeaps(retIndex)   = newTable
        this.weightLows(retIndex) = newLow
        this.times(retIndex)      = t
      } else {
        // append new bitHeap
        val sourceIndex = that.times.indexOf(t)
        this.bitHeaps += that.bitHeaps(sourceIndex)
        this.weightLows += that.weightLows(sourceIndex)
        this.times += that.times(sourceIndex)
      }
    }
    this
  }

  // add a row on bit heap, in-place operation, the row must be aligned this bit heap
  def addConstant(row: Seq[T], time: Int, zero: T) = {
    require(this.times.contains(time), s"The source BitHeap isn't contain the delay : $time, please check it !")
    val retIndex = times.indexOf(time)
    val overflow = row.length - widths(retIndex)
    if (overflow > 0) bitHeaps(retIndex) ++= Seq.fill(overflow)(ArrayBuffer[T]())
    this.bitHeaps(retIndex).zip(row).foreach { case (column, bit) => if (bit != zero) column += bit }
    this
  }

  // x % (BigInt(1) << widthTake)
  def takeLow(widthTake: Int): BitHeap[T] = BitHeap.getHeapFromTable(bitHeaps.zip(weightLows).map { case (bits, wl) => bits.take(widthTake - wl) }, Seq(widthTake), times)
  // approximately = x / (BigInt(1) << widthDrop)
  def dropLow(widthDrop: Int): BitHeap[T] = BitHeap.getHeapFromTable(bitHeaps.zip(weightLows).map { case (bits, wl) => bits.drop(widthDrop - wl) }, Seq(widthDrop), times)

  def d(pipeline: T => T): BitHeap[T] = BitHeap(bitHeaps.zip(weightLows.zip(times)).map { case (bitHeap, (weightLow, time)) => BitHeapConfigInfo(bitHeap.map(_.map(pipeline)), weightLow, time) }: _*) // pipeline the whole bit heap

  def heights = bitHeaps.map(_.map(_.length).toSeq)

  def height = bitHeaps.map(_.map(_.length).max).max // height of the bit heap, compression ends when height is under some bound

  def widths = bitHeaps.map(_.length) // number of columns in each bit heap

  def width = widths.max

  def weightHighs = weightLows.zip(widths).map { case (wgt, wd) => wgt + wd - 1 }

  def bitsCount = bitHeaps.map(_.map(_.length).sum).sum

  def weightLow = weightLows.min

  def maxValue = heights.zip(weightLows).map { case (height, low) => height.zipWithIndex.map { case (count, weight) => (BigInt(1) << (weight + low - weightLow)) * count }.sum }.sum

  def isEmpty = bitHeaps.forall(_.forall(_.isEmpty))

  def currentTime = times.min

  // find the available bitHeap at current time
  def currentIndex = times.indexWhere(_ == currentTime)

  def currentBitHeap = bitHeaps(currentIndex)

  def currentHeights = heights(currentIndex)

  def currentHeight = currentHeights.max

  def currentWidth = widths(currentIndex)

  def currentWeightLow = weightLows(currentIndex)

  def currentHigh = currentWeightLow + currentWidth - 1

  def currentIsEmpty = currentBitHeap.forall(_.isEmpty)

  def currentBitCount = currentHeights.sum

  def currentMaxValue = currentHeights.zipWithIndex.map { case (count, weight) => (BigInt(1) << weight) * count }.sum

  /** -------- methods for compression-------- */

  // these methods are designed for compress tree implement
  def implCompressOnce(compressors: Seq[Compressor], compressorSolution: CompressorSolution): BitHeap[Bool] = {
    val compressorInSolution = compressors.find(_.name == compressorSolution.compressorName).get
    val inputTable = compressorInSolution
      .inputFormat(compressorSolution.width)                   // remove and get bits in each columns that you need
      .zip(currentBitHeap.drop(compressorSolution.startIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice       = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }
    val heapIn = BitHeap.getHeapFromTable(Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]), Seq(compressorSolution.startIndex + currentWeightLow), Seq(currentTime))
    compressorInSolution.impl(heapIn, compressorSolution.width)

  }

  def implCompressOneStage(compressors: Seq[Compressor], stageSolution: StageSolution, pipeline: Bool => Bool): BitHeap[Bool] = {
    val results = ArrayBuffer[BitHeap[Bool]]()
    stageSolution.compressorSolutions.foreach(compressorSolution => results += implCompressOnce(compressors, compressorSolution))
    val partialNextStage = results
      .reduce(_ + _)
      .d(pipeline)
    this.bitHeaps.remove(currentIndex)
    this.weightLows.remove(currentIndex)
    this.times.remove(currentIndex)
    this.asInstanceOf[BitHeap[Bool]] + partialNextStage
  }

  def implCompressTree(compressors: Seq[Compressor], compressTreeSolution: CompressTreeSolution, pipeline: Bool => Bool, name: String): BitHeap[Bool] = {
    logger.info(s"begin to implement the hardware compress tree of $name")
    var currentHeap = this
    compressTreeSolution.solutions.foreach(stageSolution => currentHeap = currentHeap.implCompressOneStage(compressors, stageSolution, pipeline).asInstanceOf[BitHeap[T]])
    currentHeap.asInstanceOf[BitHeap[Bool]]
  }

  /** get the exact(rather than maximum) efficiency of a compressor applied on current bit heap
    *
    * @param columnIndex
    *   index of column with lowest weight covered by the compressor in heights
    */
  def getExactReductionEfficiency(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }    // overlap
      .sum

    (bits -                                // bitsIn
      compressor.outputBitsCount(width)) / // bitsOut
      compressor.areaCost(width).toDouble  // divided by cost
  }

  def getExactReductionRatio(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }
      .sum // overlap

    bits.toDouble / compressor.outputBitsCount(width)
  }

  def getExactBitReduction(
      compressor: Compressor,
      width: Int,
      columnIndex: Int
  ): Int = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }
      .sum // overlap

    bits - compressor.outputBitsCount(width)
  }

  /** get the most efficient compressor for current bit heap and do compression with it
    *
    * @param compressors
    *   a list of available compressors, the first one must be a 1 to 1 compressor which won't do compression
    * @param finalStage
    *   the this flag is set, a lower efficiency threshold will be used
    * @return
    *   new bit heap generated by the compressor, and the LUT cost
    */
  def compressOneTime(
      compressors: Seq[Compressor],
      finalStage: Boolean,
      headBound: Double,
      tailBound: Double,
      naiveStrategy: Boolean = true
  ): (BitHeap[T], CompressorSolution) = {

    // TODO: better strategies on final stages

    val searchThreshold = 0.2
    val effBound        = if (finalStage) tailBound else headBound
    // when no qualified compressor can be found, the 1 to 1 compressor(no compression) will be chosen
    var bestCompressor          = compressors.head
    var bestReductionEfficiency = 0.0
    val columnIndex             = currentHeights.indexWhere(_ == currentHeights.max) // find the first(lowest weight) column with maximum height
    // number of continuous nonempty columns that 1 to 1 compressor can be applied on
    var bestWidth          = currentBitHeap.drop(columnIndex).takeWhile(_.nonEmpty).length
    var bestReductionRatio = 1
    // sort by efficiency, high to low, besides the 1 to 1 compressor which appear as head
    val candidates = compressors.tail
      .sortBy(compressor =>
        if (finalStage) compressor.heightReduction((currentWidth - columnIndex) min compressor.widthMax)
        else compressor.reductionRatio((currentWidth - columnIndex) min compressor.widthMax)
      )
      .reverse

    if (naiveStrategy) {
      candidates.foreach { compressor => // traverse all available compressors
        val widthMax   = compressor.widthMax min (currentWidth - columnIndex)
        val maximumEff = compressor.reductionEfficiency(widthMax)
        if (maximumEff >= bestReductionEfficiency) // skip when ideal efficiency is lower than current best efficiency
          {
            val (exactEfficiency, width) = {
              if (compressor.isFixed) {
                (getExactReductionEfficiency(compressor, -1, columnIndex), -1) // for GPC, get eff
              } else {                                                         // for row compressor, try different widths, get the best one with its width
                // TODO: avoid trying all widths
                if (widthMax >= compressor.widthMin)
                  (compressor.widthMin to widthMax).map(w => (getExactReductionEfficiency(compressor, w, columnIndex), w)).maxBy(_._1)
                else (-1.0, 0) // skip
              }
            }
            if (exactEfficiency >= bestReductionEfficiency && (exactEfficiency >= effBound)) { // update if a better compressor is found
              bestReductionEfficiency = exactEfficiency
              bestWidth               = width
              bestCompressor          = compressor
            }
          }
      }
    } else {
      candidates.foreach { compressor =>
        val widthMax = compressor.widthMax min (currentWidth - columnIndex)
        if (compressor.isFixed) {
          val exactEfficiency     = getExactReductionEfficiency(compressor, -1, columnIndex)
          val exactReductionRatio = getExactReductionRatio(compressor, -1, columnIndex)
          if (exactEfficiency >= bestReductionEfficiency && exactEfficiency >= effBound) {
            if (exactEfficiency == bestReductionEfficiency) {
              if (exactReductionRatio > bestReductionRatio) {
                bestReductionEfficiency = exactEfficiency
                bestWidth               = -1
                bestCompressor          = compressor
              }
            } else {
              bestReductionEfficiency = exactEfficiency
              bestWidth               = -1
              bestCompressor          = compressor
            }
          }
        } else {
          if (widthMax >= compressor.widthMin) {
            var searchWidth = widthMax
            while (searchWidth >= compressor.widthMin) {
              val exactEfficiency     = getExactReductionEfficiency(compressor, searchWidth, columnIndex)
              val exactReductionRatio = getExactReductionRatio(compressor, searchWidth, columnIndex)
              if (exactEfficiency >= bestReductionEfficiency && exactEfficiency >= effBound) {
                if (exactEfficiency == bestReductionEfficiency) {
                  if (exactReductionRatio > bestReductionRatio) {
                    bestReductionEfficiency = exactEfficiency
                    bestWidth               = searchWidth
                    bestCompressor          = compressor
                  }
                } else {
                  bestReductionEfficiency = exactEfficiency
                  bestWidth               = searchWidth
                  bestCompressor          = compressor
                }
              }
              if ((exactEfficiency + searchThreshold) < effBound)
                searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
              else
                searchWidth -= 1
            }
          }
        }
      }
    }

    val inputTable = bestCompressor
      .inputFormat(bestWidth)                // remove and get bits in each columns that you need
      .zip(currentBitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice       = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }
    val heapIn = BitHeap.getHeapFromTable(Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]), Seq(columnIndex + currentWeightLow), Seq(currentTime))
    //    logger.info(heapIn.toString)
    val heapOut  = getHeapFromHeights(Seq(bestCompressor.outputFormat(bestWidth)), Seq(columnIndex + currentWeightLow), Seq(currentTime + 1)).asInstanceOf[BitHeap[T]]
    val areaCost = bestCompressor.areaCost(bestWidth)
    val currentCompressorSolution = CompressorSolution(
      bestCompressor.name,
      bestWidth,
      columnIndex,
      Consideration(areaCost, bestReductionEfficiency, heapIn.bitsCount.toDouble / heapOut.bitsCount, heapIn.bitsCount - heapOut.bitsCount, heapIn.height - heapOut.height)
    )
    (heapOut, currentCompressorSolution)
  }

  /** do compression until all bits are covered and go to next stage
    *
    * @return
    *   new heap for the next stage, and the LUT cost
    */
  def compressOneStage(
      compressors: Seq[Compressor],
      finalStage: Boolean,
      headBound: Double,
      tailBound: Double,
      bitRatioTarget: Double
  ): (BitHeap[T], StageSolution) = {
    val currentBitCountBefore = currentBitCount
    var currentBitCountAfter  = currentBitCountBefore
    val currentHeightBefore   = currentHeight
    var currentHeightAfter    = currentHeightBefore

    val heightBefore = height
    //    val mark         = currentBitHeap.filter(_.nonEmpty).head.head // for type match

    var stageAreaCost       = 0
    val results             = ArrayBuffer[BitHeap[T]]()
    val compressorTypes     = Set[String]()
    val compressorSolutions = ArrayBuffer[CompressorSolution]()
    // compress until all bits are covered

    while (!currentIsEmpty) {
      val (heap, compressorSolution) = {
        compressOneTime(compressors, finalStage, headBound, tailBound)
      }
      results += heap
      stageAreaCost += compressorSolution.getAreaCost
      compressorTypes += compressorSolution.compressorName
      compressorSolutions += compressorSolution
    }

    val partialNextStage = results
      .asInstanceOf[Seq[BitHeap[T]]]
      .reduce(_ + _)
      .asInstanceOf[BitHeap[T]] // when T is Bool
    this.bitHeaps.remove(currentIndex)
    this.weightLows.remove(currentIndex)
    this.times.remove(currentIndex)
    currentBitCountAfter = partialNextStage.bitsCount
    currentHeightAfter   = partialNextStage.height
    val nextStage = this + partialNextStage

    val stageBitReduction        = currentBitCountBefore - currentBitCountAfter
    val stageReductionRatio      = currentBitCountBefore.toDouble / currentBitCountAfter
    val stageReductionEfficiency = (currentBitCountBefore - currentBitCountAfter).toDouble / stageAreaCost
    val stageHeightReduction     = currentHeightBefore - currentHeightAfter
    if (verbose >= 1 && stageReductionRatio >= bitRatioTarget)
      logger.info(
        s"compressed info :\n\tstage bit reduction: $stageBitReduction, stage reduction efficiency: $stageReductionEfficiency, stage reduction ratio: $stageReductionRatio" +
          s"\n\tarea cost: $stageAreaCost, height: $currentHeightBefore -> $currentHeightAfter" +
          s"\n\tcompressors used: ${compressorTypes.mkString(",")}" +
          s"\n\twhole info :\n\theight: $heightBefore -> ${nextStage.height}, bits remained: ${nextStage.bitsCount}"
      )
    if (finalStage && verbose >= 1 && stageReductionRatio >= bitRatioTarget) logger.info(s"\n${nextStage.toString}")
    (
      nextStage,
      StageSolution(
        compressorSolutions,
        Consideration(stageAreaCost, stageReductionEfficiency, stageReductionRatio, stageBitReduction, stageHeightReduction),
        StageInfo(
          s"$currentHeightBefore -> $currentHeightAfter",
          s"$heightBefore -> ${nextStage.height}",
          Seq.tabulate(nextStage.bitHeaps.length)(i => BitHeapConfigInfo(nextStage.bitHeaps(i).map(_.map(_ => 0)), nextStage.weightLows(i), nextStage.times(i))),
          finalStage
        )
      )
    )
  }

  /** do compression until there's no more than two lines in the bit heap
    *
    * @return
    *   final bit heap and the key information of the compressor tree (latency, widthOut, etc.)
    */
  def compressAll(
      candidates: Seq[Compressor],
      name: String                 = "compressor tree of temp",
      solutionsPath: String        = solutionsPath,
      useHistorySolutions: Boolean = true
  ): (BitHeap[T], CompressTreeSolution) = {
    if (verbose >= 1) {
      logger.info(
        s"\n----available compressors----"
          + s"\n\t${candidates.map(compressor => compressor.getClass.getSimpleName.init + "\n" + compressor.toString(8)).mkString("\n\t")}"
      )
    }
    if (verbose >= 1 && name != null) {
      logger.info(s"find solutions of compressor tree of $name")
    }
    if (verbose >= 1)
      logger.info(s"initial state: bits in total: $bitsCount, height: $height")
    if (verbose >= 1) logger.info(s"\n$toString")

    implicit val formats: DefaultFormats.type = DefaultFormats
    val parentDir                             = new File(solutionsPath)
    if (!parentDir.exists()) { parentDir.mkdirs() }
    val solutionsFile = new File(solutionsPath, s"${this.toString.hashCode()}")
    if (solutionsFile.exists() && useHistorySolutions) {
      val solutionsReader      = new BufferedReader(new FileReader(solutionsFile))
      val compressTreeSolution = read[CompressTreeSolution](solutionsReader.readLine())
      if (verbose >= 1) logger.info(s"Find a history solution in path: $solutionsPath/${this.toString.hashCode()}, load this solution.")
      compressTreeSolution.printLog(srcBitHeap = this.asInstanceOf[BitHeap[Int]])
      (if (compressTreeSolution.getFinalBitHeap != null) compressTreeSolution.getFinalBitHeap.asInstanceOf[BitHeap[T]] else this, compressTreeSolution)
    } else {
      val bitsInTotal             = this.bitsCount
      val maxValue                = this.maxValue
      var current                 = this
      var latency                 = 0
      var badLatency              = 0
      var allCost                 = 0
      val stageSolutions          = ArrayBuffer[StageSolution]()
      var headBound               = 1.0
      var tailBound               = 0.0
      val candidateStageSolutions = ArrayBuffer[StageSolution]()
      while ((current.height > 2 || current.bitHeaps.length > 1) && latency < 100) {
        val currentBefore = current.copy
        val finalStage    = current.currentHeight <= 6
        val (heap, stageSolution) =
          current.compressOneStage(candidates, finalStage, headBound, tailBound, bitRatioTarget = 0.2)
        if (stageSolution.getReductionRatio > 0.2) {
          current = heap
          allCost += stageSolution.getAreaCost
          latency += 1
          stageSolutions += stageSolution
          if (finalStage) badLatency += 1
        } else {
          logger.info(s"fail stage ratio: ${stageSolution.getReductionRatio}")
          if (headBound > 0.2) headBound -= 0.1
          current = currentBefore
        }
      }
      val allCompressed = bitsInTotal - current.bitsCount
      if (verbose >= 1) {
        logger.info(
          s"\n----efficiency report of bit heap compressor----" +
            s"\n\tcost in total: $allCost, compressed in total: $allCompressed" +
            s"\n\tefficiency in total: ${allCompressed.toDouble / allCost}" +
            s"\n\tideal widthOut: ${maxValue.bitLength}, actual widthOut: ${current.widths.max}"
        )
      }

      val compressTreeSolution = CompressTreeSolution(stageSolutions)
      val solutionsWriter      = new BufferedWriter(new FileWriter(solutionsFile))
      solutionsWriter.write(write(compressTreeSolution))
      solutionsWriter.flush()
      solutionsWriter.close()
      if (verbose >= 1) logger.info(s"Store a solution to path : $solutionsPath/${this.toString.hashCode()}")

      (current, compressTreeSolution)
    }

  }

  def output(zero: () => T): ArrayBuffer[ArrayBuffer[T]] = {
    require(height <= 2 && bitHeaps.length <= 1, s"Output style illegal, height: $height\tnumber of delayed bitHeap: ${bitHeaps.length}")
    bitHeaps.head.map(_.padTo(2, zero())).transpose
  }

  override def toString = {
    val dotDiagram = ArrayBuffer[String]()
    bitHeaps.zip(times.zip(weightLows)).foreach { case (bitHeap, (time, weightLow)) =>
      val singleBitHeap = BitHeap(BitHeapConfigInfo(bitHeap, weightLow, time))
      val width         = log2Up(singleBitHeap.maxValue)
      val heights       = singleBitHeap.heights.head
      dotDiagram += s"time = $time ; weightLow = $weightLow\n" + heights
        .padTo(width, 0)
        .map(columnHeight => Seq.fill(columnHeight)("\u25A0").padTo(singleBitHeap.height, " "))
        .reverse
        .transpose
        .map(_.mkString(" "))
        .mkString("\n")
    }
    dotDiagram.mkString("\n")
  }

  def copy = BitHeap(bitHeaps.map(_.map(_.map(i => i))), weightLows, times)

}

object BitHeap {
  def getHeapFromTable[T](
      tables: Seq[Seq[Seq[T]]],
      weightLows: Seq[Int],
      times: Seq[Int]
  ): BitHeap[T] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[T]]()
    times.zip(tables).zip(weightLows).foreach { case ((time, table), weightLow) =>
      val tableForHeap = ArrayBuffer.fill(table.length)(ArrayBuffer[T]())
      tableForHeap.zip(table).foreach { case (buf, seq) => buf ++= seq }
      bitHeapConfigInfos += BitHeapConfigInfo(tableForHeap, weightLow, time)
    }
    BitHeap(bitHeapConfigInfos: _*)
  }

  /** build bit matrix from operands and their shifts
    *
    * @param infos
    *   infos record the width and position(shift) info of corresponding operands
    * @param operands
    *   an operand is a low to high sequence of bits
    */
  def getHeapFromInfos[T](
      infos: Seq[Seq[OperandInfo]],
      operands: Seq[Seq[Seq[T]]] = null
  ): BitHeap[T] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[T]]()
    val realOperands       = if (operands == null) infos.map(_.map(info => Seq.fill(info.width)(0.asInstanceOf[T]))) else operands
    val sortedInfos        = infos.flatten.zip(realOperands.flatten).groupBy(_._1.time).toSeq.map(_._2).map(info => (info.map(_._1), info.map(_._2)))
    sortedInfos.foreach { case (info, operand) =>
      //      require(info.forall(_.time == info.head.time), s"The delay of ${infos.indexOf(info)}th infos is illegal, delay -> ${info.map(_.time).mkString(",")}.")
      // get the width of the table
      val positionHigh = info.map(i => i.weight + i.width - 1).max
      val positionLow  = info.map(_.weight).min
      val width        = positionHigh - positionLow + 1
      // build the table from operands
      val table = ArrayBuffer.fill(width)(ArrayBuffer[T]())
      operand.zip(info).foreach { case (row, inf) =>
        val start = inf.weight - positionLow
        // insert bits from low to high
        (start until start + inf.width).foreach(i => table(i) += row(i - start))
      }
      bitHeapConfigInfos += BitHeapConfigInfo(table, positionLow, info.head.time)
    }
    BitHeap(bitHeapConfigInfos: _*)
  }

  def getHeapFromHeights(
      heights: Seq[Seq[Int]],
      weightLows: Seq[Int],
      times: Seq[Int]
  ): BitHeap[Int] = {
    val bitHeapConfigInfos = ArrayBuffer[BitHeapConfigInfo[Int]]()
    times.zip(heights).zip(weightLows).foreach { case ((time, heights), weightLow) =>
      bitHeapConfigInfos += BitHeapConfigInfo(ArrayBuffer(heights: _*).map(i => ArrayBuffer.fill(i)(0)), weightLow, time)
    }
    BitHeap(bitHeapConfigInfos: _*)
  }

  def getInfosFromBitHeap[T](bitHeap: BitHeap[T]): Seq[Seq[OperandInfo]] = {
    val copyHeap     = bitHeap.bitHeaps.map(_.map(_.map(b => b)))
    val infosForHeap = ArrayBuffer[ArrayBuffer[OperandInfo]]()
    bitHeap.times.zip(copyHeap).zip(bitHeap.weightLows).foreach { case ((time, heap), weightLow) =>
      val infos = ArrayBuffer[OperandInfo]()
      while (!heap.forall(_.isEmpty)) {
        val start  = heap.indexOf(heap.find(_.nonEmpty).get)
        val width  = heap.drop(start).span(_.nonEmpty)._1.length
        val weight = start + weightLow
        infos += OperandInfo(width, weight, positive = true, time)
        Range(start, start + width).foreach { idx => heap(idx) -= heap(idx).head }
      }
      infosForHeap += infos
    }
    infosForHeap
  }

  def apply[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int, time: Int): BitHeap[T] = BitHeap(BitHeapConfigInfo(bitHeap, weightLow, time))

  def apply[T](bitHeaps: Seq[ArrayBuffer[ArrayBuffer[T]]], weightLows: Seq[Int], times: Seq[Int]): BitHeap[T] = BitHeap(bitHeaps.zip(weightLows.zip(times)).map { case (bitHeap, (weightLow, time)) => BitHeapConfigInfo(bitHeap, weightLow, time) }: _*)
}
