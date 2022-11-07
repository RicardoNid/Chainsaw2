package org.datenlord
package zprize

import dfg._
import zprize.BitHeap.getHeapFromHeights
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
    require(time == that.time, "add two BitHeapConigInfo must have same time value !")
    val newLow   = weightLow min that.weightLow
    val newHigh  = (weightLow + bitHeap.length - 1) max (that.weightLow + that.bitHeap.length - 1)
    val newWidth = newHigh + 1 - newLow

    // initialization
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    // move bits
    newTable
      .drop(weightLow - newLow) // align
      .zip(bitHeap)
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
  val newBitHeapConfigInfo = bitHeapConfigInfo.groupBy(_.time).toSeq.map { case (_, infos) => infos.reduce(_ + _) }

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
    require(
      this.lastPipeline == that.lastPipeline,
      s"two BitHeap which will to be add should have the same lastPipeline.\tleftHeap's lastPipeline is ${this.lastPipeline}, rightHeap's lastPipeline is ${that.lastPipeline}"
    )
    val leftHeap  = this.copy
    val rightHeap = that.copy
    rightHeap.times.foreach { t =>
      if (leftHeap.times.contains(t)) {
        val retIndex    = leftHeap.times.indexOf(t)
        val sourceIndex = rightHeap.times.indexOf(t)
        // get size of the new table
        val newLow   = leftHeap.weightLows(retIndex) min rightHeap.weightLows(sourceIndex)
        val newHigh  = leftHeap.weightHighs(retIndex) max rightHeap.weightHighs(sourceIndex)
        val newWidth = newHigh + 1 - newLow

        // initialization
        val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
        // move bits
        newTable
          .drop(leftHeap.weightLows(retIndex) - newLow) // align
          .zip(leftHeap.bitHeaps(retIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        newTable
          .drop(rightHeap.weightLows(sourceIndex) - newLow) // align
          .zip(rightHeap.bitHeaps(sourceIndex))
          .foreach { case (a, b) => a ++= b } // move bits
        leftHeap.bitHeaps(retIndex)   = newTable
        leftHeap.weightLows(retIndex) = newLow
        leftHeap.times(retIndex)      = t
      } else {
        // append new bitHeap
        val sourceIndex = rightHeap.times.indexOf(t)
        leftHeap.bitHeaps += rightHeap.bitHeaps(sourceIndex)
        leftHeap.weightLows += rightHeap.weightLows(sourceIndex)
        leftHeap.times += rightHeap.times(sourceIndex)
      }
    }
    val retHeap = leftHeap.copy
    retHeap.lastPipeline = this.lastPipeline
    retHeap
  }

  // add a row on bit heap, in-place operation, the row must be aligned this bit heap
  def addConstant(row: Seq[T], time: Int, zero: T) = {
    require(times.contains(time), s"The source BitHeap isn't contain the delay : $time, please check it !")
    val retIndex = times.indexOf(time)
    val overflow = row.length - widths(retIndex)
    if (overflow > 0) bitHeaps(retIndex) ++= Seq.fill(overflow)(ArrayBuffer[T]())
    bitHeaps(retIndex).zip(row).foreach { case (column, bit) => if (bit != zero) column += bit }
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

  def finalStage = currentTime == times.max

  def isPipeline = if (finalStage) !lastPipeline else lastPipeline

  var lastPipeline = true

  var enterFinalStage = times.length <= 1

  /** -------- methods for compression-------- */

  // these methods are designed for compress tree implement
  def implCompressOnce(compressors: Seq[Compressor], compressorSolution: CompressorSolution, isPipeline: Boolean): BitHeap[Bool] = {
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
    val heapIn = BitHeap.getHeapFromTable(
      Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]),
      Seq(compressorSolution.startIndex + currentWeightLow),
      Seq(if (isPipeline) currentTime + 1 else currentTime)
    )
    compressorInSolution.impl(heapIn, compressorSolution.width)

  }

  def implCompressOneStage(compressors: Seq[Compressor], stageSolution: StageSolution, pipeline: Bool => Bool): BitHeap[Bool] = {
    val results = ArrayBuffer[BitHeap[Bool]]()
    stageSolution.compressorSolutions.foreach(compressorSolution => results += implCompressOnce(compressors, compressorSolution, stageSolution.isPipeline))
    val partialNextStage = results
      .reduce(_ + _)
      .d(if (stageSolution.isPipeline) pipeline else b => b)
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
                                   considerCarry8: Boolean,
                                   width: Int,
                                   columnIndex: Int
                                 ): Double = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }    // overlap
      .sum

    (bits -                                                  // bitsIn
      compressor.outputBitsCount(width)) /                   // bitsOut
      compressor.areaCost(width, considerCarry8, isPipeline) // divided by cost
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

  def getExactHeightReduction(
                               compressor: Compressor,
                               width: Int,
                               columnIndex: Int
                             ): Int = {
    // for a given compressor and a column, find the exact number of bits covered by the compressor
    val bits = compressor
      .inputFormat(width)                    // height of columns in input pattern
      .zip(currentHeights.drop(columnIndex)) // zip with height of columns in this heap
      .map { case (h0, h1) => h0 min h1 }

    bits.max - compressor.outputFormat(width).max
  }

  def headStrategy(
                    compressors: Seq[Compressor],
                    considerCarry8: Boolean,
                    headBound: Double
                  ) = {
    val searchThreshold = 0.2
    val effBound        = headBound

    val columnIndex             = currentHeights.indexWhere(_ == currentHeights.max) // find the first(lowest weight) column with maximum height
    var bestCompressor          = compressors.head
    var bestWidth               = currentBitHeap.drop(columnIndex).takeWhile(_.nonEmpty).length
    var bestReductionEfficiency = bestCompressor.reductionEfficiency(bestWidth, considerCarry8)
    var bestReductionRatio      = bestCompressor.reductionRatio(bestWidth)

    val candidates = compressors.tail
      .sortBy(compressor => compressor.reductionEfficiency((currentWidth - columnIndex) min compressor.widthMax, considerCarry8))
      .reverse

    candidates.foreach { compressor =>
      val widthMax = compressor.widthMax min (currentWidth - columnIndex)
      if (compressor.isFixed) {
        val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = -1, columnIndex)
        val exactReductionRatio      = getExactReductionRatio(compressor, width = -1, columnIndex = columnIndex)
        if (exactReductionEfficiency >= bestReductionEfficiency && exactReductionEfficiency >= effBound) {
          if (exactReductionEfficiency == bestReductionEfficiency) {
            if (exactReductionRatio > bestReductionRatio) {
              bestReductionEfficiency = exactReductionEfficiency
              bestWidth               = -1
              bestCompressor          = compressor
              bestReductionRatio      = exactReductionRatio
            }
          } else {
            bestReductionEfficiency = exactReductionEfficiency
            bestWidth               = -1
            bestCompressor          = compressor
            bestReductionRatio      = exactReductionRatio
          }
        }
      } else {
        if (widthMax >= compressor.widthMin) {
          var searchWidth = widthMax
          while (searchWidth >= compressor.widthMin) {
            val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = searchWidth, columnIndex)
            val exactReductionRatio      = getExactReductionRatio(compressor, searchWidth, columnIndex)
            if (exactReductionEfficiency >= bestReductionEfficiency && exactReductionEfficiency >= effBound) {
              if (exactReductionEfficiency == bestReductionEfficiency) {
                if (exactReductionRatio > bestReductionRatio) {
                  bestReductionEfficiency = exactReductionEfficiency
                  bestWidth               = searchWidth
                  bestCompressor          = compressor
                  bestReductionRatio      = exactReductionRatio
                }
              } else {
                bestReductionEfficiency = exactReductionEfficiency
                bestWidth               = searchWidth
                bestCompressor          = compressor
                bestReductionRatio      = exactReductionRatio
              }
            }
            if ((exactReductionEfficiency + searchThreshold) < effBound) {
              searchWidth = 8 * floor((searchWidth - 1) / 8).toInt
            } else
              searchWidth -= 1
          }
        }
      }
    }
    (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency)
  }

  def tailStrategy(
                    compressors: Seq[Compressor],
                    considerCarry8: Boolean,
                    tailBound: Double
                  ) = {

    val effBound                = tailBound
    var bestCompressor          = compressors.head
    var bestWidth               = 1
    var bestReductionEfficiency = bestCompressor.reductionEfficiency(bestWidth, considerCarry8, isPipeline)
    var bestHeightReduction     = 0
    val columnIndex             = currentHeights.indexWhere(_ == currentHeights.max)

    val candidates = compressors.tail.filter(_.isFixed).sortBy(compressor => compressor.reductionEfficiency(width = -1, considerCarry8, isPipeline)).reverse

    candidates.foreach { compressor =>
      val exactReductionEfficiency = getExactReductionEfficiency(compressor, considerCarry8, width = -1, columnIndex)
      val exactHeightReduction     = getExactHeightReduction(compressor, width = -1, columnIndex)
      if (exactHeightReduction >= bestHeightReduction && exactReductionEfficiency >= effBound) {
        if (exactHeightReduction == bestHeightReduction) {
          if (exactReductionEfficiency > bestReductionEfficiency) {
            bestReductionEfficiency = exactReductionEfficiency
            bestCompressor          = compressor
            bestWidth               = -1
            bestHeightReduction     = exactHeightReduction
          }
        } else {
          bestReductionEfficiency = exactReductionEfficiency
          bestCompressor          = compressor
          bestWidth               = -1
          bestHeightReduction     = exactHeightReduction
        }
      }

    }
    (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency)
  }

  /** get the most efficient compressor for current bit heap and do compression with it
   *
   * @param compressors
   *   a list of available compressors, the first one must be a 1 to 1 compressor which won't do compression
   * @return
   *   new bit heap generated by the compressor, and the LUT cost
   */
  def compressOneTime(
                       compressors: Seq[Compressor],
                       considerCarry8: Boolean,
                       headBound: Double,
                       tailBound: Double
                     ): (BitHeap[T], CompressorSolution) = {

    val finalStageBefore = finalStage
    val shouldPipeline   = isPipeline

    val (bestCompressor, bestWidth, columnIndex, bestReductionEfficiency) = if (finalStageBefore) tailStrategy(compressors, considerCarry8, tailBound) else headStrategy(compressors, considerCarry8, headBound)

    val inputTable = bestCompressor
      .inputFormat(bestWidth)                // remove and get bits in each columns that you need
      .zip(currentBitHeap.drop(columnIndex)) // align and zip
      .map { case (number, column) =>
        val exactNumber = column.length min number
        val slice       = column.take(exactNumber) // take the bits need
        column --= slice // remove them from current heap
        slice
      }

    val heapIn   = BitHeap.getHeapFromTable(Seq(inputTable.asInstanceOf[Seq[ArrayBuffer[Bool]]]), Seq(columnIndex + currentWeightLow), Seq(currentTime))
    val heapOut  = getHeapFromHeights(Seq(bestCompressor.outputFormat(bestWidth)), Seq(columnIndex + currentWeightLow), Seq(if (shouldPipeline) currentTime + 1 else currentTime)).asInstanceOf[BitHeap[T]]
    val areaCost = bestCompressor.areaCost(bestWidth, considerCarry8, shouldPipeline)
    val currentCompressorSolution = CompressorSolution(
      bestCompressor.name,
      bestWidth,
      columnIndex,
      Consideration(areaCost, bestReductionEfficiency, heapIn.bitsCount.toDouble / heapOut.bitsCount, heapIn.bitsCount - heapOut.bitsCount, heapIn.height - heapOut.height)
    )
    if (finalStageBefore) heapOut.lastPipeline           = !lastPipeline
    if (currentIsEmpty && finalStageBefore) lastPipeline = !lastPipeline
    if (currentIsEmpty && shouldPipeline && finalStageBefore) this.times.indices.foreach(i => this.times(i) += 1)
    (heapOut, currentCompressorSolution)
  }

  /** do compression until all bits are covered and go to next stage
   *
   * @return
   *   new heap for the next stage, and the LUT cost
   */
  def compressOneStage(
                        compressors: Seq[Compressor],
                        considerCarry8: Boolean           = true,
                        headBound: Double                 = 1.0,
                        tailBound: Double                 = 0.0,
                        bitRatioTarget: Double            = 1.0,
                        reductionEfficiencyTarget: Double = 1.8
                      ): (BitHeap[T], StageSolution, String) = {
    require((!finalStage && lastPipeline) || this.finalStage, s"finalStage: ${this.finalStage}\tlastPipeline: ${this.lastPipeline}")
    val finalStageBefore = finalStage
    var shouldPipeline   = isPipeline

    val currentBitCountBefore = currentBitCount
    var currentBitCountAfter  = currentBitCountBefore
    val currentHeightBefore   = currentHeight
    var currentHeightAfter    = currentHeightBefore
    val heightBefore          = height

    var stageAreaCost       = 0.0
    val results             = ArrayBuffer[BitHeap[T]]()
    val compressorTypes     = Set[String]()
    val compressorSolutions = ArrayBuffer[CompressorSolution]()

    // compress until all bits are covered
    while (!currentIsEmpty) {
      val (heap, compressorSolution) = {
        compressOneTime(compressors, considerCarry8, headBound, tailBound)
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

    if (nextStage.height <= 2 && nextStage.bitHeaps.length <= 1 && !shouldPipeline) {
      shouldPipeline = true
      nextStage.times.indices.foreach(i => nextStage.times(i) += 1)
    }
    nextStage.lastPipeline = shouldPipeline
    val stageBitReduction        = currentBitCountBefore - currentBitCountAfter
    val stageReductionRatio      = currentBitCountBefore.toDouble / currentBitCountAfter
    val stageReductionEfficiency = (currentBitCountBefore - currentBitCountAfter).toDouble / stageAreaCost
    val stageHeightReduction     = currentHeightBefore - currentHeightAfter

    val stageLog = s"compressed info :\n\tstage bit reduction: $stageBitReduction, stage reduction efficiency: $stageReductionEfficiency, stage reduction ratio: $stageReductionRatio" +
      s"\n\tarea cost: $stageAreaCost, height: $currentHeightBefore -> $currentHeightAfter" +
      s"\n\tcompressors used: ${compressorTypes.mkString(",")}" +
      s"\n\twhole info :\n\theight: $heightBefore -> ${nextStage.height}, bits remained: ${nextStage.bitsCount}"

    if (verbose >= 1 && stageReductionRatio >= bitRatioTarget && stageReductionEfficiency >= reductionEfficiencyTarget) logger.info(stageLog)
    if (finalStageBefore && shouldPipeline && verbose >= 1 && stageReductionRatio >= bitRatioTarget && stageReductionEfficiency >= reductionEfficiencyTarget) logger.info(s"\n${nextStage.toString}")
    (
      nextStage,
      StageSolution(
        compressorSolutions,
        Consideration(stageAreaCost, stageReductionEfficiency, stageReductionRatio, stageBitReduction, stageHeightReduction),
        StageInfo(
          s"$currentHeightBefore -> $currentHeightAfter",
          s"$heightBefore -> ${nextStage.height}",
          Seq.tabulate(nextStage.bitHeaps.length)(i => BitHeapConfigInfo(nextStage.bitHeaps(i).map(_.map(_ => 0)), nextStage.weightLows(i), nextStage.times(i))),
          finalStageBefore,
          shouldPipeline,
          shouldPipeline && finalStageBefore
        )
      ),
      stageLog
    )
  }

  /** do compression until there's no more than two lines in the bit heap
   *
   * @return
   *   final bit heap and the key information of the compressor tree (latency, widthOut, etc.)
   */
  def compressAll(
                   candidates: Seq[Compressor],
                   considerCarry8: Boolean      = true,
                   name: String                 = "compressor tree of temp",
                   solutionsPath: String        = solutionsPath,
                   useHistorySolutions: Boolean = false
                 ): (BitHeap[T], CompressTreeSolution) = {
    if (verbose >= 1) {
      if (name != null) logger.info(s"the name of compressor tree is : $name")
      logger.info(
        s"\n----available compressors----"
          + s"\n\t${candidates.map(compressor => compressor.getClass.getSimpleName.init + "\n" + compressor.toString(8)).mkString("\n\t")}"
      )
      logger.info(s"initial state: bits in total: $bitsCount, height: $height")
      if (finalStage) logger.info(s"initial enter finalStage")
      logger.info(s"\n$toString")
    }

    implicit val formats: DefaultFormats.type = DefaultFormats
    val parentDir                             = new File(solutionsPath)
    if (!parentDir.exists()) { parentDir.mkdirs() }
    val solutionsFile = new File(solutionsPath, s"${this.toString.hashCode()}")
    if (solutionsFile.exists() && useHistorySolutions) {
      val solutionsReader      = new BufferedReader(new FileReader(solutionsFile))
      val compressTreeSolution = read[CompressTreeSolution](solutionsReader.readLine())
      logger.info(s"Find a history solution in path: $solutionsPath/${this.toString.hashCode()}, load this solution.")
      compressTreeSolution.printLog(srcBitHeap = this.asInstanceOf[BitHeap[Int]])
      (if (compressTreeSolution.getFinalBitHeap != null) compressTreeSolution.getFinalBitHeap.asInstanceOf[BitHeap[T]] else this, compressTreeSolution)
    } else {
      val bitsInTotal             = this.bitsCount
      val maxValue                = this.maxValue
      var current                 = this
      var latency                 = 0
      var badLatency              = 0
      var allCost                 = 0.0
      val stageSolutions          = ArrayBuffer[StageSolution]()
      var headBound               = 1.5
      var tailBound               = 0.0
      val candidateStageSolutions = ArrayBuffer[StageSolution]()
      val candidateBitHeaps       = ArrayBuffer[BitHeap[T]]()
      val candidateStageLogs      = ArrayBuffer[String]()
      while ((current.height > 2 || current.bitHeaps.length > 1) && latency < 100) {
        val currentBefore = current.copy
        val (heap, stageSolution, stageLog) =
          current.compressOneStage(
            candidates,
            considerCarry8,
            headBound,
            tailBound,
            bitRatioTarget            = if (currentBefore.finalStage) 0.0 else 0.0,
            reductionEfficiencyTarget = if (currentBefore.finalStage) 0.0 else 1.9
          )
        if (stageSolution.getReductionRatio >= (if (currentBefore.finalStage) 0.0 else 0.0) && stageSolution.getReductionEfficiency >= (if (currentBefore.finalStage) 0.0 else 1.9)) {
          headBound = 1.5
          current   = heap
          allCost += stageSolution.getAreaCost
          if (stageSolution.isPipeline) latency += 1
          stageSolutions += stageSolution
          if (currentBefore.finalStage && stageSolution.isPipeline) badLatency += 1
          if (verbose >= 1 && !currentBefore.finalStage && current.finalStage) logger.info(s"enter finalStage")
        } else {
          candidateStageSolutions += stageSolution
          candidateBitHeaps += heap
          candidateStageLogs += stageLog
          if (headBound > 0.2) {
            headBound -= 0.1
            current = currentBefore
          } else {
            headBound = 1.5
            val (finalStageSolution, (finalHeap, stagLog)) = candidateStageSolutions.zip(candidateBitHeaps.zip(candidateStageLogs)).sortBy(_._1.getReductionEfficiency).reverse.head
            allCost += finalStageSolution.getAreaCost
            if (finalStageSolution.isPipeline) latency += 1
            stageSolutions += finalStageSolution
            if (verbose >= 1) logger.info(stagLog)
            if (currentBefore.finalStage && finalStageSolution.isPipeline) {
              badLatency += 1
              logger.info(finalHeap.toString)
            }
            if (verbose >= 1 && !currentBefore.finalStage && current.finalStage) logger.info(s"enter finalStage")
            candidateStageSolutions.clear()
            candidateBitHeaps.clear()
            candidateStageLogs.clear()
            current = finalHeap
          }
        }
      }
      val allCompressed = bitsInTotal - current.bitsCount
      logger.info(
        s"\n----efficiency report of bit heap compressor----" +
          s"\n\tcost in total: $allCost, compressed in total: $allCompressed" +
          s"\n\tefficiency in total: ${allCompressed.toDouble / allCost}" +
          s"\n\tideal widthOut: ${maxValue.bitLength}, actual widthOut: ${current.widths.max}"
      )

      val compressTreeSolution = CompressTreeSolution(stageSolutions)
      val solutionsWriter      = new BufferedWriter(new FileWriter(solutionsFile))
      solutionsWriter.write(write(compressTreeSolution))
      solutionsWriter.flush()
      solutionsWriter.close()
      logger.info(s"Store a solution to path : $solutionsPath/${this.toString.hashCode()}")

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

  def copy = BitHeap(this.bitHeaps.map(_.map(_.map(i => i))), this.weightLows, this.times)

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
        infos += OperandInfo(width, weight, positive = true, time = time)
        Range(start, start + width).foreach { idx => heap(idx) -= heap(idx).head }
      }
      infosForHeap += infos
    }
    infosForHeap
  }

  def apply[T](bitHeap: ArrayBuffer[ArrayBuffer[T]], weightLow: Int, time: Int): BitHeap[T] = BitHeap(BitHeapConfigInfo(bitHeap, weightLow, time))

  def apply[T](bitHeaps: Seq[ArrayBuffer[ArrayBuffer[T]]], weightLows: Seq[Int], times: Seq[Int]): BitHeap[T] = BitHeap(bitHeaps.zip(weightLows.zip(times)).map { case (bitHeap, (weightLow, time)) => BitHeapConfigInfo(bitHeap, weightLow, time) }: _*)
}
