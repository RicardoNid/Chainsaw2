package org.datenlord
package arithmetic

import dfg.ArithInfo

import scala.collection.mutable.ArrayBuffer

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
    val ret = BitHeap.getHeapFromInfos(infos, operands) + pipelinedMatrix
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
