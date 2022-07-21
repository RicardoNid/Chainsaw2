package org.datenlord
package arithmetic

import dfg._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Compressor(infosIn: Seq[ArithInfo]) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infosIn).map { case (int, info) => int << info.shift }.sum
    Seq(ret)
  }

  override val size = (infosIn.length, 2)

  override def latency = ???

  override def implH = ???

}

case class BitMatrixCompressor[T](baseCompressor: Seq[Seq[T]] => Seq[T], baseWidth: Int) {

  def compressOnce(matrix: BitMatrix[T]): (BitMatrix[T], Int) = {
    //    println(s"before compression\n$matrix")
    val table = matrix.table
    val bitsCountBefore = matrix.bitsCount
    val operands = ArrayBuffer[Seq[T]]()
    val infos = ArrayBuffer[ArithInfo]()
    var cost = 0
    while (matrix.height >= 3) {
      // get slice as long as possible
      val start = table.indexWhere(_.length >= 3)
      val strategy = 2
      val slice = if (strategy == 0) {
        val end = table.lastIndexWhere(_.length >= 3)
        table.slice(start, end + 1).take(baseWidth)
      } else if (strategy == 1) {
        val end = table.lastIndexWhere(_.length >= 2)
        table.slice(start, end + 1).take(baseWidth)
      } else {
        table.drop(start).takeWhile(_.length >= 3)
      }

      //      println(s"remove slice $start to $end")
      val width = slice.length + 2
      val inputs: Seq[ArrayBuffer[T]] = slice.map(_.take(3))
      slice.zip(inputs).foreach { case (whole, part) => whole --= part }
      val operand = baseCompressor(inputs)
      operands += operand
      infos += ArithInfo(width, matrix.shift + start)
      cost += slice.length
    }
    //    val sum = BitMatrix.apply(operands, infos)
    //    val remained = matrix
    //    println(s"sum:\n$sum\nremained:$remained")
    val ret = BitMatrix.apply(operands, infos) + matrix
    val bitsCountAfter = ret.bitsCount
    val bitsReduction = bitsCountBefore - bitsCountAfter
    println(s"cost = $cost, bits reduction = ${bitsReduction}, ratio = ${cost.toDouble / bitsReduction}")
    (ret, cost)
  }

  def compressAll(matrix: BitMatrix[T]): (BitMatrix[T], Int) = {
    var current = matrix
    var cost = 0
    while (current.height >= 3) {
      val (matrixNext, costOnce) = compressOnce(current)
      current = matrixNext
      cost += costOnce
    }
    //    println(s"final\n$current")
    (current, cost)
  }

}

// TODO: change the strategy to "as far as possible"
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

  def main(args: Array[String]): Unit = {

    def compressor = (dataIn: Seq[Seq[Char]]) => {
      val width = dataIn.length
      val padded = dataIn.map(_.padTo(3, '0'))
      val sum = padded.transpose.map(operand2BigInt).sum
      bigInt2Operand(sum).padTo(width + 2, '0')
    }

    def bigInt2Operand(value: BigInt) = value.toString(2).toCharArray.toSeq.reverse

    def operand2BigInt(operand: Seq[Char]) = BigInt(operand.mkString("").reverse, 2)

    def testOnce(seed: Int): Unit = {

      Random.setSeed(seed)

      val n = 20
      val width = 377
      val baseWidth = 126
      val operands = (0 until n).map(_ => Random.nextBigInt(width - 1) + (BigInt(1) << (width - 1)))
      //      val shifts = (0 until n).map(_ => Random.nextInt(10))
      val shifts = Seq.fill(n)(0)
      val infos = operands.zip(shifts).map { case (op, shift) => ArithInfo(op.bitLength, shift) }

      val original = BitMatrix(operands.map(bigInt2Operand), infos)
      val bitsCountBefore = original.bitsCount
      val (ret, cost) = BitMatrixCompressor(compressor, baseWidth).compressAll(original)
      val bitsCountAfter = ret.bitsCount
      val paddedRet = ret.table.map(_.padTo(2, '0'))

      val golden = operands.zip(infos).map { case (int, info) => int * (1 << info.shift) }.sum
      val yours = paddedRet.transpose.map(operand2BigInt).sum * (1 << ret.shift)
      //      println(s"golden = $golden, yours = $yours\noperands: ${operands.mkString(" ")}\nshifts: ${shifts.mkString(" ")}")
      assert(golden == yours)
      val bitsReduction = bitsCountBefore - bitsCountAfter
      println(s"bits tobe compressed = $bitsReduction,`` cost = $cost")
    }

    //    (0 until 1000).foreach(testOnce)
    (0 until 1).foreach(testOnce)
  }
}
