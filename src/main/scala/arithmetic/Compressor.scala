package org.datenlord
package arithmetic

import dfg._

import scala.collection.mutable.ArrayBuffer

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

case class BitMatrix[T](table: ArrayBuffer[ArrayBuffer[T]], shift: Int) {

  def +(that: BitMatrix[T]) = {
    val newLow = this.shift min that.shift
    val newHigh = (this.table.length + this.shift) max (that.table.length + that.shift)
    val newWidth = newHigh - newLow
    val newTable = ArrayBuffer.fill(newWidth)(ArrayBuffer[T]())
    newTable.drop(this.shift - newLow).zip(this.table).foreach { case (a, b) => a ++= b }
    newTable.drop(that.shift - newLow).zip(that.table).foreach { case (a, b) => a ++= b }
    BitMatrix(newTable, newLow)
  }

  def height = table.map(_.length).max

  def compressOnce(compressor: Seq[Seq[T]] => Seq[T], baseWidth: Int) = {
    println(s"after compression $this")
    val operands = ArrayBuffer[Seq[T]]()
    val infos = ArrayBuffer[ArithInfo]()
    while (height >= 3) {
      val start = table.indexWhere(_.length >= 3)
      val slice = table.drop(start).takeWhile(_.length >= 3).take(baseWidth)
      val width = slice.length + 2
      val inputs: Seq[ArrayBuffer[T]] = slice.map(_.take(3))
      slice.zip(inputs).foreach { case (whole, part) => whole --= part }
      println(s"after remove $this")
      val operand = compressor(inputs.transpose)
      operands += operand
      infos += ArithInfo(width, start)
    }
    BitMatrix.build(operands, infos) + this
  }

  def compress(compressor: Seq[Seq[T]] => Seq[T], baseWidth: Int) = {
    var current = this
    while (current.height >= 3) current = current.compressOnce(compressor, baseWidth)
    current
  }

  override def toString = s"table:\n${table.map(_.length).mkString(",")}"

}

object BitMatrix {

  /**
   * @param operands An operand is a low to high sequence of bits
   */
  def build[T](operands: Seq[Seq[T]], infos: Seq[ArithInfo]) = {

    val positionHigh = infos.map(info => info.shift + info.width).max
    val positionLow = infos.map(_.shift).min
    val width = positionHigh - positionLow

    val table = ArrayBuffer.fill(width)(ArrayBuffer[T]())
    operands.zip(infos).foreach { case (operand, info) =>
      val start = info.shift - positionLow
      (start until start + info.width).foreach { i => // low to high
        table(i) += operand(i - start)
      }
    }

    BitMatrix(table, positionLow)
  }

  def main(args: Array[String]): Unit = {
    def compressor = (dataIn: Seq[Seq[Char]]) => {
      require(dataIn.length == 3)
      val width = dataIn.head.length
      require(width <= 2, s"width: $width")
      dataIn.map(_.mkString("").reverse).map(BigInt(_, 2)).sum.toString(2).toCharArray.toSeq.reverse.padTo(width + 2, '0')
    }

    val operands: Seq[Seq[Char]] = Seq(13, 12, 11, 13, 12, 11).map(_.toBinaryString.toCharArray.toSeq.reverse.padTo(4, '0'))
    val infos = Seq(0, 0, 0, 1, 2, 3).map(ArithInfo(4, _))

    val original = BitMatrix.build(operands, infos)
    val ret = original.compress(compressor, 2)
    ret.table.foreach(col => if(col.length < 2) col += '0')
    val golden = operands.zip(infos).map{ case (chars, info) => BigInt(chars.reverse.mkString(""), 2) * (1 << info.shift)}.sum
    println(golden)
    val yours = ret.table.transpose.map(chars => BigInt(chars.reverse.mkString(""), 2)).sum
    println(yours)

  }
}
