package org.datenlord
package arithmetic

import org.datenlord.dfg.ArithInfo
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.assert

import scala.util.Random

class BitMatrixTest extends AnyFlatSpec {

  def compressor = (dataIn: Seq[Seq[Char]]) => {
    val width = dataIn.length
    val padded = dataIn.map(_.padTo(3, '0'))
    val sum = padded.transpose.map(operand2BigInt).sum
    bigInt2Operand(sum).padTo(width + 2, '0')
  }

  def pipeline = (data: Char) => data

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
    val (ret, cost, latency) = BitMatrixCompressor(compressor, pipeline, baseWidth).compressAll(original)
    val bitsCountAfter = ret.bitsCount
    val paddedRet = ret.table.map(_.padTo(2, '0'))

    val golden = operands.zip(infos).map { case (int, info) => int * (1 << info.shift) }.sum
    val yours = paddedRet.transpose.map(operand2BigInt).sum * (1 << ret.shift)
    //      println(s"golden = $golden, yours = $yours\noperands: ${operands.mkString(" ")}\nshifts: ${shifts.mkString(" ")}")
    assert(golden == yours)
    val bitsReduction = bitsCountBefore - bitsCountAfter
    println(s"bits tobe compressed = $bitsReduction, cost = $cost, ration = ${cost.toDouble / bitsReduction}")
  }

  "Bit Matrix Compressor" should "work" in (0 until 1000).foreach(testOnce)

}
