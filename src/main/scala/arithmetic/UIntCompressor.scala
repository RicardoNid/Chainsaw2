package org.datenlord
package arithmetic

import dfg.ArithInfo

import spinal.core._
import spinal.lib._

case class UIntCompressorConfig(infos: Seq[ArithInfo]) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infos).map { case (int, info) => int << info.shift }.sum
    Seq(ret)
  }

  override val size = (infos.length, 1)

  // TODO: get latency from infos
  override def latency = 5

  override def implH = UIntCompressor(this)

  val baseWidth = 126
}

case class UIntCompressor(config: UIntCompressorConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), 1))

  val inOperands = dataIn.fragment.map(_.asBools)
  val bitMatrix: BitMatrix[Bool] = BitMatrix(inOperands, infos)

  def compressor = (dataIn: Seq[Seq[Bool]]) => {
    val width = dataIn.length
    val core = device.TernaryAdderConfig(width).implH
    val operands = dataIn
      .map(_.padTo(3, False))
      .transpose
      .map(_.asBits.asUInt.resize(width))
    require(operands.length == 3)
    core.dataIn.fragment := Vec(operands)
    core.skipControl()
    core.dataOut.fragment.asBits.asBools
  }

  def pipeline(data: Bool) = data.d(1)

  val retBitMatrix = BitMatrixCompressor(compressor, pipeline, baseWidth).compressAll(bitMatrix)._1.table.map(_.padTo(2, False))

  //  dataOut.fragment(0) := retBitMatrix.transpose.head.asBits.asUInt
  //  dataOut.fragment(1) := retBitMatrix.transpose.head.asBits.asUInt

  val retOperands = retBitMatrix.transpose.map(_.asBits.asUInt)
  dataOut.fragment.head := (retOperands(0) +^ retOperands(1)).d(1)
  autoValid()
  autoLast()
}
