package org.datenlord
package arithmetic

import dfg.ArithInfo

import spinal.core.{out, _}
import spinal.lib._

import scala.language.postfixOps

case class BmcConfig(infos: Seq[ArithInfo]) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infos).map { case (int, info) => int << info.shift }.sum
    Seq(ret)
  }

  override val size = (infos.length, 2)

  // TODO: adjustable baseWidth
  val baseWidth = 126
  val fixedLatency = BitMatrix.getLatency(infos, baseWidth)

  override def latency = fixedLatency

  override def implH = BMC(this)

  def compressor: Seq[Seq[Bool]] => Vec[Bool] = (dataIn: Seq[Seq[Bool]]) => {
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

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]) = yours.sum == golden.head

  def naiveImplH: Module = new Module {
    val dataIn = in Vec infos.map(info => UInt(info.width bits))
    val dataOut = out UInt()
    val add = (a: UInt, b: UInt) => a +^ b
    val pipeline = (a: UInt, level: Int) => a.d(1)
    dataOut := dataIn.reduceBalancedTree(add, pipeline)
  }
}

case class BMC(config: BmcConfig)
  extends TransformModule[UInt, UInt] {

  import config._
  logger.info(s"latency of UInt Compressor = $fixedLatency")

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), 2))

  val inOperands = dataIn.fragment.map(_.asBools)
  val bitMatrix: BitMatrix[Bool] = BitMatrix(inOperands, infos)
  val retBitMatrix = BitMatrixCompressor(compressor, pipeline, baseWidth).compressAll(bitMatrix)._1.table.map(_.padTo(2, False))

  val retOperands = Vec(retBitMatrix.transpose.map(_.asBits.asUInt))
  dataOut.fragment := retOperands

  autoValid()
  autoLast()
}
