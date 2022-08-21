package org.datenlord
package arithmetic

import dfg.ArithInfo

import spinal.core.{out, _}
import spinal.lib._

import scala.language.postfixOps

/** the hardware module using [[BitHeap]] model to implement multi operand addition
 * @param infos width and position(shift) of input operands
 */
case class BitHeapCompressorConfig(infos: Seq[ArithInfo]) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = bigInts.zip(infos).map { case (int, info) => int << info.shift }.sum
    Seq(ret)
  }

  override val size = (infos.length, 2)

  val (_, fixedLatency, widthOut) = BitHeap.getFakeHeapFromInfos(infos).compressAll(GPC())

  override def latency = fixedLatency

  override def implH = BitHeapCompressor(this)

  def pipeline(data: Bool) = data.d(1)

  def zero() = False

  def metric(yours: Seq[BigInt], golden: Seq[BigInt]) = yours.sum == golden.head

  def naiveImplH: Module = new Module {
    val dataIn = in Vec infos.map(info => UInt(info.width bits))
    val dataOut = out UInt()
    val add = (a: UInt, b: UInt) => a +^ b
    val pipeline = (a: UInt, level: Int) => a.d(1)
    dataOut := dataIn.reduceBalancedTree(add, pipeline)
  }
}

case class BitHeapCompressor(config: BitHeapCompressorConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  logger.info(s"latency of UInt Compressor = $fixedLatency")

  override val dataIn = slave Flow Fragment(Vec(infos.map(info => UInt(info.width bits))))
  override val dataOut = master Flow Fragment(Vec(UInt(), 2))

  val operands = dataIn.fragment.map(_.asBools)
  val bitHeap = BitHeap.getHeapFromInfos(infos, operands)
  infos.foreach( info => logger.info(s"info: ${info.shift}, ${info.width}"))
  val (ret, _, _) = bitHeap.compressAll(GPC(), pipeline)
  dataOut.fragment := ret.output(zero).map(_.asBits().asUInt)

  //  dataOut.fragment := Vec(op(dataIn.fragment))

  autoValid()
  autoLast()
}
