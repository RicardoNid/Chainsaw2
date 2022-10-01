package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class GpcsTest extends AnyFlatSpec {

  /** wrapper of a compressor, used by the functional test function
   */
  def getModule(compressor: Compressor[Bool], width: Int, pipeline: Bool => Bool = bool => bool) =
    new Module {
      val inputShape = compressor.inputFormat(width)
      val outputShape = compressor.outputFormat(width)

      val dataIn = in Bits (inputShape.sum bits)
      val dataOut = out Bits (outputShape.sum bits)

      val bitHeap = ArrayBuffer.fill(inputShape.length)(ArrayBuffer[Bool]())
      val bits = ArrayBuffer(dataIn.asBools.reverse: _*)
      inputShape.zip(bitHeap).foreach { case (i, container) =>
        val column = bits.take(i)
        container ++= column
        bits --= column
      }
      val ret = compressor.impl(BitHeap(bitHeap, 0).d(pipeline), width)
      dataOut := ret.d(pipeline).bitHeap.reverse.flatten.asBits()
    }

  def compressorFuncTest(compressor: Compressor[Bool], width: Int = -1, testCount: Int = 1000): Unit = {

    val inputShape = compressor.inputFormat(width)
    val outputShape = compressor.outputFormat(width)

    def getValueByShape(bigInt: BigInt, shape: Seq[Int]) = {
      val bits = ArrayBuffer(bigInt.toString(2).padToLeft(shape.sum, '0').map(_.asDigit): _*)
      shape.zipWithIndex.map { case (number, weight) =>
        val column = bits.take(number)
        bits --= column
        BigInt(column.sum) << weight
      }.sum
    }

    def getMaxValueByShape(shape: Seq[Int]): Int = {
      shape.zipWithIndex.map { case (height, weight) => height << weight }.sum
    }

    SimConfig.withFstWave
      .compile(getModule(compressor, width))
      .doSim { dut =>
        val data = Seq.fill(testCount)(BigInt(Random.nextInt(getMaxValueByShape(inputShape))))
        data.foreach { value =>
          dut.dataIn #= value
          sleep(1)
          val golden = getValueByShape(value, inputShape)
          val yours = getValueByShape(dut.dataOut.toBigInt, outputShape)
          assert(yours == golden, s"yours: $yours, golden: $golden")
        }
      }
  }

  def compressorPerfTest(compressor: Compressor[Bool], width: Int = -1): Unit =
    VivadoSynth(getModule(compressor, width, RegNext(_)), name = s"compressor$width").require(compressor.utilRequirement(width), compressor.fMaxRequirement)

  behavior of "counter42"

  it should "work" in (1 to Compressor4to2.widthMax by 4).foreach(compressorFuncTest(Compressor4to2, _))
  it should "synth" in (1 to Compressor4to2.widthMax by 4).foreach(compressorPerfTest(Compressor4to2, _))

  behavior of "counter31"

  it should "work" in (1 to Compressor3to1.widthMax by 4).foreach(compressorFuncTest(Compressor3to1, _))
  it should "synth" in (1 to Compressor3to1.widthMax by 4).foreach(compressorPerfTest(Compressor3to1, _))

  behavior of "counter63"

  it should "work" in compressorFuncTest(Compressor6to3)
  it should "synth" in compressorPerfTest(Compressor6to3)

  behavior of "counter32"

  it should "work" in (1 to Compressor3to2.widthMax by 4).foreach(compressorFuncTest(Compressor3to2, _))
  it should "synth" in (1 to Compressor3to2.widthMax by 4).foreach(compressorPerfTest(Compressor3to2, _))

}
