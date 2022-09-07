package org.datenlord
package arithmetic

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class GPCTest extends AnyFlatSpec {

  Random.setSeed(42)

  def getModule(compressor: Compressor[Bool], width: Int) = new Module {

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
    val ret = compressor.impl(BitHeap(bitHeap, 0), width)
    dataOut := ret.bitHeap.reverse.flatten.asBits()
  }

  def compressorFuncTest(compressor: Compressor[Bool], width: Int): Unit = {

    val inputShape = compressor.inputFormat(width)
    val outputShape = compressor.outputFormat(width)

    SimConfig.withFstWave.compile {
      getModule(compressor, width)
    }.doSim { dut =>

      val testCount = 1000
      val data = Seq.fill(testCount)(BigInt(inputShape.sum))

      def getValue(bigInt: BigInt, shape: Seq[Int]) = {
        val bits = ArrayBuffer(bigInt.toString(2).padToLeft(shape.sum, '0').map(_.asDigit): _*)
        shape.zipWithIndex.map { case (number, weight) =>
          val column = bits.take(number)
          bits --= column
          BigInt(column.sum) << weight
        }.sum
      }

      data.foreach { value =>
        dut.dataIn #= value
        sleep(1)
        val golden = getValue(value, inputShape)
        val yours = getValue(dut.dataOut.toBigInt, outputShape)
        assert(yours == golden, s"yours: $yours, golden: $golden")
      }
    }
  }

  def compressorPerfTest(compressor: Compressor[Bool], width: Int): Unit =
    VivadoSynth(getModule(compressor, width), name = s"compressor_$width")
      .require(compressor.utilRequirement(width), 600 MHz)


  behavior of "counter42"

  it should "work" in (1 to Compressor4to2.widthMax by 4).foreach(compressorFuncTest(Compressor4to2, _))
  ignore should "synth" in (1 to Compressor4to2.widthMax by 4).foreach(compressorPerfTest(Compressor4to2, _))

  behavior of "counter63"

  it should "work" in (1 to Compressor6to3.widthMax by 4).foreach(compressorFuncTest(Compressor6to3, _))
  ignore should "synth" in (1 to Compressor6to3.widthMax by 4).foreach(compressorPerfTest(Compressor6to3, _))

  behavior of "counter31"

  it should "work" in (1 to Compressor3to1.widthMax by 4).foreach(compressorFuncTest(Compressor3to1, _))
  ignore should "synth" in (1 to Compressor3to1.widthMax by 4).foreach(compressorPerfTest(Compressor3to1, _))

  behavior of "counter31 another"

  it should "work" in (1 to Compressor3to1NoCarry.widthMax by 4).foreach(compressorFuncTest(Compressor3to1NoCarry, _))
  ignore should "synth" in (1 to Compressor3to1NoCarry.widthMax by 4).foreach(compressorPerfTest(Compressor3to1NoCarry, _))

  behavior of "counter32"

  it should "work" in (1 to Compressor3to2.widthMax by 4).foreach(compressorFuncTest(Compressor3to2, _))
  ignore should "synth" in (1 to Compressor3to2.widthMax by 4).foreach(compressorPerfTest(Compressor3to2, _))

}
