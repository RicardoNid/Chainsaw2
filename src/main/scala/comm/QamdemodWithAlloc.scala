package org.datenlord
package comm

import breeze.math._
import breeze.numerics.sqrt
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** hard decision qamdemod module for QAM2 <-> QAM256, expected to be the same as qamdemod(x, )
 *
 * @param bitAlloc bit allocation fore each subcarrier
 * @param powAlloc power allocation fore each subcarrier
 */
case class QamdemodWithAlloc(bitAlloc: Seq[Int], powAlloc: Seq[Double], numericType: NumericTypeInfo)
  extends ChainsawGenerator {

  override def name = s"qamdemod_${bitAlloc.hashCode()}_${powAlloc.hashCode()}".replace("-", "N")

  val availableList = Seq(2, 4, 6, 8)
  require(bitAlloc.forall(availableList.contains(_)))
  require(powAlloc.forall(_ <= 2.0)) // TODO: proper bound?

  /** --------
   * pre-computation
   * -------- */
  override val impl = (dataIn: Seq[Any]) => {
    val complex = dataIn.asInstanceOf[Seq[Complex]].toArray
    val scaled = complex.zip(powAlloc).map { case (complex, pow) => complex / sqrt(pow) }

    scaled.zip(bitAlloc).map { case (complex, bit) =>
      val ret = QamAlgo.qamdemod(Array(complex), 1 << bit) // demapped integer
      BigInt(ret.head).withWidth(bit) // binary with correct width
    }.reduce(_ + _).map(_.asDigit) // bits
      .map(BigInt(_))
  }

  override var inputTypes = bitAlloc.map(_ => numericType)
  override var outputTypes = Seq.fill(bitAlloc.sum)(UIntInfo(1))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = (bitAlloc.max + 1) / 2

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val realType = inputTypes.head.asSFix

    def genDeterminants(value: SFix, stage: Int, latency: Int, thresholds: Seq[Double]): Seq[Bool] = {
      if (stage == 0) ArrayBuffer[Bool]()
      else {
        val dets = ArrayBuffer[SFix](value)
        val thresholdsSF = thresholds.map(value => SFConstant(value, realType))
        (0 until stage - 1).foreach(i => dets += RegNext(dets.last.abs - thresholdsSF(i)))
        // align pipeline registers
        val ret = dets.map(_.isNegative)
        ret.zipWithIndex.map { case (bool, i) => Delay(bool, latency - 1 - i) }
      }
    }

    def qamdemod(value: ComplexFix, bit: Int, pow: Double, latency: Int): Bits = {
      val (stageReal, stageImag) = ((bit + 1) / 2, bit / 2)
      val rms = QamAlgo.getRms(1 << bit)
      val allThresholds = (0 until stageReal - 1).map(i => (1 << (i + 1)) / rms * sqrt(pow))
      val bits = (genDeterminants(value.real, stageReal, latency, allThresholds) ++ genDeterminants(value.imag, stageImag, latency, allThresholds)).reverse.asBits()
      RegNext(~bits.msb ## bits(bit - 2 downto 0))
    }

    val segments = complexDataIn.zip(bitAlloc.zip(powAlloc)).map { case (complex, (bit, pow)) => qamdemod(complex, bit, pow, latency) }
    val ret = segments.reduce(_ ## _)

    dataOut := ret.subdivideIn(1 bits).reverse
  }
}