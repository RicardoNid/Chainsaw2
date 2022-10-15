package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

case class Fft(n: Int, streamWidth: Int, radix: Int, inverse: Boolean, numericType: NumericTypeInfo)
  extends ChainsawGenerator {

  require(isPow2(n) && isPow2(streamWidth))
  require(Seq(2, 4, 8).contains(radix))

  val prefix = if (inverse) "ifft" else "fft"

  override def name = s"fft_n${n}_sw${streamWidth}_r${radix}"

  override val impl = (dataIn: Seq[Any]) => {
    val data = dataIn.asInstanceOf[Seq[Complex]].toArray
    fourierTr.dvComplex1DFFT(DenseVector(data)).toArray.toSeq.map(_ / n)
  }

  val types = Seq.fill(streamWidth)(numericType)
  val frameFormat = MatrixFormat(streamWidth, period = n / streamWidth)

  override var inputTypes = types
  override var outputTypes = types

  override var inputFormat = frameFormat
  override var outputFormat = frameFormat
  override var latency = 74 + 8 // TODO: for our generator

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val core = SpiralFft(n, streamWidth, numericType.bitWidth / 2)
    val coreComplexIn = core.dataIn.grouped(2).toSeq
    val coreComplexOut = core.dataOut.grouped(2).toSeq

    // dataIn -> core
    complexDataIn.zip(coreComplexIn).foreach { case (fix, cores) =>
      cores(0) := fix.real.asBits
      cores(1) := fix.imag.asBits
    }

    // core -> dataOut
    complexDataOut.zip(coreComplexOut).foreach { case (fix, cores) =>
      fix.real.assignFromBits(cores(0))
      fix.imag.assignFromBits(cores(1))
    }

    core.next := (if (lastIn== null) True else lastIn)
  }
}
