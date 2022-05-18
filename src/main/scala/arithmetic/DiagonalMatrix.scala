package org.datenlord
package arithmetic

import flowConverters._

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DiagonalMatrixConfig(coeffs: Seq[BigInt], override val spaceFold: Int,
                                bitWidthIn: Int, bitWidthCoeff: Int, bitWidthOut: Int,
                                baseMult: (Bits, Bits) => Bits, baseLatency: Int) extends TransformConfig {

  val N = coeffs.length
  require(N % spaceFold == 0)
  val portWidth = N / spaceFold


  override val size = (N, N)

  override def latency = baseLatency

  override def impl(dataIn: Seq[_]) = dataIn.asInstanceOf[Seq[BigInt]].zip(coeffs).map { case (a, b) => a * b }

  override def implH = DiagonalMatrix(this)
}

case class DiagonalMatrix(config: DiagonalMatrixConfig) extends TransformModule[Bits, Bits] {

  import config._

  val controlType = HardType(Bits(N / 2 bits))

  override val dataIn = slave Flow Fragment(Vec(Bits(bitWidthIn bits), portWidth))
  override val dataOut = master Flow Fragment(Vec(Bits(bitWidthOut bits), portWidth))

  if (spaceFold > 1) {
    val coeffHard = coeffs.map(B(_, bitWidthCoeff bits)).grouped(portWidth).toSeq.map(Vec(_))
    val coeffROM = Mem(coeffHard)

    val counter = CounterFreeRun(spaceFold)
    when(dataIn.last)(counter.clear())

    val currentCoeffs = coeffROM.readAsync(counter.value)
    dataOut.fragment := Vec(dataIn.fragment.zip(currentCoeffs).map { case (data, coeff) => baseMult(data, coeff) })
  }
  else dataOut.fragment := Vec(dataIn.fragment.zip(coeffs).map { case (data, coeff) => baseMult(data, coeff) }.map(_.resize(bitWidthOut)))

  autoValid()
  autoLast()
}
