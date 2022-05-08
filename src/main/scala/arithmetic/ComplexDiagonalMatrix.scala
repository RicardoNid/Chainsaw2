package org.datenlord
package arithmetic

import dataFlow._

import breeze.math._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ComplexDiagonalMatrixConfig(coeffs: Seq[Complex], fold: Int,
                                       dataType: HardType[SFix], coeffType: HardType[SFix]) extends TransformConfig {

  val n = coeffs.length
  require(n % fold == 0)
  val portWidth = n / fold

  override def latency = ComplexMult.latency

  override def inputFlow = CyclicFlow(portWidth, fold)

  override def outputFlow = CyclicFlow(portWidth, fold)

  override def complexTransform(dataIn: Seq[Complex]) =
    dataIn.zip(coeffs).map { case (data, coeff) => data * coeff }
}

case class ComplexDiagonalMatrix(config: ComplexDiagonalMatrixConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import config._
  import ComplexMult.complexMult

  val controlType = HardType(Bits(n / 2 bits))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), portWidth))
  val coeffHard = coeffs.map(CF(_, coeffType))
  val currentCoeffs = Vec(HardType(ComplexFix(coeffType)), portWidth)
  currentCoeffs.setName("current")

  if (fold > 1) {
    val coeffROM = Mem(coeffHard.grouped(portWidth).toSeq.map(Vec(_)))
    val counter = CounterFreeRun(fold)
    when(dataIn.last)(counter.clear())
    currentCoeffs := coeffROM.readAsync(counter.value)
  }
  else currentCoeffs := Vec(coeffHard)

  val ret = Vec(dataIn.fragment.zip(currentCoeffs).map { case (data, coeff) => complexMult(data, coeff) })

  override val dataOut = master Flow Fragment(ret)
  dataOut.fragment := ret
  autoValid()
  autoLast()
}
