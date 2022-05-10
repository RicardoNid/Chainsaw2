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

  if (portWidth * coeffType.getBitsWidth * 2 > 4096) println(s"too big signal")


  if (fold > 1) {

    val counter = CounterFreeRun(fold)
    when(dataIn.last)(counter.clear())

    val coeff0 = coeffHard.grouped(portWidth).toSeq.map(_.take(portWidth / 2)).map(Vec(_))
    val coeff1 = coeffHard.grouped(portWidth).toSeq.map(_.takeRight(portWidth / 2)).map(Vec(_))
    val rom0 = Mem(coeff0)
    val rom1 = Mem(coeff1)
    currentCoeffs := Vec(rom0.readAsync(counter.value) ++ rom1.readAsync(counter.value))

    //    val coeffROM = Mem(coeffHard.grouped(portWidth).toSeq.map(Vec(_)))
    //    currentCoeffs := coeffROM.readAsync(counter.value)
  }
  else currentCoeffs := Vec(coeffHard)

  val ret = Vec(dataIn.fragment.zip(currentCoeffs).map { case (data, coeff) => complexMult(data, coeff) })

  override val dataOut = master Flow Fragment(ret)
  dataOut.fragment := ret
  autoValid()
  autoLast()
}
