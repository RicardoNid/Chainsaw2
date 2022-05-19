package org.datenlord
package arithmetic

import flowConverters._

import breeze.math._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class ComplexDiagonalMatrixConfig(coeffs: Seq[Complex], override val spaceFold: Int,
                                       dataType: HardType[SFix], coeffType: HardType[SFix])
  extends TransformBase {

  val N = coeffs.length
  require(N % spaceFold == 0)
  val portWidth = N / spaceFold

  override val size = (N, N)

  override def latency = ComplexMult.latency

  override def impl(dataIn: Seq[Any]) =
    dataIn.asInstanceOf[Seq[Complex]].zip(coeffs).map { case (data, coeff) => data * coeff }

  override def implH = ComplexDiagonalMatrix(this)

  override def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int) = ComplexDiagonalMatrixConfig(coeffs, spaceFold, dataType, coeffType)
}

case class ComplexDiagonalMatrix(config: ComplexDiagonalMatrixConfig) extends TransformModule[ComplexFix, ComplexFix] {

  import config._
  import ComplexMult.complexMult

  val controlType = HardType(Bits(N / 2 bits))

  override val dataIn = slave Flow Fragment(Vec(ComplexFix(dataType), portWidth))
  val coeffHard = coeffs.map(CF(_, coeffType))
  val currentCoeffs = Vec(HardType(ComplexFix(coeffType)), portWidth)
  currentCoeffs.setName("current")

  if (portWidth * coeffType.getBitsWidth * 2 > 4096) println(s"too big signal")

  if (spaceFold > 1) {

    val counter = CounterFreeRun(spaceFold)
    when(dataIn.last)(counter.clear())

    // TODO: implement "BigMem"
    //    val coeff0 = coeffHard.grouped(portWidth).toSeq.map(_.take(portWidth / 2)).map(Vec(_))
    //    val coeff1 = coeffHard.grouped(portWidth).toSeq.map(_.takeRight((portWidth + 1) / 2)).map(Vec(_))
    //    val rom0 = Mem(coeff0)
    //    val rom1 = Mem(coeff1)
    //    currentCoeffs := Vec(rom0.readAsync(counter.value) ++ rom1.readAsync(counter.value))

    val coeff = coeffHard.grouped(portWidth).toSeq.map(Vec(_))
    val rom = Mem(coeff)
    currentCoeffs := Vec(rom.readAsync(counter.value))
  }
  else currentCoeffs := Vec(coeffHard)

  val ret = Vec(dataIn.fragment.zip(currentCoeffs).map { case (data, coeff) => complexMult(data, coeff) })

  override val dataOut = master Flow Fragment(ret)
  dataOut.fragment := ret
  autoValid()
  autoLast()
}
