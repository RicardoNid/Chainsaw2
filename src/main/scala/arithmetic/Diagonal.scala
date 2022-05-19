package org.datenlord
package arithmetic

import breeze.math._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class DiagonalConfig[TSoft, THard <: Data]
(coeffs: Seq[TSoft],
 dataType: HardType[THard], coeffType: HardType[THard],
 override val spaceFold: Int)
  extends TransformBase {

  val n = coeffs.length
  require(n % spaceFold == 0)
  val portWidth = n / spaceFold

  override val size = (n, n)

  override def spaceFolds = factors(n)

  override def latency = dataType() match {
    case _: ComplexFix => ComplexMult.latency
    case _ => 1
  }

  override def impl(dataIn: Seq[Any]) =
    dataIn.zip(coeffs).map { case (data, coeff) =>
      coeff match {
        case int: Int => data.asInstanceOf[Int] * int
        case double: Double => data.asInstanceOf[Double] * double
        case complex: Complex => data.asInstanceOf[Complex] * complex
      }
    }

  override def implH = Diagonal(this)

  override def getConfigWithFoldsChanged(spaceFold: Int, timeFold: Int) =
    DiagonalConfig(coeffs, dataType, coeffType, spaceFold)
}

case class Diagonal[TSoft, THard <: Data]
(config: DiagonalConfig[TSoft, THard])
  extends TransformModule[THard, THard] {

  import config._

  val controlType = HardType(Bits(n / 2 bits))

  override val dataIn = slave Flow Fragment(Vec(dataType, portWidth))
  override val dataOut = master Flow Fragment(Vec(dataType, portWidth))

  // build coeffs and ROM
  val coeffHard = Util.getCoeff(coeffType, coeffs)
  val currentCoeffs = Vec(coeffType, portWidth)
  currentCoeffs.setName("current")

  if (portWidth * coeffType.getBitsWidth > 4096) logger.warn(s"too big signal")

  if (spaceFold > 1) {
    val counter = autoInputCounter()
    // TODO: implement "BigMem"
    val coeffGroups = coeffHard.grouped(portWidth).toSeq.map(Vec(_))
    val rom = Mem(coeffGroups)
    currentCoeffs := Vec(rom.readAsync(counter.value))
  }
  else currentCoeffs := Vec(coeffHard)

  dataOut.fragment := Util.mult(dataIn.fragment, currentCoeffs, dataType)

  autoValid()
  autoLast()
}
