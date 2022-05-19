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

  val N = coeffs.length
  require(N % spaceFold == 0)
  val portWidth = N / spaceFold

  override val size = (N, N)

  override def spaceFolds = factors(N)

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

  import ComplexMult.complexMult

  import config._

  val controlType = HardType(Bits(N / 2 bits))

  override val dataIn = slave Flow Fragment(Vec(dataType, portWidth))
  val coeffHard = GetCoeff(coeffType, coeffs)
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

  val ret = dataIn.fragment.zip(currentCoeffs).map { case (data, coeff) =>
    data match {
      case cf: ComplexFix => complexMult(cf.asInstanceOf[ComplexFix], coeff.asInstanceOf[ComplexFix])
      case sf: SFix => (sf.asInstanceOf[SFix] * coeff.asInstanceOf[SFix]).truncated(dataType.asInstanceOf[HardType[SFix]])
      case si: SInt => (si.asInstanceOf[SInt] * coeff.asInstanceOf[SInt]).resize(dataType.getBitsWidth)
      case ui: UInt => (ui.asInstanceOf[UInt] * coeff.asInstanceOf[UInt]).resize(dataType.getBitsWidth)
    }
  }.asInstanceOf[Seq[THard]]

  override val dataOut = master Flow Fragment(cloneOf(Vec(ret)))
  dataOut.fragment := Vec(ret)
  autoValid()
  autoLast()
}
