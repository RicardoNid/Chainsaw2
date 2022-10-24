package org.datenlord
package dsp

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object DoFir {

  /** we drop the first taps - 1 output, as the registers hasn't been fully refreshed, and therefore, the output and input have the same size
   */
  def latency(taps: Int, structure: SopStructure = Transpose) = structure match {
    case Direct => taps + 1
    case Transpose => taps + 1
    case Systolic => taps * 2 + 1
  }

  def apply[T <: Data](data: T, coeffs: Seq[T], mult: (T, T) => T, add: (T, T) => T,
                       structure: SopStructure = Transpose) = {
    val taps = coeffs.length
    structure match {
      case Direct =>
        val delayLine = Seq.iterate(data.d(1), taps)(_.d(1))
        val products = delayLine.zip(coeffs).map { case (x, h) => mult(x, h) }
        products.reduce(add).d(1)
      case Transpose =>
        val products = coeffs.map(coeff => mult(data.d(1), coeff))
        products.reverse.reduce((prev, next) => add(prev.d(1), next)).d(1)
      case Systolic =>
        val delayLine = Seq.iterate(data.d(1), taps)(_.d(2))
        val products = delayLine.zip(coeffs).map { case (x, h) => mult(x, h) }.map(_.d(1))
        products.reduce((prev, next) => add(prev.d(1), next)).d(1)
    }
  }
}

case class FirConfig(coeffs: Seq[Double], typeIn: HardType[SFix], structure: SopStructure) extends TransformBase {

  val taps = coeffs.length
  val typeCoeff = HardType(SFix(0 exp, -17 exp))
  val typeOut = HardType(SFix(log2Up(taps) exp, typeIn().minExp exp))

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Double]]
    matlab.Dsp.fir(data.toArray, coeffs.toArray).drop(coeffs.length - 1) // drop leading results, size unchanged
  }

  override val implMode = Infinite

  override val size = (1, 1)

  override def latency = DoFir.latency(taps, structure)

  override def implH = Fir(this)
}

case class Fir(config: FirConfig) extends TransformModule[SFix, SFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(typeIn, inputPortWidth))
  override val dataOut = master Flow Fragment(Vec(typeOut, outputPortWidth))

  val coeffsHard = coeffs.map(SFConstant(_, typeCoeff()))

  def mult(data: SFix, coeff: SFix) = (data * coeff).truncate(typeOut)

  def add(a: SFix, b: SFix) = a + b

  def pipeline(value: SFix, nothing: Int) = value.d(1)

  dataOut.fragment.head := DoFir(dataIn.fragment.head, coeffsHard, mult, add, structure)

  autoValid()
  autoLast()
}
