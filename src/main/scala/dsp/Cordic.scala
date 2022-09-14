package org.datenlord
package dsp

import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object AlgebraicMode extends Enumeration {
  type AlgebraicMode = Value
  val CIRCULAR, HYPERBOLIC, LINEAR = Value
}

object RotationMode extends Enumeration {
  type RotationMode = Value
  val ROTATION, VECTORING = Value
}

import dsp.AlgebraicMode._
import dsp.RotationMode._

// TODO: range extension
case class CordicConfig(algebraicMode: AlgebraicMode, rotationMode: RotationMode,
                        iteration: Int, fraction: Int)
  extends TransformBase {

  val amplitudeType = HardType(SFix(1 exp, -fraction exp)) // [-1,1]
  val phaseType = HardType(SFix(2 exp, -fraction exp)) // [-pi, pi]

  /** --------
   * calculating coefficients
   * -------- */
  // TODO: better implementation
  def getHyperbolicSequence(iteration: Int) = {
    require(iteration < 54, "iteration times should be less than 54")
    val sequence = (1 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
    sequence.slice(0, iteration)
  }

  def getPhaseCoeff(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    algebraicMode match {
      case AlgebraicMode.CIRCULAR => atan(pow(2.0, -iter))
      case AlgebraicMode.HYPERBOLIC => atanh(pow(2.0, -getHyperbolicSequence(iter + 1).last))
      case AlgebraicMode.LINEAR => pow(2.0, -iter)
    }
  }

  def getScaleComplement(iter: Int)(implicit algebraicMode: AlgebraicMode) = {
    require(iter >= 1)
    algebraicMode match {
      case AlgebraicMode.CIRCULAR => (0 until iter).map(i => cos(getPhaseCoeff(i))).product
      case AlgebraicMode.HYPERBOLIC => 1.0 / (1 until iter)
        .map(i => getHyperbolicSequence(i).last)
        .map(i => sqrt(1 - pow(2.0, -2 * i))).product
      case AlgebraicMode.LINEAR => 1.0
    }
  }

  // TODO: analysis
  def getAmplitudeType(iter: Int) = {
    val minExp = -(fraction + iter)
    val lengthMax = sqrt(2) // (1,1)
    val scale = 1.0 / getScaleComplement(iter + 1)(algebraicMode) * lengthMax
    val maxExp = log(2.0, scale).ceil.toInt
    HardType(SFix(maxExp exp, minExp exp))
  }

  // reference model
  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[Double]]
    val Seq(x, y, z) = data
    algebraicMode match {
      case CIRCULAR => rotationMode match {
        case ROTATION => Seq(x * cos(z) - y * sin(z), y * cos(z) + x * sin(z), 0.0)
        case VECTORING => Seq(sqrt(x * x + y * y), 0.0, z + atan(y / x))
        // FIXME: atan is not the same as angle
      }
      case HYPERBOLIC => rotationMode match {
        case ROTATION => Seq(x * cosh(z) - y * sinh(z), y * cosh(z) + x * sinh(z), 0.0)
        case VECTORING => Seq(sqrt(x * x - y * y), 0.0, z + atanh(y / x))
      }
      case LINEAR => rotationMode match {
        case ROTATION => Seq(x, y + x * z, 0.0)
        case VECTORING => Seq(x, 0.0, z + y / x)
      }
    }
  }

  override val size = (3, 3)

  override def latency = iteration + 1 // 1 for each stage, and 1 for compensation

  override def implH = Cordic(this)
}

case class Cordic(config: CordicConfig)
  extends TransformModule[SFix, SFix] {

  import config._

  implicit val refMode: AlgebraicMode = algebraicMode

  val dataIn = slave Flow Fragment(Vec(amplitudeType(), amplitudeType(), phaseType()))
  val dataOut = master Flow Fragment(Vec(amplitudeType(), amplitudeType(), phaseType()))

  val shiftingCoeffs = // shift value at each stage
    if (algebraicMode == AlgebraicMode.HYPERBOLIC) getHyperbolicSequence(iteration)
    else (0 until iteration)

  val scaleComplement = SF(getScaleComplement(iteration), 1 exp, -fraction exp)
  logger.info(s"compensation = ${getScaleComplement(iteration)}")

  def getCounterclockwise(group: Seq[SFix]) = rotationMode match {
    case ROTATION => ~group(2).asBits.msb // Z > 0
    case VECTORING => group(1).asBits.msb // Y < 0
  }

  val Seq(x, y, z) = dataIn.fragment
  val determinant = (y.raw.msb ## x.raw.msb).asUInt

  // TODO: pipeline for this stage
  // TODO: for circular mode only?
  val Seq(xPrime, yPrime, zPrime) = Seq(x, y, z).map(_.clone())

  switch(determinant) { // range extension
    is(U(0)) { // first quadrant
      xPrime := x
      yPrime := y
      zPrime := z
    }
    is(U(1)) { // second quadrant
      xPrime := y
      yPrime := -x
      zPrime := z + SFConstant(Pi / 2, phaseType)
    }
    is(U(2)) { // fourth quadrant
      xPrime := x
      yPrime := y
      zPrime := z
    }
    is(U(3)) { // third quadrant
      xPrime := -y
      yPrime := x
      zPrime := z - SFConstant(Pi / 2, phaseType)
    }
  }

  val ret = Seq.iterate((Seq(xPrime, yPrime, zPrime) , 0), iteration + 1) {
    case (Seq(x, y, z), i) =>
      val nextStageType = getAmplitudeType(i)
      assert(1.0 / getScaleComplement(i + 1) < nextStageType().maxValue)
      val shift = shiftingCoeffs(i)
      val phaseStep = SFConstant(getPhaseCoeff(i), phaseType())
      val xShifted = x >> shift
      val yShifted = y >> shift
      val direction = getCounterclockwise(Seq(x, y, z))
      // next-stage logic
      val xNext = (algebraicMode match {
        case AlgebraicMode.CIRCULAR => Mux(direction, x - yShifted, x + yShifted)
        case AlgebraicMode.HYPERBOLIC => Mux(direction, x + yShifted, x - yShifted)
        case AlgebraicMode.LINEAR => x
      }).truncate(nextStageType).d(1)
      val yNext = Mux(direction, y + xShifted, y - xShifted).truncate(nextStageType).d(1)
      val zNext = Mux(direction, z - phaseStep, z + phaseStep).d(1)

      yNext.setName(s"y$i")

      logger.info(s"stage${i + 1}: x: ${xNext.maxExp}Q${-xNext.minExp}, y: ${yNext.maxExp}Q${-yNext.minExp}, z: ${zNext.maxExp}Q${-zNext.minExp}")
      (Vec(xNext, yNext, zNext), i + 1)
  }.last._1

  val Seq(xOut, yOut, zOut) = ret

  dataOut.fragment(0) := (xOut * scaleComplement).d(1).truncated
  dataOut.fragment(1) := (yOut * scaleComplement).d(1).truncated
  dataOut.fragment(2) := zOut.d(1).truncated

  autoValid()
  autoLast()
}

object Cordic {

  def phaseZero(fraction: Int) = SFConstant(0, 2 exp, -fraction exp)

  def getAbsAndPhase(real: SFix, imag: SFix, iteration: Int, fraction: Int) = {
    val op = CordicConfig(CIRCULAR, VECTORING, iteration, fraction).implH.asFunc
    val ret = op(Seq(real, imag, phaseZero(fraction)))
    (ret.head, ret.last) // (abs, phase)
  }

}