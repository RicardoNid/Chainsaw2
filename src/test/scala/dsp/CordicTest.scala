package org.datenlord
package dsp

import dsp.AlgebraicMode._
import dsp.RotationMode._
import xilinx.VivadoUtilRequirement

import breeze.numerics.constants._
import breeze.numerics.{cos, sin}
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.IntToBuilder

import scala.util.Random

class CordicTest extends AnyFlatSpec {

  Random.setSeed(42)
  val testCount = 1000
  val testIteration = 12
  val testFraction = 13

  def getOne: Double = Random.nextDouble() * 2 - 1 // [-1,1]

  def getGroup: Seq[Double] = {
    val phase0 = getOne * Pi
    val phase1 = getOne * Pi
    val length = getOne.abs
    Seq(cos(phase0) * length, sin(phase0) * length, phase1)
  }

  val validGroups = Seq.fill(testCount)(getGroup).flatten

  behavior of "Cordic"

  def cordicMetric(epsilonX: Double, epsilonY: Double, epsilonZ: Double) = (yours: Seq[Double], golden: Seq[Double]) => {
    val Seq(yourX, yourY, yourZ) = yours
    val Seq(gX, gY, gZ) = golden
    (yourX - gX).abs <= epsilonX && (yourY - gY).abs <= epsilonY && (yourZ - gZ).abs <= epsilonZ
  }

  val metric = cordicMetric(1e-3, 1e-3, Pi / 180)

  def test(alg: AlgebraicMode, rot: RotationMode) = {
    val config = CordicConfig(CIRCULAR, ROTATION, iteration = testIteration, fraction = testFraction)
    TransformTest.test(config.implH, metric = metric, data = validGroups, name = s"test_${alg}_$rot")
  }

  it should s"work for CIRCULAR   + ROTATION mode" in test(CIRCULAR, ROTATION)
  it should s"work for HYPERBOLIC + ROTATION mode" in test(HYPERBOLIC, ROTATION)
  it should s"work for LINEAR     + ROTATION mode" in test(LINEAR, ROTATION)

  it should s"work for CIRCULAR   + VECTOR mode" in test(CIRCULAR, VECTORING)

  it should s"work for HYPERBOLIC + VECTOR mode" in test(HYPERBOLIC, VECTORING)
  it should s"work for LINEAR     + VECTOR mode" in test(LINEAR, VECTORING)

  val utilRequirement = VivadoUtilRequirement(lut = 1000, ff = 1000, dsp = 10, bram36 = 0, carry8 = 125)

  it should "synth for CIRCULAR + VECTORING" in
    VivadoSynth(CordicConfig(CIRCULAR, VECTORING, iteration = 12, fraction = 13).implH, "CORDIC_CIRCULAR_VECTOR")
      .require(utilRequirement, 500 MHz)

}
