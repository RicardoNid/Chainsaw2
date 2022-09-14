package org.datenlord
package dsp

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class UnwrapTest extends AnyFlatSpec {

  val testCount = 1000
  val integralMax = 10
  val randomPhases = Seq.fill(testCount)(Random.nextDouble() * 2 * integralMax - integralMax)
  val typeStored = HardType(SFix(log2Up(integralMax) exp, -4 exp))
  val typeFull = HardType(SFix(log2Up(integralMax) exp, -10 exp))
  val config = UnwrapConfig(typeStored, typeFull)

  def metric(epsilon: Double) =
    (yours: Seq[Double], golden: Seq[Double]) => {
      val diff = (yours.head - golden.head).abs
      // unwrap is imperfect when the difference between two phases is very close to Pi, but this seldom happened, and has no effect on its functionality
      diff < epsilon || (2 - epsilon <= diff && diff <= 2 + epsilon)
    }

  behavior of "Unwrap"

  it should "work correctly" in TransformTest.test(config.implH, randomPhases, metric = metric(1e-1))

  it should "synth" in VivadoSynth(config.implH, "unwrap")
}
