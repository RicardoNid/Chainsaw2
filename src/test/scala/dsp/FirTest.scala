package org.datenlord
package dsp

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class FirTest extends AnyFlatSpec {

  val coeffs = Seq.fill(16)(Random.nextDouble())
  val typeIn = HardType(SFix(0 exp, - 13 exp))
  val data = Seq.fill(128)(Random.nextDouble())
  val config = FirConfig(coeffs, typeIn, structure = Systolic)

  "fir" should "work" in TransformTest.test(config.implH, data)

}
