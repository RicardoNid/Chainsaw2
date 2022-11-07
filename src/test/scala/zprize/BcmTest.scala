package org.datenlord
package zprize

import org.datenlord.ChainsawMetric._

import scala.util.Random
import org.scalatest.flatspec.AnyFlatSpec

class BcmTest extends AnyFlatSpec {
  val testCase = Seq.fill(100)(BigInt(320, Random))

  behavior of "the Full mode of Big constant multiplier test"

  it should "work correctly in FullMultiplier mode using Csd" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, FullMultiplier, useCsd = true)
    ChainsawTest.test(generator, testCase, testName = "FullBcmUseCsd")
  }

  it should "work correctly in FullMultiplier mode using Binary" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, FullMultiplier)
    ChainsawTest.test(generator, testCase, testName = "FullBcmUseBinary")
  }

  behavior of "the Msb mode of Big constant multiplier test"

  it should "work correctly in MsbMultiplier mode using Csd" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, MsbMultiplier, widthTake = 128, useCsd = true)
    ChainsawTest.test(generator, testCase, metric = ChainsawMetric(noBound, generator.metric), testName = "MsbBcmUseCsd")
  }

  it should "work correctly in MsbMultiplier mode using Binary" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, MsbMultiplier, widthTake = 128)
    ChainsawTest.test(generator, testCase, metric = ChainsawMetric(noBound, generator.metric), testName = "MsbBcmUseBinary")
  }

  behavior of "the Lsb mode of Big constant multiplier test"

  it should "work correctly in LsbMultiplier mode using Csd" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, LsbMultiplier, widthTake = 10, useCsd = true)
    ChainsawTest.test(generator, testCase, testName = "LsbBcmUseCsd")
  }

  it should "work correctly in LsbMultiplier mode using Binary" in {
    verbose = 1
    val generator = Bcm(BigInt("ff0ff0f", 16), 320, LsbMultiplier, widthTake = 128)
    ChainsawTest.test(generator, testCase, testName = "LsbBcmUseBinary")
  }

}
