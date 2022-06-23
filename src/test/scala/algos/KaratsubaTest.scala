package org.datenlord
package algos

import arithmetic.{KaratsubaConfig}
import device.KaratsubaForXilinx

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 10
  val width = 377
  val baseWidth = 34

  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val xs = data.take(testCount)
  val ys = data.takeRight(testCount)

  val configMult = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency)
  val configLow = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency, "low")
  val configSquare = KaratsubaConfig(width, 34, baseMult, baseMultLatency = KaratsubaForXilinx.latency, "square")
  val configConstant = KaratsubaConfig(width, 34, lutMult, baseMultLatency = 1, "constant", constant = algos.MSM.scalarModulus)

  "Karatsuba Algo" should "work for full multiplication" in {
    xs.zip(ys).foreach { case (x, y) =>
      Karatsuba.karatsuba(width, baseWidth, x, y)
      Karatsuba.printAndClear()
    }
  }

  it should "work for low-bit multiplication" in {
    xs.zip(ys).foreach { case (x, y) =>
      Karatsuba.karatsubaLowOnly(width, baseWidth, x, y)
      Karatsuba.printAndClear()
    }
  }

  it should "work for squaring" in {
    xs.zip(ys).foreach { case (x, y) =>
      Karatsuba.karatsubaSquare(width, baseWidth, x)
      Karatsuba.printAndClear()
    }
  }

  // configuration
  def baseMult(x: UInt, y: UInt) = {
    val core = KaratsubaForXilinx()
    core.dataIn.payload := Vec(x.resized, y.resized)
    core.dataIn.valid := True
    core.dataOut.payload
  }

  def lutMult(x: UInt, y: UInt) = {
    val product = x * y
    product.addAttribute("use_dsp", "no")
    product.d(1)
  }

  "Karatsuba Multiplier" should "work for full multiplication" in TransformTest.test(configMult.implH, data)
  "Karatsuba Multiplier" should "work for low-bit multiplication" in TransformTest.test(configLow.implH, data)
  "Karatsuba Multiplier" should "work for squaring" in TransformTest.test(configSquare.implH, xs)
  "Karatsuba Multiplier" should "work for constant multiplication" in TransformTest.test(configConstant.implH, xs)

  "Karatsuba Multiplier" should "synth for full multiplication" in VivadoSynth(configMult.implH, name = "karatsubaMult")
  "Karatsuba Multiplier" should "synth for low-bit multiplication" in VivadoSynth(configLow.implH, name = "karatsubaLow")
  "Karatsuba Multiplier" should "synth for squaring" in VivadoSynth(configSquare.implH, name = "karatsubaSquare")
  "Karatsuba Multiplier" should "synth for constant multiplication" in VivadoSynth(configConstant.implH, name = "karatsubaConstant")
}
