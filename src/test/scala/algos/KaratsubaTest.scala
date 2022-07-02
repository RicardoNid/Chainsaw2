package org.datenlord
package algos

import arithmetic.KaratsubaConfig
import device.MultiplicationByDsp

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.util.Random

class KaratsubaTest extends AnyFlatSpec {

  val testCount = 1000
  val width = 377

  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(width))
  val xs = data.take(testCount)
  val ys = data.takeRight(testCount)

  val multByLut = (x: UInt, y: UInt) => {
    val product = x * y
    product.addAttribute("use_dsp", "no")
    product.d(1)
  }

  import arithmetic.MultplierMode._

  val configMult = KaratsubaConfig(width, Full)
  val configLow = KaratsubaConfig(width, Low)
  val configSquare = KaratsubaConfig(width, Square)
  val configFullConstant = KaratsubaConfig(width, Full, constant = algos.ZPrizeMSM.scalarModulus, byDsp = false)
  val configLowConstant = KaratsubaConfig(width, Low, constant = algos.ZPrizeMSM.scalarModulus, byDsp = false)

  val baseWidth = 32

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

  "Karatsuba Algo with forward-backward structure" should "work for full multiplication" in {
    xs.zip(ys).foreach { case (x, y) =>
      Karatsuba.anotherKaratsuba(width, baseWidth, x, y)
      Karatsuba.printAndClear()
    }
  }

  "Karatsuba Multiplier" should "work for full multiplication" in TransformTest.test(configMult.implH, data)
  it should "work for low-bit multiplication" in TransformTest.test(configLow.implH, data)
  it should "work for squaring" in TransformTest.test(configSquare.implH, xs)
  it should "work for constant multiplication" in TransformTest.test(configFullConstant.implH, xs)
  it should "work for constant low-bit multiplication" in TransformTest.test(configLowConstant.implH, xs)

//  it should "synth for full multiplication" in VivadoSynth(configMult.implH, name = "karatsubaMult")
//  it should "synth for low-bit multiplication" in VivadoSynth(configLow.implH, name = "karatsubaLow")
//  it should "synth for squaring" in VivadoSynth(configSquare.implH, name = "karatsubaSquare")
//  it should "synth for constant multiplication" in VivadoSynth(configFullConstant.implH, name = "karatsubaConstant")
//  it should "synth for constant low-bit multiplication" in VivadoSynth(configLowConstant.implH, name = "karatsubaLowConstant")
}
