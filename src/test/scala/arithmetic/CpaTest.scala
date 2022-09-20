package org.datenlord
package arithmetic

import xilinx.VivadoUtilRequirement

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps
import scala.util.Random

class CpaTest extends AnyFlatSpec {

  val testCount = 1000

  behavior of "ternary CPA"

  def testTernary(width: Int, sub:Int): Unit = {
    val config = CpaConfig(width, TernaryAdder, sub)
    val dataForSub = Seq.fill(testCount){
      val a = BigInt(width, Random)
      val b = BigInt(width, Random)
      val c = (a + b) / 2
      Seq(a,b,c)
    }.flatten
    TransformTest.test(config.implH, dataForSub, name = "testTernary")
  }

  def synthTernary(width: Int): Unit = {
    val config = CpaConfig(width, TernaryAdder)
    VivadoSynth(config.implH, name = "synthTernary").require(config.utilRequirement, 800 MHz)
  }

  val ternaryWidths = Seq(31, 127, 511, 1023)

  it should "work for ternary addition x + y + z" in ternaryWidths.foreach(testTernary(_, 0))
  it should "work for ternary addition x + y - z" in ternaryWidths.foreach(testTernary(_, 1))
  it should "synth for ternary addition" in ternaryWidths.foreach(synthTernary)

  behavior of "binary CPA"

  def testBinary(width: Int, adderType: AdderType): Unit = {
    val config = CpaConfig(width, adderType)
    val dataForSub = Seq.fill(testCount){
      val a = BigInt(width, Random)
      val b = a / 2
      Seq(a,b)
    }.flatten
    TransformTest.test(config.implH, dataForSub, name = "testBinary")
  }

  def synthBinary(width: Int, adderType: AdderType): Unit = {
    val config = CpaConfig(width, adderType)
    VivadoSynth(config.implH, name = "synthBinary").require(config.utilRequirement, 800 MHz)
  }

  val binaryWidths = Seq(31, 127, 511, 1023)

  it should "work for binary addition x + y" in binaryWidths.foreach(testBinary(_, BinaryAdder))
  it should "work for binary addition x - y" in binaryWidths.foreach(testBinary(_, BinarySubtractor))
  ignore should "synth for binary addition" in binaryWidths.foreach(synthBinary(_, BinaryAdder))

}
