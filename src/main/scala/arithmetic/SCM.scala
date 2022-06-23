package org.datenlord
package arithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.util.Random

case class SCMConfig(constant: BigInt, width: Int, plain:Boolean) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = Seq(dataIn.head.asInstanceOf[BigInt] * constant)

  override val size = (1, 1)

  override def latency = 1

  override def implH = SCM(this)
}

case class SCM(config: SCMConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(width bits), 1))
  val widthOut = width + constant.bitLength
  println(widthOut)
  override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), 1))

  if (plain) {
    val product = dataIn.payload.head * U(constant)
    product.addAttribute("use_dsp", "no")
    dataOut.payload.head := product.d(1)
    // LUT = 171, FF = 44
  } else {
    val spiral = SpiralSCM(width, widthOut)
    spiral.X := dataIn.payload.head
    dataOut.payload.head := spiral.Y.d(1)
  }

  autoValid()
  autoLast()
}

object SCM {
  def main(args: Array[String]): Unit = {
    val length = BigInt(9239041).bitLength
    val config0 = SCMConfig(BigInt(9239041), length, plain = true)
    val config1 = SCMConfig(BigInt(9239041), length, plain = false)
    val testCases = (0 until 100).map(_ => Random.nextBigInt(length))
    TransformTest.test(SCM(config0), testCases, name = "testSCM")
    TransformTest.test(SCM(config1), testCases, name = "testSCM")
    VivadoSynth(SCM(config0))
    VivadoSynth(SCM(config1))
  }
}

case class SpiralSCM(widthIn: Int, widthOut: Int) extends BlackBox {
  val X = in UInt (widthIn bits)
  val Y = out UInt (widthOut bits)
  setDefinitionName("multiplier_block")
  addRTLPath("/home/ltr/IdeaProjects/Chainsaw2/multiplier_block.v")
}
