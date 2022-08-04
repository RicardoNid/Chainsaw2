package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.sys.process._
import scala.util.Random

// TODO: implement more operators in Flopoco
case class ScmConfig(constant: BigInt, widthIn: Int, plain: Boolean) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = Seq(dataIn.head.asInstanceOf[BigInt] * constant)

  override val size = (1, 1)

  override def latency = 1

  override def implH = Scm(this)
}

// TODO: merge this to Mcm, as Scm is a special case of mcm
case class Scm(config: ScmConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
  val widthOut = widthIn + constant.bitLength
  override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), 1))

  if (plain) {
    val product = dataIn.payload.head.d(1) * U(constant)
    product.addAttribute("use_dsp", "no")
    dataOut.payload.head := product.d(1)
  } else {
    val flopoco = FlopocoSCM(widthIn, constant)
    flopoco.X := dataIn.payload.head.d(1)
    dataOut.payload.head := flopoco.Y.d(1)
  }

  autoValid()
  autoLast()
}

object Scm {
  def main(args: Array[String]): Unit = {
    val constant = BigInt(62317)
    val length = constant.bitLength
    val config0 = ScmConfig(constant, length, plain = true)
    val config1 = ScmConfig(constant, length, plain = false)
    val testCases = (0 until 100).map(_ => Random.nextBigInt(length))
    //    TransformTest.test(SCM(config0), testCases, name = "testSCM")
    //    TransformTest.test(SCM(config1), testCases, name = "testSCM")
    VivadoSynth(Scm(config0))
    VivadoSynth(Scm(config1))
  }
}

case class FlopocoSCM(widthIn: Int, constant: BigInt) extends BlackBox {
  val X = in UInt (widthIn bits)
  X.setName("X0")
  val Y = out UInt (widthIn + constant.bitLength bits)
  Y.setName(s"R_c$constant")
  val defName = s"IntConstMultShiftAddOpt_${constant}_${widthIn}"
  setDefinitionName(defName)
  val fileName = s"$defName.vhd"
  doCmd(s"/home/ltr/flopoco/build/flopoco frequency=800 outputFile=$fileName IntConstMultShiftAddOpt wIn=$widthIn constant=$constant", "/home/ltr/IdeaProjects/Chainsaw2")
  addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/$fileName")
}

case class FlopocoMult(widthX: Int, widthY:Int) extends BlackBox {
  val X= in UInt (widthX bits)
  val Y = in UInt (widthY bits)
  val R = out UInt (widthX + widthY bits)
  val frequency = 800
  val defName = s"IntMultiplier_F${frequency}_uid2"
  setDefinitionName(defName)
  val fileName = s"$defName.vhd"
  doCmd(s"/home/ltr/flopoco/build/flopoco frequency=$frequency outputFile=$fileName IntMultiplier wX=$widthX wY=$widthY useKaratsuba=1", "/home/ltr/IdeaProjects/Chainsaw2")
  addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/$fileName")
}

// FIXME: currently, this module won't work, synth result cost nothing
case class BigMult(widthX: Int, widthY:Int) extends Component{
  val X= in UInt (widthX bits)
  val Y = in UInt (widthY bits)
  val R = out UInt (widthX + widthY bits)
  val core = FlopocoMult(widthX, widthY)
  core.X := X
  core.Y := Y
  R := core.R
}

object BigMult {
  def main(args: Array[String]): Unit = {
    VivadoSynth(BigMult(128,128))
  }
}
