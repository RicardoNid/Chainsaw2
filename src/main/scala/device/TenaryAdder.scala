package org.datenlord
package device

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

/** Ternary adder/subtractor which can be recognized by Xilinx synthesis tools
 *
 * @param width width of input operands, output width = width + 2
 * @param sub   sub = 0 => a+b+c 1 => a+b-c 2 => a-b-c
 */
case class TernaryAdderConfig(width: Int, sub: Int = 0) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = sub match {
      case 0 => bigInts.sum
      case 1 => bigInts(0) + bigInts(1) - bigInts(2)
      case 2 => bigInts(0) - bigInts(1) - bigInts(2)
    }
    Seq(ret)
  }

  override val size = (3, 1)

  override def latency = 1

  override def implH = TernaryAdder(this)

  def asNode: Seq[UInt] => Seq[UInt] = (dataIn: Seq[UInt]) => {
    val Seq(a, b, c) = dataIn
    val core = implH
    core.dataIn.fragment := Vec(a.resized, b.resized, c.resized)
    core.skipControl()
    core.dataOut.fragment
  }
}

case class TernaryAdder(config: TernaryAdderConfig) extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(width bits), 3))
  val dataOut = master Flow Fragment(Vec(UInt(width + 2 bits), 1))

  val impl = TernaryAdderXilinx(width, sub)
  impl.dataIn := dataIn.fragment
  dataOut.fragment.head := impl.dataOut.d(1)

  autoValid()
  autoLast()
}

case class TernaryAdderXilinx(width: Int, sub: Int) extends BlackBox {
  val dataIn = in Vec(UInt(width bits), 3)
  val dataOut = out UInt (width + 2 bits)
  val generic = new Generic {
    val width = TernaryAdderXilinx.this.width
  }

  sub match {
    case 0 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAdder.v")
    case 1 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAddSub.v")
    case 2 => addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxese/TernaryAddSubSub.v")
  }
}
