package org.datenlord
package device

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Ternary adder/subtractor which can be recognized by Xilinx synthesis tools
 *
 * @param width width of input operands, output width = width + 2
 * @param sub   sub = 0 => a+b+c 1 => a+b-c 2 => a-b-c
 */
case class TernaryAdderConfig(width: Int, sub: Int = 0, pipelined: Int = 1)
  extends TransformBase {
  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val Seq(a, b, c) = bigInts
    val ret = sub match {
      case 0 => a + b + c
      case 1 => a + b - c
      case 2 => a - b - c
    }
    Seq(ret)
  }

  override val size = (3, 1)

  override def latency = pipelined

  override def implH = TernaryAdder(this)
}

case class TernaryAdderSignedConfig(width: Int, sub: Int = 0)
  extends TransformBase {
  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    val Seq(a, b, c) = bigInts
    val ret = sub match {
      case 0 => a + b + c
      case 1 => a + b - c
      case 2 => a - b - c
    }
    Seq(ret)
  }

  override val size = (3, 1)

  override def latency = 1

  override def implH = TernaryAdderSigned(this)
}

case class TernaryAdder(config: TernaryAdderConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  // TODO: fix the problem introduced by no latency
  val dataIn = slave Flow Fragment(Vec(UInt(width bits), 3))
  val inc = 2 - sub
  val dataOut = master Flow Fragment(Vec(UInt(width + inc bits), 1))

  val impl = TernaryAdderXilinx(width, sub)
  impl.dataIn := dataIn.fragment
  dataOut.fragment.head := impl.dataOut.resize(width + inc).d(pipelined)

  autoValid()
  autoLast()
}

case class TernaryAdderSigned(config: TernaryAdderSignedConfig)
  extends TransformModule[SInt, SInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(SInt(width bits), 3))
  val dataOut = master Flow Fragment(Vec(SInt(width + 2 bits), 1))

  val impl = TernaryAdderXilinxSigned(width, sub)
  impl.dataIn := dataIn.fragment
  dataOut.fragment.head := impl.dataOut.resize(width + 2).d(1)

  autoValid()
  autoLast()
}

case class TernaryAdderXilinx(width: Int, sub: Int) extends BlackBox {
  val dataIn = in Vec(UInt(width bits), 3)
  val dataOut = out UInt (width + 2 bits)
  val generic = new Generic {
    val width = TernaryAdderXilinx.this.width
    val mode = sub
  }

  addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxes/TernaryAdder.v")
}

case class TernaryAdderXilinxSigned(width: Int, sub: Int) extends BlackBox {
  val dataIn = in Vec(SInt(width bits), 3)
  val dataOut = out SInt (width + 2 bits)
  val generic = new Generic {
    val width = TernaryAdderXilinxSigned.this.width
    val mode = sub
  }

  addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/blackboxes/TernaryAdderSigned.v")
}
