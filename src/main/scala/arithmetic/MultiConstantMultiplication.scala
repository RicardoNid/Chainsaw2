package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

object McmType extends Enumeration {
  val NAIVE, SPIRAL, PAG = Value
  type McmType = Value
}

import arithmetic.McmType._


/** multiple constant multiplication for UInt implemented by different ways
 *
 * @param widthIn width of input operand
 * @see [[https://gitlab.com/kumm/pagsuite]] for PAGSuite implementation
 * @see [[https://spiral.ece.cmu.edu/mcm/gen.html]] for spiral implementation
 */
case class MultiConstantMultiplicationConfig(constants: Seq[BigInt], widthIn: Int, mcmType: McmType)
  extends TransformBase {

  override def impl(dataIn: Seq[Any]) = constants.map(_ * dataIn.head.asInstanceOf[BigInt])

  override val size = (1, constants.length)

  val (op, graphLatency) = mcmType match {
    case NAIVE =>
      val op = (dataIn: UInt) => constants.map { constant =>
        val product = dataIn.d(1) * constant
        product.addAttribute("use_dsp", "no")
        product
      }
      (op, 2)
    case SPIRAL => parsers.SpiralParser.build(constants, widthIn)
    case PAG => parsers.PagSuiteParser.build(constants, widthIn)
  }

  override def latency = graphLatency

  override def implH = MultiConstantMultiplication(this)
}

case class MultiConstantMultiplication(config: MultiConstantMultiplicationConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
  val dataOut = master Flow Fragment(Vec(UInt(), constants.length))

  val ret = Vec(op(dataIn.fragment.head))
  dataOut.fragment := ret

  autoValid()
  autoLast()
}