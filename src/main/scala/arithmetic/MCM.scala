package org.datenlord
package arithmetic

import org.datenlord.parsers.PagNodeType.Value
import spinal.core.{Vec, out, _}
import spinal.lib._

import scala.sys.process._

object McmType extends Enumeration {
  val NAIVE, SPIRAL, PAG = Value
  type McmType = Value
}

import McmType._

/** multiple constant multiplication for UInt implemented by different ways
 *
 * @param widthIn width of input operand
 * @see [[https://gitlab.com/kumm/pagsuite]] for PAGSuite implementation
 * @see [[https://spiral.ece.cmu.edu/mcm/gen.html]] for spiral implementation
 */
case class Mcm(constants: Seq[BigInt], widthIn: Int, mcmType: McmType = SPIRAL)
  extends TransformBase {
  override def impl(dataIn: Seq[Any]) = constants.map(_ * dataIn.head.asInstanceOf[BigInt])

  override val size = (1, constants.length)

  override def latency = 2

  override def implH = ConstantMultByLut(this)

  def naiveImplH = new Module {
    val dataIn = in UInt (widthIn bits)
    val dataOut = out Vec(UInt(), constants.length)
    dataOut.zip(constants).foreach { case (out, coeff) =>
      val product = dataIn.d(1) * coeff
      product.addAttribute("use_dsp", "no")
      out := product.d(1)
    }
  }

  def asNode: Seq[UInt] => Seq[UInt] = (dataIn: Seq[UInt]) => {
    val core = implH
    core.dataIn.fragment := Vec(dataIn)
    core.skipControl()
    core.dataOut.fragment
  }

  // generating shift-add graph by spiral
  val spiralPath = "/home/ltr/IdeaProjects/Chainsaw2/spiral"
  val constantsGt1 = constants.filter(_ > BigInt(1))
  val spiralText = Process(s"acm ${constantsGt1.mkString(" ")} -gc", new java.io.File(spiralPath)).!!
  // regular expressions for information extraction
  val outputPattern = """int t(\d+) = (\d+) \* t0""".r // match output information
  val exprPattern = """t(\d+) = (shl|shr|sub|add)\(t(\d+), t?(\d+)\); {3}/\* (\d+)\*/""".r // match shift-add information

  case class ShiftAddOp(opType: String, op0: Int, op1: Int, ret: Int, multiple: BigInt)

  case class Output(index: Int, value: BigInt)

  def parse(spiralText: String) = {
    logger.info(s"spiral output:\n$spiralText")
    val lines = spiralText.split("\n")

    val ops = lines.map(exprPattern.findFirstMatchIn(_)).filter(_.nonEmpty) // get expressions
      .map(_.get).map { groups => // construct ShiftAddOp instances
      val opType = groups.group(2)
      val op0 = groups.group(3).toInt
      val op1 = groups.group(4).toInt
      val ret = groups.group(1).toInt
      val multiple = BigInt(groups.group(5))
      assert(multiple > BigInt(0))
      ShiftAddOp(opType, op0, op1, ret, multiple)
    }.sortBy(_.ret) // sort ShiftAddOp by the index of its output

    val outputInfos = lines.map(outputPattern.findFirstMatchIn(_)).filter(_.nonEmpty) // get outputs
      .map(_.get)
      .map { output =>
        val index = output.group(1).toInt
        val constant = BigInt(output.group(2))
        Output(index, constant)
      }

    (ops, outputInfos)
  }

  val (ops, outputInfos) = parse(spiralText)
  logger.info(s"${ops.length} ops extracted")
  logger.info(s"targets: ${ops.map(_.ret).sorted.mkString(" ")}")

}

case class ConstantMultByLut(config: Mcm) extends TransformModule[UInt, UInt] {

  import config._

  val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 1))
  val dataOut = master Flow Fragment(Vec(UInt(), constants.length))

  val vertices = dataIn.fragment.head.d(1) +: ops.map(op => UInt(op.multiple.bitLength + widthIn bits))

  ops.foreach { op =>
    val ret = op.opType match {
      case "shl" => println(s"t${op.ret} = t${op.op0} << ${op.op1}")
        (vertices(op.op0) << op.op1).resized
      case "shr" => println(s"t${op.ret} = t${op.op0} >> ${op.op1}")
        (vertices(op.op0) >> op.op1).resized
      case "add" => println(s"t${op.ret} = t${op.op0} + t${op.op1}")
        (vertices(op.op0) +^ vertices(op.op1)).resized
      case "sub" => println(s"t${op.ret} = t${op.op0} - t${op.op1}")
        (vertices(op.op0) - vertices(op.op1)).resized
    }
    vertices(op.ret) := ret
  }

  val op = parsers.SpiralParser.build(constants, widthIn)
  val ret = Vec(op(dataIn.fragment.head))

  //  // for constants > 1
  //  outputInfos.foreach { info =>
  //    dataOut.fragment(constants.indexOf(info.value)) := vertices(info.index).d(1)
  //  }
  //  // for constants 0 and 1
  //  dataOut.fragment.zip(constants).filter(_._2 == BigInt(1)).foreach(_._1 := dataIn.fragment.head.d(2))
  //  dataOut.fragment.zip(constants).filter(_._2 == BigInt(0)).foreach(_._1 := U(0).resized)
  //
  //

  dataOut.fragment := ret

  autoValid()
  autoLast()
}