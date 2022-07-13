package org.datenlord
package device

import spinal.core._
import spinal.lib._

import scala.sys.process._
import scala.util.Random


case class ConstantMultByLutConfig(constants: Seq[BigInt], widthIn: Int) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = constants.map(_ * dataIn.head.asInstanceOf[BigInt])

  override val size = (1, constants.length)

  override def latency = 2

  override def implH = ConstantMultByLut(this)

  // generating shift-add graph by spiral
  val spiralPath = "/home/ltr/IdeaProjects/Chainsaw2/spiral"
  val spiralText = Process(s"acm ${constants.mkString(" ")} -gc", new java.io.File(spiralPath)).!!
  // regular expressions for information extraction
  val outputPattern = "int t(\\d+) = (\\d+) \\* t0".r // match output information
  val exprPattern = "t(\\d+) = (shl|shr|sub|add)\\(t(\\d+), t?(\\d+)\\);   /\\* (\\d+)\\*/".r // match shift-add information

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

case class ConstantMultByLut(config: ConstantMultByLutConfig) extends TransformModule[UInt, UInt] {

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
        (vertices(op.op0) +^ vertices(op.op1)).d(1).resized
      case "sub" => println(s"t${op.ret} = t${op.op0} - t${op.op1}")
        (vertices(op.op0) - vertices(op.op1)).d(1).resized
    }
    vertices(op.ret) := ret
  }

  // TODO: treat 0 & 1 specially
  outputInfos.foreach { info =>
    dataOut.fragment(constants.indexOf(info.value)) := vertices(info.index).d(1)
    println(s"out_${constants.indexOf(info.value)} = t${info.index}")
  }

  autoValid()
  autoLast()
}

object ConstantMultByLut {
  def main(args: Array[String]): Unit = {
    def testOnce() = {
      val constants = (0 until 3).map(_ => Random.nextBigInt(31))
      val config = ConstantMultByLutConfig(constants, 32)
      val data = (0 until 100 * constants.length).map(_ => Random.nextBigInt(32))
      TransformTest.test(config.implH, data)
    }

    import algos.ZPrizeMSM.{NPrime, baseModulus}

    def testZPrize(constant: BigInt) = {
      val splitPoints = (0 until (baseModulus.bitLength - 1 / 31)).map(_ * 31).reverse
      val constants = constant.split(splitPoints).filter(_ > 1)
      val config = ConstantMultByLutConfig(constants, 127 - 32)
      val data = (0 until 100 * constants.length).map(_ => Random.nextBigInt(32) % baseModulus)
      TransformTest.test(config.implH, data)
    }

    def synthZPrize(constant: BigInt) = {
      val splitPoints = (0 until (baseModulus.bitLength - 1 / 31)).map(_ * 31).reverse
      val constants = constant.split(splitPoints).filter(_ > 1)
      val config = ConstantMultByLutConfig(constants, 127 - 32)
      VivadoSynth(config.implH)
    }

    //    (0 until 100).foreach(_ => testOnce())
    //    testZPrize(baseModulus)
    //    testZPrize(NPrime)

    synthZPrize(baseModulus)
    synthZPrize(NPrime)
  }
}