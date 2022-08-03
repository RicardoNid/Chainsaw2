package org.datenlord
package parsers

import spinal.core._

import scala.io.Source
import scala.tools.nsc.io.File

object SpiralParser {

  case class ShiftAddOp(opType: String, op0: Int, op1: Int, ret: Int, multiple: BigInt)

  case class Output(index: Int, value: BigInt)

  val outputPattern = """int t(\d+) = (\d+) \* t0""".r // match output information
  val exprPattern = """t(\d+) = (shl|shr|sub|add)\(t(\d+), t?(\d+)\); {3}/\* (\d+)\*/""".r // match shift-add information

  def run(constants: Seq[BigInt], widthIn: Int) = {
    val spiralDir = "/home/ltr/IdeaProjects/Chainsaw2/spiral"
    val constantsGt1 = constants.filter(_ > BigInt(1))
    val filename = s"spiral_w${widthIn}_${constants.mkString("_")}"
    val filepath = s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/generated/$filename"
    val command = s"acm ${constantsGt1.mkString(" ")} -gc"

    val file = File(filepath)
    if (!file.exists) file.writeAll(doCmdAndGetLine(command, spiralDir))
    val src = Source.fromFile(filepath)
    src.getLines().mkString("\n")
  }

  def parse(spiralText: String) = {

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

  def build(constants: Seq[BigInt], widthIn: Int) = {
    val (ops, outputInfos) = parse(run(constants, widthIn))
    val op = (dataIn: UInt) => {
      var cost = 0

      val vertices = dataIn.d(1) +: ops.map(op => UInt(op.multiple.bitLength + widthIn bits))

      ops.foreach { op =>
        val (uint0, uint1) = (vertices(op.op0), vertices(op.op1))
        val ret = op.opType match {
          case "shl" => (uint0 << op.op1).resized
          case "shr" => (uint0 >> op.op1).resized
          case "add" => cost += uint0.getBitsWidth max uint1.getBitsWidth
            (uint0 +^ uint1).resized
          case "sub" => cost += uint0.getBitsWidth
            (uint0 - uint1).resized
        }
        vertices(op.ret) := ret
      }

      logger.info(s"cost of pipelined adder graph: $cost")

      constants.map { constant =>
        constant.toInt match {
          case 0 => U(0).resized
          case 1 => dataIn.d(2)
          case _ =>
            val indexOfInfo = outputInfos.indexWhere(_.value == constant)
            val indexInVertices = outputInfos(indexOfInfo).index
            vertices(indexInVertices).d(1)
        }
      }
    }
    (op, 2) // TODO: auto pipeline for spiral, 2 should be changed to the true latency
  }
}