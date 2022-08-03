package org.datenlord
package parsers

import spinal.core._

import scala.collection.mutable
import scala.io.Source
import scala.tools.nsc.io.File

object PagNodeType extends Enumeration {
  val OUTPUT, REG, BINARY, TERNARY = Value
  type PagNodeType = Value
}

import parsers.PagNodeType._

object PagSuiteParser {

  case class Operand(stage: Int, fundamental: Int, shift: Int = 0)

  case class PagNode(nodeType: PagNodeType, output: Operand, input: Seq[Operand]) {
    def outPosition = (output.stage, output.fundamental)
  }

  val outputPattern = """'O',\[(\d+)],(\d+),\[(\d+)],(\d+),(\d+)""".r
  val regPattern = """'R',\[(\d+)],(\d+),\[(\d+)],(\d+)""".r
  val binaryPattern = """'A',\[(\d+)],(\d+),\[(\d+)],(\d+),(-?\d+),\[(-?\d+)],(\d+),(-?\d+)""".r
  val ternaryPattern = """'A',\[(\d+)],(\d+),\[(\d+)],(\d+),(-?\d+),\[(-?\d+)],(\d+),(-?\d+),\[(-?\d+)],(\d+),(-?\d+)""".r

  def run(constants: Seq[BigInt], widthIn: Int) = {
    val rpagDir = "/home/ltr/IdeaProjects/Chainsaw2/pagsuite/build/bin"
    val filename = s"pag_w${widthIn}_${constants.mkString("_")}"
    val filepath = s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/generated/$filename"
    val file = File(filepath)
    //    if (!file.exists) doCmd(s"rpag --ternary_adders --cost_model=ll_fpga --verbose=0 --input_wordsize=$widthIn --file_output=$filepath ${constants.mkString(" ")}", rpagDir)
    if (!file.exists) doCmd(s"rpag --verbose=0 --input_wordsize=$widthIn --file_output=$filepath ${constants.mkString(" ")}", rpagDir)
    val src = Source.fromFile(filepath)
    src.getLines().toSeq(1).split("=")(1)
  }

  def parse(expr: String) = {
    val rawString = expr.slice(2, expr.length - 2)
    val nodeStrings = rawString.split("\\},\\{")
    nodeStrings.map {
      case outputPattern(fo, so, fi, si, shift) =>
        PagNode(OUTPUT, Operand(so.toInt, fo.toInt), Seq(Operand(si.toInt, fi.toInt, shift.toInt)))
      case regPattern(fo, so, fi, si) =>
        PagNode(REG, Operand(so.toInt, fo.toInt), Seq(Operand(si.toInt, fi.toInt)))
      case binaryPattern(fo, so, fi0, si0, shift0, fi1, si1, shift1) =>
        val op0 = Operand(si0.toInt, fi0.toInt, shift0.toInt)
        val op1 = Operand(si1.toInt, fi1.toInt, shift1.toInt)
        if (Seq(op0, op1).forall(_.shift > 0)) logger.warn("redundant addition exist")
        PagNode(BINARY, Operand(so.toInt, fo.toInt), Seq(op0, op1))
      case ternaryPattern(fo, so, fi0, si0, shift0, fi1, si1, shift1, fi2, si2, shift2) =>
        val op0 = Operand(si0.toInt, fi0.toInt, shift0.toInt)
        val op1 = Operand(si1.toInt, fi1.toInt, shift1.toInt)
        val op2 = Operand(si2.toInt, fi2.toInt, shift2.toInt)
        if (Seq(op0, op1, op2).forall(_.shift > 0)) logger.warn("redundant addition exist")
        PagNode(TERNARY, Operand(so.toInt, fo.toInt), Seq(op0, op1, op2))
    }
  }

  def build(constants: Seq[BigInt], widthIn: Int) = {
    val nodes = parse(run(constants, widthIn))
    val op = (dataIn: UInt) => {

      val signalMap = mutable.Map[(Int, Int), UInt]((0, 1) -> dataIn) // (stage, fundamental) -> signal
      val outputMap = mutable.Map[Int, UInt]() //  fundamental -> signal

      def getOperand(operand: Operand) =
        if (operand.shift >= 0) signalMap(operand.stage, operand.fundamental.abs) << operand.shift
        else signalMap(operand.stage, operand.fundamental.abs) >> -operand.shift

      var cost = 0

      nodes.foreach { node =>
        println(node)
        node.nodeType match {
          case OUTPUT => outputMap += node.output.fundamental -> getOperand(node.input.head)
          case REG =>
            val ret = getOperand(node.input.head).d(1)
            signalMap += node.outPosition -> ret
          //            cost += ret.getBitsWidth
          case BINARY =>
            val Seq(op0, op1) = node.input.map(getOperand)
            val positive = node.input.last.fundamental > 0
            val widthOut = log2Up(node.output.fundamental) + widthIn
            val ret = if (positive) (op0 +^ op1).d(1) else (op0 - op1).d(1).resize(widthOut)
            signalMap += node.outPosition -> ret
            cost += ret.getBitsWidth
          case TERNARY =>
            val Seq(op0, op1, op2) = node.input.map(getOperand)
            val sign1 = node.input(1).fundamental > 0
            val sign2 = node.input(2).fundamental > 0
            val reverse = !sign1 && sign2
            val (regularOp1, regularOp2) = if (!reverse) (op1, op2) else (op2, op1)
            val sub = Seq(sign1, sign2).count(_ == false)
            val width = Seq(op0, op1, op2).map(_.getBitsWidth).max
            val widthOut = log2Up(node.output.fundamental) + widthIn
            val config = device.TernaryAdderConfig(width, sub)
            val ret = config.asNode(Seq(op0, regularOp1, regularOp2)).head.resize(widthOut)
            signalMap += node.outPosition -> ret
            cost += ret.getBitsWidth
        }
      }

      logger.info(s"cost of pipelined adder graph: $cost")
      constants.map(_.toInt).map(outputMap(_))
    }
    val latency = nodes.map(_.output.stage).max
    (op, latency)
  }
}