package org.datenlord
package zprize

import org.datenlord
import org.datenlord.arithmetic.Compressor3to1Hard
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

sealed trait CpaMode

object M2M extends CpaMode

object M2S extends CpaMode

object S2M extends CpaMode

object S2S extends CpaMode

case class Cpa(adderType: AdderType, sub: Int, widths: Seq[Int], cpaMode: CpaMode) extends ChainsawGenerator {
  override val name = s"cpa_${widths.mkString("_")}_${cpaMode.getClass.getSimpleName.init}"

  val widthInc = adderType match {
    case BinaryAdder => 1
    // when you want the sign of subtraction, set 1 bit more on width, then MSB = 1 means negative
    case BinarySubtractor => 0
    case TernaryAdder => sub match {
      case 0 => 2
      // TODO: reconsider case 1 & 2
      case 1 => 0
      case 2 => 0
    }
  }

  val widthsWithInc = widths.init :+ (widths.last + widthInc)

  val operandCount = adderType match {
    case datenlord.TernaryAdder => 3
    case _ => 2
  }

  override var inputTypes = {
    val temp = cpaMode match {
      case M2M => widths.map(UIntInfo(_))
      case M2S => widths.map(UIntInfo(_))
      case S2M => Seq(widths.sum).map(UIntInfo(_))
      case S2S => Seq(widths.sum).map(UIntInfo(_))
    }
    Seq.fill(operandCount)(temp).flatten
  }

  override var outputTypes = cpaMode match {
    case M2M => widthsWithInc.map(UIntInfo(_))
    case M2S => Seq(widthsWithInc.sum).map(UIntInfo(_))
    case S2M => widthsWithInc.map(UIntInfo(_))
    case S2S => Seq(widthsWithInc.sum).map(UIntInfo(_))
  }

  override val impl = (dataIn: Seq[Any]) => {

    def concat(bigInts: Seq[BigInt], widths: Seq[Int]) = {
      val str = bigInts.zip(widths).map { case (int, i) => int.toString(2).padToLeft(i, '0') }.reverse.reduce(_ + _)
      BigInt(str, 2)
    }

    val data = dataIn.asInstanceOf[Seq[BigInt]]
      .grouped(inputWidths.length / operandCount).toSeq
      .map(concat(_, widths))

    val ret = adderType match {
      case BinaryAdder => data.sum
      case BinarySubtractor =>
        if (data(0) > data(1)) data(0) - data(1)
        else data(0) - data(1) + (BigInt(1) << widths.sum) // wrap around
      case TernaryAdder => sub match {
        case 0 => data.sum
        // TODO: wrap around behavior of ternary adders
        case 1 => data(0) + data(1) - data(2)
        case 2 => data(0) - data(1) - data(2)
      }
    }

    val retString = ret.toString(2).padToLeft(widths.sum + widthInc, '0').reverse // bits, low to high
    val slices = widthsWithInc.scan(0)(_ + _) // low to high
    val words = slices.prevAndNext { case (low, high) => retString.slice(low, high).reverse }.map(BigInt(_, 2))

    if (cpaMode == M2S | cpaMode == S2S) Seq(ret)
    else words
  }

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl

  override val inputTimes = {
    val temp = cpaMode match {
      case M2M => widths.indices
      case M2S => widths.indices
      case S2M => Seq(0)
      case S2S => Seq(0)
    }
    Seq.fill(operandCount)(temp).flatten
  }

  override val outputTimes = cpaMode match {
    case M2M => widths.indices
    case M2S => Seq(0)
    case S2M => widths.indices
    case S2S => Seq(0)
  }

  override var latency = cpaMode match {
    case M2M => 1
    case M2S => widths.length
    case S2M => 1
    case S2S => widths.length
  }



  override def implH = new ChainsawModule(this) {

    val coreCount = widths.length // number of subAdders
    val inputTimesExtended = inputTimes.padTo(coreCount, 0)
    val outputTimesExtended = outputTimes.padTo(coreCount, 0)
    val inputCompensations = widths.indices.zip(inputTimesExtended).map { case (target, actual) => target - actual }
    val outputCompensations = outputTimesExtended.zip(widths.indices).map { case (target, actual) => target + latency - actual }
    logger.info(s"inputDelays = ${inputCompensations.mkString(" ")}")
    logger.info(s"outputDelays = ${outputCompensations.mkString(" ")}")

    val uintIns = dataIn.map(_.asUInt)
    val dataWords: Seq[Seq[UInt]] = // a mesh of dataWords where each column contains all words of an operand
      {
        if (cpaMode == S2M | cpaMode == S2S) {
          val slices = widths.scan(0)(_ + _).prevAndNext { case (prev, next) => (next - 1) downto prev }
          uintIns.map(operand => slices.map(operand(_)))
        } else uintIns.grouped(coreCount).toSeq
      }.transpose

    // prepare output words
    val sumWords = widths.map(w => UInt(w bits)) //

    val carriesStart = adderType match {
      case BinaryAdder => Seq(False)
      case BinarySubtractor => Seq(True)
      case TernaryAdder => sub match {
        case 0 => Seq(False, False)
        case 1 => Seq(False, True)
      }
    }

    val finalCarryOut = Seq.iterate((carriesStart, 0), coreCount + 1) { case (carries, i) => // low to high
      adderType match {
        case BinaryAdder =>
          val cin = carries.head
          val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
          // TODO: use primitive for binary
          val ret = x +^ y + cin.asUInt
          sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
          (Seq(ret.msb.d(1)), i + 1)
        case BinarySubtractor =>
          val cin = carries.head
          val Seq(x, y) = dataWords(i).map(_.d(inputCompensations(i)))
          val ret = x +^ ~y + cin.asUInt
          sumWords(i) := ret.takeLow(x.getBitsWidth).asUInt.d(outputCompensations(i))
          (Seq(ret.msb.d(1)), i + 1)
        case TernaryAdder =>
          val Seq(cin0, cin1) = carries // get input
          val Seq(x, y, z) = dataWords(i).map(_.d(inputCompensations(i)))
          val core = Compressor3to1Hard(x.getBitsWidth, sub) // connection
          core.cIn0 := cin0
          core.cIn1 := cin1
          core.x := x
          core.y := y
          core.z := z
          sumWords(i) := core.sumsOut.d(outputCompensations(i))
          (Seq(core.cOut0.d(1), core.cOut1.d(1)), i + 1) // pass output
      }
    }.last._1

    val outputWords = (sumWords.init :+ (finalCarryOut.head.asUInt @@ sumWords.last)).map(_.asBits)

    // TODO: implementation for ternary
    if (cpaMode == M2S | cpaMode == S2S) dataOut.head := outputWords.reverse.reduce(_ ## _)
    else dataOut.zip(outputWords).foreach { case (port, bits) => port := bits }

    Seq.tabulate(operandCount, inputWidths.length / operandCount)((i, j) => dataIn(i * inputWidths.length / operandCount + j).setName(s"operand_${i}_$j"))
  }
}
