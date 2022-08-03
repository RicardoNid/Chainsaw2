package org.datenlord
package arithmetic

import arithmetic.MultplierMode._
import dfg.ArithInfo

import spinal.core._

import scala.language.implicitConversions
import scala.util.Random

case class Karatsuba(width: Int, mode: MultiplierMode, baseWidth: Int, constant: BigInt = null, byDsp: Boolean = true) {

  var baseMultCount = 0

  def baseMult(a: BigInt, b: BigInt) = {
    require(a.bitLength <= baseWidth && b.bitLength <= baseWidth)
    baseMultCount += 1
    a * b
  }

  def getSplit(width: Int) = Seq.iterate(baseWidth, 10)(2 * _).filter(_ < width).last

  def multRec(op0: BigInt, op1: BigInt, width: Int, mode: MultiplierMode): BigInt = {
    val ret =
      if (op0 == BigInt(0) || op1 == BigInt(0)) BigInt(0) // skip imbalanced part
      else if (width <= baseWidth) baseMult(op0, op1)
      else {
        val lowWidth = getSplit(width)
        val doubleWidth = 2 * lowWidth
        val highWidth = width - lowWidth
        require(lowWidth >= highWidth)

        val (a, b) = op0.split(lowWidth)
        val (c, d) = op1.split(lowWidth)

        mode match {
          case Full =>
            val (abMsb, abMain) = (a + b).split(lowWidth)
            val (cdMsb, cdMain) = (c + d).split(lowWidth)
            val ac = multRec(a, c, highWidth, Full)
            val bd = multRec(b, d, lowWidth, Full)
            val all = multRec(abMain, cdMain, lowWidth, Full) + ((abMsb * cdMain + cdMsb * abMain) << lowWidth) + ((abMsb * cdMsb) << doubleWidth)
            val adPlusBc = all - ac - bd
            (ac << doubleWidth) + (adPlusBc << lowWidth) + bd
          case Low =>
            val ad = multRec(a, d, lowWidth, Low) // this multiplication is imbalanced
            val cb = multRec(c, b, lowWidth, Low) // this multiplication is imbalanced
            val bd = multRec(b, d, lowWidth, Full)
            ((ad + cb) << lowWidth) + bd
          case Square =>
            val ac = multRec(a, c, highWidth, Square)
            val adOrBc = multRec(a, d, lowWidth, Full) // this multiplication is imbalanced
            val bd = multRec(b, d, lowWidth, Square)
            (ac << doubleWidth) + (adOrBc << (lowWidth + 1)) + bd
        }
      }

    ret
  }

  def mult(op0: BigInt, op1: BigInt) = {
    val lowWidth = getSplit(width)
    val ret = mode match {
      case Low => multRec(op0, op1, width, mode) % (BigInt(1) << lowWidth)
      case _ => multRec(op0, op1, width, mode)
    }
    val golden = mode match {
      case Low => (op0 * op1) % (BigInt(1) << lowWidth)
      case _ => op0 * op1
    }
    println(s"this multiplication consumed $baseMultCount base mult")
    baseMultCount = 0
    assert(ret == golden, s"\nret    = $ret\ngolden = $golden")
    ret
  }

  case class Arith(value: BigInt, width: Int, baseShift: Int, sign: Boolean = true) {
    def <<(shift: Int) = Arith(value, width, baseShift + shift, sign)

    def unary_- = Arith(value, width, baseShift, !sign)

    def +(that: Arith) = Ariths(Seq(this, that))

    def -(that: Arith) = Ariths(Seq(this, -that))

    def eval = (value << baseShift) * (if (sign) 1 else -1)
  }

  implicit def asExp(arith: Arith): Ariths = Ariths(Seq(arith))

  case class Ariths(data: Seq[Arith]) {
    def <<(shift: Int) = Ariths(data.map(_ << shift))

    def sum = data.map(_.eval).sum

    def +(that: Ariths) = Ariths(data ++ that.data)

    def unary_- = Ariths(data.map(-_))

    def -(that: Ariths) = this.+(-that)

    def positivePart = data.filter(_.sign == true)

    def negativePart = data.filter(_.sign == false)
  }

  def multImprovedRec(op0: BigInt, op1: BigInt, width: Int, mode: MultiplierMode): Ariths = {
    val ret =
      if (op0 == BigInt(0) || op1 == BigInt(0)) Ariths(Seq[Arith]()) // skip imbalanced part
      else if (width <= baseWidth) Ariths(Seq(Arith(baseMult(op0, op1), width * 2, 0)))
      else {
        val lowWidth = getSplit(width)
        val doubleWidth = 2 * lowWidth
        val highWidth = width - lowWidth
        require(lowWidth >= highWidth)

        val (a, b) = op0.split(lowWidth)
        val (c, d) = op1.split(lowWidth)

        mode match {
          case Full =>
            // before mults
            val (abMsb, abMain) = (a + b).split(lowWidth)
            val (cdMsb, cdMain) = (c + d).split(lowWidth)
            val cdOption = Arith(abMsb * cdMain, lowWidth, 0)
            val abOption = Arith(cdMsb * abMain, lowWidth, 0)
            val msbOption = Arith(abMsb * cdMsb, 1, 0)

            // mults
            val ac = multImprovedRec(a, c, highWidth, Full)
            val bd = multImprovedRec(b, d, lowWidth, Full)
            val allLow = multImprovedRec(abMain, cdMain, lowWidth, Full)

            // after mults
            val all = allLow + ((cdOption + abOption) << lowWidth) + (msbOption << doubleWidth)
            //            val all = multImprovedRec(a + b, c + d, lowWidth + 1, Full)
            val adPlusBc = all - ac - bd
            (ac << doubleWidth) + (adPlusBc << lowWidth) + bd
          case Low =>
            val ad = multImprovedRec(a, d, lowWidth, Low) // this multiplication is imbalanced
            val cb = multImprovedRec(c, b, lowWidth, Low) // this multiplication is imbalanced
            val bd = multImprovedRec(b, d, lowWidth, Full)
            ((ad + cb) << lowWidth) + bd
          case Square =>
            val ac = multImprovedRec(a, c, highWidth, Square)
            val adOrBc = multImprovedRec(a, d, lowWidth, Full) // this multiplication is imbalanced
            val bd = multImprovedRec(b, d, lowWidth, Square)
            (ac << doubleWidth) + (adOrBc << (lowWidth + 1)) + bd
        }
      }
    ret
  }

  def multImproved(op0: BigInt, op1: BigInt) = {
    val ariths = multImprovedRec(op0, op1, width, mode)

    val lowWidth = getSplit(width)
    val ret = mode match {
      case Low => ariths.sum % (BigInt(1) << lowWidth)
      case _ => ariths.sum
    }
    val golden = mode match {
      case Low => (op0 * op1) % (BigInt(1) << lowWidth)
      case _ => op0 * op1
    }
    println(s"this multiplication consumed $baseMultCount base mult")
    println(s"number of operands in post network: ${ariths.data.count(_.value != BigInt(0))}")
    println(s"number of bits in operands: ${ariths.data.map(_.width).sum}")
    baseMultCount = 0
    assert(ret == golden, s"\nret    = $ret\ngolden = $golden")

    val compressor = UIntCompressorConfig(ariths.data.map(arith => ArithInfo(arith.width, arith.baseShift)))
    VivadoSynth(compressor.implH)

    ret
  }

//  case class HardArith(value: UInt, width: Int, baseShift: Int, sign: Boolean = true) {
//    def <<(shift: Int) = HardArith(value, width, baseShift + shift, sign)
//
//    def unary_- = HardArith(value, width, baseShift, !sign)
//
//    def +(that: Arith) = HardAriths(Seq(this, that))
//
//    def -(that: Arith) = HardAriths(Seq(this, -that))
//  }
//
//  implicit def asExp(arith: Arith): Ariths = Ariths(Seq(arith))
//
//  case class HardAriths(data: Seq[HardArith]) {
//    def <<(shift: Int) = HardAriths(data.map(_ << shift))
//
//    def +(that: HardAriths) = HardAriths(data ++ that.data)
//
//    def unary_- = HardAriths(data.map(-_))
//
//    def -(that: HardAriths) = this.+(-that)
//
//    def positivePart = data.filter(_.sign == true)
//
//    def negativePart = data.filter(_.sign == false)
//  }

//  def multHardRec(op0: UInt, op1: UInt, width: Int, mode: MultiplierMode) = {
//
//    def baseMult(mode:MultiplierMode) = MultiplicationByDspConfig(mode).asOperator
//
//    val ret =
//      if (width <= baseWidth) baseMult(mode)(op0, op1).resize(width * 2)
//      else {
//        val lowWidth = getSplit(width)
//        val doubleWidth = 2 * lowWidth
//        val highWidth = width - lowWidth
//        require(lowWidth >= highWidth)
//
//        val (a, b) = op0.splitAt(lowWidth)
//        val (c, d) = op1.splitAt(lowWidth)
//
//        mode match {
//          case Full =>
//            // before mults
//            val bigAddConfig = PipelinedBigAdderConfig(lowWidth)
//            val bigAdd = bigAddConfig.asOperator
//            val (abMsb, abMain) = bigAdd(a.asUInt ,b.asUInt).splitAt(lowWidth)
//            val (cdMsb, cdMain) = bigAdd(c.asUInt ,d.asUInt).splitAt(lowWidth)
//            val cdOption = Mux(abMsb.asBool, cdMain.asUInt, U(0, lowWidth bits))
//            val abOption = Mux(cdMsb.asBool, cdMain.asUInt, U(0, lowWidth bits))
//              Arith(abMsb * cdMain, lowWidth, 0)
//            val abOption = Arith(cdMsb * abMain, lowWidth, 0)
//            val msbOption = Arith(abMsb * cdMsb, 1, 0)
//
//            // mults
//            val ac = multImprovedRec(a, c, highWidth, Full)
//            val bd = multImprovedRec(b, d, lowWidth, Full)
//            val allLow = multImprovedRec(abMain, cdMain, lowWidth, Full)
//
//            // after mults
//            val all = allLow + ((cdOption + abOption) << lowWidth) + (msbOption << doubleWidth)
//            //            val all = multImprovedRec(a + b, c + d, lowWidth + 1, Full)
//            val adPlusBc = all - ac - bd
//            (ac << doubleWidth) + (adPlusBc << lowWidth) + bd
//          case Low =>
//            val ad = multImprovedRec(a, d, lowWidth, Low) // this multiplication is imbalanced
//            val cb = multImprovedRec(c, b, lowWidth, Low) // this multiplication is imbalanced
//            val bd = multImprovedRec(b, d, lowWidth, Full)
//            ((ad + cb) << lowWidth) + bd
//          case Square =>
//            val ac = multImprovedRec(a, c, highWidth, Square)
//            val adOrBc = multImprovedRec(a, d, lowWidth, Full) // this multiplication is imbalanced
//            val bd = multImprovedRec(b, d, lowWidth, Square)
//            (ac << doubleWidth) + (adOrBc << (lowWidth + 1)) + bd
//        }
//      }
//    ret
//
//  }


}

object Karatsuba {
  def main(args: Array[String]): Unit = {
    val data = (0 until 1000).map(_ => Random.nextBigInt(377))
    //    Karatsuba(377, Full, 32).mult(data(0), data(1))
    //    Karatsuba(377, Low, 32).mult(data(0), data(1))
    //    Karatsuba(377, Square, 32).mult(data(0), data(0))
    Karatsuba(1024, Full, 32).multImproved(data(0), data(1))
//    Karatsuba(377, Low, 32).multImproved(data(0), data(1))
//    Karatsuba(377, Square, 32).multImproved(data(0), data(0))
  }
}
