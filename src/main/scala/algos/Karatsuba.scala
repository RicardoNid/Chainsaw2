package org.datenlord
package algos

import arithmetic.{ArithInfo, BitHeap}

import spinal.core._

import scala.language.implicitConversions

// TODO: more comment, less code
/** karatsuba decomposition algorithm framework, estimate the cost a decomposition method while assuring its correctness
 *
 * @param mode special situation where we use "generalized karatsuba" method
 */
case class Karatsuba(width: Int, mode: MultiplierType, baseWidth: Int, constant: BigInt = null, byDsp: Boolean = true) {

  // counters for cost
  var baseMultCount = 0
  var preAdditionCost = 0
  var postAdditionCost = 0

  // declare methods which will update cost
  implicit class bigIntUtil(bi: BigInt) {
    def **(that: BigInt) = {
      //      require(bi.bitLength <= 17 && that.bitLength <= 26)
      baseMultCount += 1
      bi * that
    }

    // pre addition/subtraction
    def +<(that: BigInt) = {
      preAdditionCost += bi.bitLength max that.bitLength
      bi + that
    }

    def -<(that: BigInt) = {
      preAdditionCost += bi.bitLength max that.bitLength
      bi - that
    }

    // post addition/subtraction
    def +>(that: BigInt) = {
      postAdditionCost += bi.bitLength max that.bitLength
      bi + that
    }

    def ->(that: BigInt) = {
      postAdditionCost += bi.bitLength max that.bitLength
      bi - that
    }
  }

  def getSplit(width: Int) = Seq.iterate(baseWidth, 10)(2 * _).filter(_ < width).last

  def multRec(op0: BigInt, op1: BigInt, width: Int, mode: MultiplierType): BigInt = {
    val ret =
      if (op0 == BigInt(0) || op1 == BigInt(0)) BigInt(0) // skip imbalanced part
      else if (width <= baseWidth) op0 ** op1
      else {
        val lowWidth = getSplit(width)
        val doubleWidth = 2 * lowWidth
        val highWidth = width - lowWidth
        require(lowWidth >= highWidth)

        val (a, b) = op0.split(lowWidth)
        val (c, d) = op1.split(lowWidth)

        mode match {
          case FullMultiplier =>
            // pre
            val (abMsb, abMain) = (a +< b).split(lowWidth)
            val (cdMsb, cdMain) = (c +< d).split(lowWidth)
            // mult
            val ac = multRec(a, c, highWidth, FullMultiplier)
            val bd = multRec(b, d, lowWidth, FullMultiplier)
            val all = multRec(abMain, cdMain, lowWidth, FullMultiplier) + ((abMsb * cdMain + cdMsb * abMain) << lowWidth) + ((abMsb * cdMsb) << doubleWidth)
            // post
            val adPlusBc = all - ac - bd
            (ac << doubleWidth) +> (adPlusBc << lowWidth) +> bd
          case LsbMultiplier =>
            // mult
            val ad = multRec(a, d, lowWidth, LsbMultiplier) // this multiplication is imbalanced
            val cb = multRec(c, b, lowWidth, LsbMultiplier) // this multiplication is imbalanced
            val bd = multRec(b, d, lowWidth, FullMultiplier)
            // post
            ((ad +> cb) << lowWidth) +> bd
          // TODO: estimate this
          case MsbMultiplier =>
            // mult
            val ad = multRec(a, d, lowWidth, MsbMultiplier) // this multiplication is imbalanced
            val cb = multRec(c, b, lowWidth, MsbMultiplier) // this multiplication is imbalanced
            val ac = multRec(b, d, lowWidth, MsbMultiplier)
            (ac << lowWidth) +> (ad +> cb)
          case SquareMultiplier =>
            val ac = multRec(a, c, highWidth, SquareMultiplier)
            val adOrBc = multRec(a, d, lowWidth, FullMultiplier) // this multiplication is imbalanced
            val bd = multRec(b, d, lowWidth, SquareMultiplier)
            (ac << doubleWidth) +> (adOrBc << (lowWidth + 1)) + bd
        }
      }

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

  def multImprovedRec(op0: BigInt, op1: BigInt, width: Int, mode: MultiplierType): Ariths = {
    val ret =
      if (op0 == BigInt(0) || op1 == BigInt(0)) Ariths(Seq[Arith]()) // skip imbalanced part
      else if (width <= baseWidth) Ariths(Seq(Arith(op0 ** op1, width * 2, 0)))
      else {
        val lowWidth = getSplit(width)
        val doubleWidth = 2 * lowWidth
        val highWidth = width - lowWidth
        require(lowWidth >= highWidth)

        val (a, b) = op0.split(lowWidth)
        val (c, d) = op1.split(lowWidth)

        mode match {
          case FullMultiplier =>
            // before mults
            val (abMsb, abMain) = (a +< b).split(lowWidth)
            val (cdMsb, cdMain) = (c +< d).split(lowWidth)
            val cdOption = Arith(abMsb * cdMain, lowWidth, 0)
            val abOption = Arith(cdMsb * abMain, lowWidth, 0)
            val msbOption = Arith(abMsb * cdMsb, 1, 0)

            // mults
            val ac = multImprovedRec(a, c, highWidth, FullMultiplier)
            val bd = multImprovedRec(b, d, lowWidth, FullMultiplier)
            val allLow = multImprovedRec(abMain, cdMain, lowWidth, FullMultiplier)

            // after mults
            val all = allLow + ((cdOption + abOption) << lowWidth) + (msbOption << doubleWidth)
            //            val all = multImprovedRec(a + b, c + d, lowWidth + 1, Full)
            val adPlusBc = all - ac - bd
            (ac << doubleWidth) + (adPlusBc << lowWidth) + bd
          case LsbMultiplier =>
            val ad = multImprovedRec(a, d, lowWidth, LsbMultiplier) // this multiplication is imbalanced
            val cb = multImprovedRec(c, b, lowWidth, LsbMultiplier) // this multiplication is imbalanced
            val bd = multImprovedRec(b, d, lowWidth, FullMultiplier)
            ((ad + cb) << lowWidth) + bd
          case SquareMultiplier =>
            val ac = multImprovedRec(a, c, highWidth, SquareMultiplier)
            val adOrBc = multImprovedRec(a, d, lowWidth, FullMultiplier) // this multiplication is imbalanced
            val bd = multImprovedRec(b, d, lowWidth, SquareMultiplier)
            (ac << doubleWidth) + (adOrBc << (lowWidth + 1)) + bd
        }
      }
    ret
  }

  def mult(op0: BigInt, op1: BigInt, verbose: Boolean = false, byImproved: Boolean = false) = {

    val retWhole = {
      if (byImproved) {
        val ariths = multImprovedRec(op0, op1, width, mode)
        if (verbose) {
          logger.info(s"pre-addition cost: $preAdditionCost")
          logger.info(s"post-addition bits number: ${ariths.data.map(_.width).sum}")
          val infos = ariths.data.map(info => ArithInfo(info.width, info.baseShift))
//          val (widthOut, latency, postAdditionCost) = BitHeap.getInfoOfCompressor(infos, 126)
//          logger.info(s"post-addition cost: $postAdditionCost")
//          logger.info(s"overall cost: ${preAdditionCost + postAdditionCost} for addition, $baseMultCount for mults")
        }
        ariths.sum
      }
      else multRec(op0, op1, width, mode)
    }
    val lowWidth = getSplit(width)

    val ret = mode match {
      case LsbMultiplier => retWhole % (BigInt(1) << lowWidth)
      case _ => retWhole
    }
    val golden = mode match {
      case LsbMultiplier => (op0 * op1) % (BigInt(1) << lowWidth)
      case _ => op0 * op1
    }
    logger.info(s"${baseMultCount * 3} base mult consumed")
    baseMultCount = 0
    preAdditionCost = 0
    assert(ret == golden, s"\nret    = $ret\ngolden = $golden\nmode=$mode\nop0=$op0\nop1=$op1")
    ret
  }

  def multImproved(op0: BigInt, op1: BigInt, verbose: Boolean = false) = mult(op0, op1, verbose, byImproved = true)

  def karatsuba96(a: BigInt, b: BigInt) = {
    def karatsubaBase(aHigh: BigInt, aLow: BigInt, bHigh: BigInt, bLow: BigInt) = {
      // pre addition
      require(aHigh.bitLength <= 16 && aLow.bitLength <= 16 && bHigh.bitLength <= 24 && bLow.bitLength <= 24)
      val (aMerge, bMerge) = (aHigh -< aLow, bHigh -< bLow)
      // mults
      val (high, low) = (aHigh * bHigh, aLow * bLow) // already got
      val temp = aMerge ** bMerge
      // post addition
      val mid = high +> low - temp
      assert(mid == aHigh * bLow + bHigh * aLow)
      mid
    }

    // k-part karatsuba
    val aWords = a.toWords(16).padTo(6, BigInt(0)) // 6 elements
    val bWords = b.toWords(24).padTo(4, BigInt(0)) // 4 elements

    val tiles = Seq.tabulate(6, 4) { (i, j) =>
      val (aWord, bWord) = (aWords(i), bWords(j))
      val (aPos, bPos) = (i * 16, j * 24)
      (aWord, bWord, aPos, bPos)
    }.flatten

    val partials = tiles
      .groupBy(tuple => tuple._3 + tuple._4).toSeq
      .map { case (pos, group) =>
        //        logger.info(s"pos: $pos")
        group.length match {
          case 1 =>
            val prod = group.head._1 ** group.head._2 // black points
            (prod, pos)
          case 2 =>
            val Seq(left, right) = group.sortBy(_._4)
            val (aHigh, aLow, bHigh, bLow) = (left._1, right._1, right._2, left._2)
            //            logger.info(s"pos: $pos, $aHigh, $aLow, $bHigh, $bLow")
            val prod = karatsubaBase(aHigh, aLow, bHigh, bLow)
            (prod, pos)
        }
      }

    partials.map { case (int, pos) => int << pos }.sum
  }

  def karatsuba377(a: BigInt, b: BigInt) = {

    def rec(a: BigInt, b: BigInt, width: Int): BigInt = {
      require(a.bitLength <= width && b.bitLength <= b)
      if (width == 96) karatsuba96(a, b)
      else {
        val (aHigh, aLow) = a.split(width / 2)
        val (bHigh, bLow) = b.split(width / 2)
        val widthNext = width / 2 + 1

        val (aMerge, bMerge) = (aHigh + aLow, bHigh + bLow)
        // mults
        val (high, low) = (rec(aHigh, bHigh, widthNext), rec(aLow, bLow, widthNext)) // already got
        val temp = rec(aMerge, bMerge, widthNext)
        // post addition
        val mid = temp - high - low // cost: 3 * width
        val long = (high << width) + low // cost: 2 * wi
        (mid << (width / 2)) + long
      }
    }

    val ret = rec(a, b, 378)
    logger.info(s"mult cost = $baseMultCount")
    logger.info(s"pre-add cost = $preAdditionCost")
    baseMultCount = 0
    preAdditionCost = 0
    assert(ret == a * b)
    ret
  }
}