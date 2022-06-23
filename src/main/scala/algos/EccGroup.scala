package org.datenlord
package algos

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

abstract class EccPoint

case class EccPointAffine(x: IntZ, y: IntZ) {

  def toProjective = if (this == EccZeroAffine) EccPointProj(1, 1, 0) else EccPointProj(x, y, asBigInteger(1))

  def +(that: EccPointAffine)(implicit eccGroup: EccGroup) = eccGroup.padd(this, that)

  def dbl(implicit eccGroup: EccGroup) = eccGroup.pdbl(this)

  def *(that: IntZ)(implicit eccGroup: EccGroup) = eccGroup.pmult(that, this)

  def inverse(implicit eccGroup: EccGroup) = EccPointAffine(x, (-y) % eccGroup.modulus)
  
}

object EccZeroAffine extends EccPointAffine(null, null)

case class EccPointProj(x: IntZ, y: IntZ, z: IntZ) {
  def toAffine(implicit eccGroup: EccGroup) = {
    if (isZero) EccZeroAffine
    else {
      val xAffine = eccGroup.zp.divideExact(x, z * z)
      val yAffine = eccGroup.zp.divideExact(y, z * z * z)
      EccPointAffine(xAffine, yAffine)
    }
  }

  def isZero = z.intValue() == 0
}

case class EccGroup(modulus: IntZ, a: IntZ, b: IntZ) {

  val zp = Zp(modulus)
  val polyRing = UnivariateRing(zp, "x")
  val curveExpr = s"x ^ 3 + $a * x + $b"
  val curve = polyRing(curveExpr)

  def getSquareRoot(value: IntZ): Seq[IntZ] = {
    require(modulus < 1024)
    val p = modulus.toBigInt.toInt
    (0 until p).filter(i => zp.multiply(i, i) == value % modulus).map(asBigInteger)
  }

  def getPoints(x: IntZ): Seq[EccPointAffine] =
    getSquareRoot(curve.evaluate(x)).map(y => EccPointAffine(x, y))

  def getAllPoints: Seq[EccPointAffine] = {
    val p = modulus.toBigInt.toInt
    (0 until p).map(asBigInteger).flatMap(getPoints)
  }

  def isOnCurve(eccPoint: EccPointAffine) =
    if (eccPoint == EccZeroAffine) true
    else curve.evaluate(eccPoint.x) == eccPoint.y * eccPoint.y % modulus

  def apply(x: IntZ, y: IntZ): EccPointAffine = {
    val ret = EccPointAffine(x, y)
    assert(isOnCurve(ret), s"point ($x, $y) is not on curve $curveExpr")
    ret
  }

  def padd(p0: EccPointAffine, p1: EccPointAffine): EccPointAffine = {
    val (x0, x1, y0, y1) = (p0.x, p1.x, p0.y, p1.y)

    val ret = {
      if (p0 == EccZeroAffine) p1
      else if (p1 == EccZeroAffine) p0
      else if (p0 == p1.inverse(this)) EccZeroAffine
      else {
        val lambda = {
          if (p0 == p1) zp.divideExact(x0 * x0 * 3 + a, y0 * 2)
          else zp.divideExact(y1 - y0, x1 - x0)
        }

        val x2 = (lambda * lambda - x0 - x1) % modulus
        val y2 = (lambda * (x0 - x2) - y0) % modulus

        EccPointAffine(x2, y2)
      }
    }
    ret
  }

  def pdbl(p: EccPointAffine) = padd(p, p)

  def paddByProj(p0: EccPointProj, p1: EccPointProj) = {
    require(a.intValue() == 0)

    val (x0, x1, y0, y1, z0, z1) = (p0.x, p1.x, p0.y, p1.y, p0.z, p1.z)

    val ret = {
      if (p0.isZero) p1
      else if (p1.isZero) p0
      //      else if (p0 == p1.inverse(this)) EccZeroAffine
      else if (p0 != p1) {
        val z0Squared = zp.multiply(z0, z0)
        val z0Cubed = zp.multiply(z0, z0Squared)
        val z1Squared = zp.multiply(z1, z1)
        val z1Cubed = zp.multiply(z1, z1Squared)
        val u0 = zp.multiply(x0, z1Squared)
        val u1 = zp.multiply(x1, z0Squared)
        val s0 = zp.multiply(y0, z1Cubed)
        val s1 = zp.multiply(y1, z0Cubed)
        val h = zp.subtract(u1, u0)
        val r = zp.subtract(s1, s0)
        val hSquared = zp.multiply(h, h)
        val hCubed = zp.multiply(hSquared, h)
        val rSquared = zp.multiply(r, r)
        val uhCross = zp.multiply(u0, hSquared)
        val shCross = zp.multiply(s0, hCubed)
        val x2 = (rSquared - hCubed - uhCross * 2) % modulus
        val y2 = zp.multiply(r, uhCross - x2) - shCross
        val zCross = zp.multiply(z0, z1)
        val z2 = zp.multiply(zCross, h)
        EccPointProj(x2, y2, z2)
      }
      else {
        val xSquared = zp.multiply(x0, x0)
        val xSquaredSquared = zp.multiply(xSquared, xSquared)
        val ySquared = zp.multiply(y0, y0)
        val ySquaredSquared = zp.multiply(ySquared, ySquared)

        // version a
        val xyCross = zp.multiply(x0, ySquared)
        val x2 = (xSquaredSquared * 9 - xyCross * 8) % modulus
        val temp0 = (xyCross * 4 - x2) % modulus
        val temp1 = zp.multiply(xSquared, temp0)
        val y2 = (temp1 * 3 - ySquaredSquared * 8) % modulus
        val z2 = (zp.multiply(y0, z0) * 2) % modulus

        EccPointProj(x2, y2, z2)
      }
    }
    ret
  }

  def pdblByProj(p: EccPointProj) = paddByProj(p, p)

  def pmult(k: IntZ, p: EccPointAffine) = {
    println("affine")
    var temp = p
    println(temp, temp.toProjective)
    k.toBigInt.toString(2).tail.foreach { bit =>
      temp = pdbl(temp)
      if (bit == '1') temp = padd(temp, p)
      println(temp, temp.toProjective)
    }
    temp
  }

  def pmultByProj(k: IntZ, p: EccPointProj) = {
    println("proj")
    var temp = p
    println(temp)
    k.toBigInt.toString(2).tail.foreach { bit =>
      temp = pdblByProj(temp)
      if (bit == '1') temp = paddByProj(temp, p)
      println(temp)
    }
    temp
  }

}

object EccGroup {

  // test from "Guide to ECC"
  def testEcc() = {

    val p = 23
    val a = 0
    val b = 1

    implicit val ecc = EccGroup(p, a, b)
    val points = ecc.getAllPoints
    val (point0, point1, scalar) = (points(0), points(2), 15)

    val sum = ecc.padd(point0, point1)
    val sumByProj = ecc.paddByProj(point0.toProjective, point1.toProjective).toAffine
    assert(ecc.isOnCurve(sum))
    assert(sumByProj == sum)
    println(s"sum: $sumByProj = $sum")

    val dbl = ecc.pdbl(point0)
    val dblByProj = ecc.pdblByProj(point0.toProjective).toAffine
    assert(ecc.isOnCurve(dbl))
    assert(dblByProj == dbl)
    println(s"dbl: $dblByProj = $dbl")

    println(ecc.paddByProj(point0.toProjective, point0.inverse.toProjective))

    val product = ecc.pmult(scalar, point1)
    val productByProj = ecc.pmultByProj(scalar, point1.toProjective).toAffine
    assert(ecc.isOnCurve(product))
    assert(product == productByProj)
    println(s"product $product = $productByProj")
  }

  def main(args: Array[String]): Unit = {
    testEcc()
  }

}
