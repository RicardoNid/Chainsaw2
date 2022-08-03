package org.datenlord
package algos

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

case class EcPointAffine(x: IntZ, y: IntZ) {

  def toProjective = if (this == EcZeroAffine) EcPointProj(1, 1, 0) else EcPointProj(x, y, asBigInteger(1))

  def +(that: EcPointAffine)(implicit eccGroup: EcGroup) = eccGroup.padd(this, that)

  def dbl(implicit eccGroup: EcGroup) = eccGroup.pdbl(this)

  def *(that: IntZ)(implicit eccGroup: EcGroup) = eccGroup.pmult(that, this)

  def inverse(implicit eccGroup: EcGroup) = EcPointAffine(x, (-y) % eccGroup.modulus)

}

object EcZeroAffine extends EcPointAffine(null, null)

case class EcPointProj(x: IntZ, y: IntZ, z: IntZ) {

  def toAffine(implicit eccGroup: EcGroup) = {
    if (isZero) EcZeroAffine
    else {
      val xAffine = eccGroup.zp.divideExact(x, z * z)
      val yAffine = eccGroup.zp.divideExact(y, z * z * z)
      EcPointAffine(xAffine, yAffine)
    }
  }

  def +(that: EcPointProj)(implicit eccGroup: EcGroup) = eccGroup.paddByProj(this, that)

  def dbl(implicit eccGroup: EcGroup) = eccGroup.pdblByProj(this)

  def *(that: IntZ)(implicit eccGroup: EcGroup) = eccGroup.pmultByProj(that, this)

  def isZero = z.intValue() == 0
}

case class EcGroup(modulus: IntZ, a: IntZ, b: IntZ) {

  val zp = Zp(modulus)
  val polyRing = UnivariateRing(zp, "x")
  val curveExpr = s"x ^ 3 + $a * x + $b"
  val curve = polyRing(curveExpr)

  // only for small p
  def getSquareRoot(value: IntZ): Seq[IntZ] = {
    require(modulus < 1024)
    val p = modulus.toBigInt.toInt
    (0 until p).filter(i => zp.multiply(i, i) == value % modulus).map(asBigInteger)
  }

  def getPoints(x: IntZ): Seq[EcPointAffine] =
    getSquareRoot(curve.evaluate(x)).map(y => EcPointAffine(x, y))

  def getAllPoints: Seq[EcPointAffine] = {
    val p = modulus.toBigInt.toInt
    (0 until p).map(asBigInteger).flatMap(getPoints)
  }


  def isOnCurve(eccPoint: EcPointAffine) =
    if (eccPoint == EcZeroAffine) true
    else curve.evaluate(eccPoint.x) == eccPoint.y * eccPoint.y % modulus

  def apply(x: IntZ, y: IntZ): EcPointAffine = {
    val ret = EcPointAffine(x, y)
    assert(isOnCurve(ret), s"point ($x, $y) is not on curve $curveExpr")
    ret
  }

  def padd(p0: EcPointAffine, p1: EcPointAffine): EcPointAffine = {
    val (x0, x1, y0, y1) = (p0.x, p1.x, p0.y, p1.y)

    val ret = {
      if (p0 == EcZeroAffine) p1
      else if (p1 == EcZeroAffine) p0
      else if (p0 == p1.inverse(this)) EcZeroAffine
      else {
        val lambda = {
          if (p0 == p1) zp.divideExact(x0 * x0 * 3 + a, y0 * 2)
          else zp.divideExact(y1 - y0, x1 - x0)
        }

        val x2 = (lambda * lambda - x0 - x1) % modulus
        val y2 = (lambda * (x0 - x2) - y0) % modulus

        EcPointAffine(x2, y2)
      }
    }
    ret
  }

  def pdbl(p: EcPointAffine) = padd(p, p)

  /** By far the fastest impl for PADD on BLS-377, 11S + 5M required
   * @see [[https://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl]]
   */
  def paddByProj(p0: EcPointProj, p1: EcPointProj) = {
    require(a.intValue() == 0)

    val (x0, x1, y0, y1, z0, z1) = (p0.x, p1.x, p0.y, p1.y, p0.z, p1.z)

    def zpSquare(a: IntZ) = zp.multiply(a, a)

    val ret = {
      if (p0.isZero) p1
      else if (p1.isZero) p0
      else if (p0 != p1) {
        val z0Squared = zpSquare(z0)
        val z0Cubed = zp.multiply(z0, z0Squared)
        val z1Squared = zpSquare(z1)
        val z1Cubed = zp.multiply(z1, z1Squared)
        val u0 = zp.multiply(x0, z1Squared)
        val u1 = zp.multiply(x1, z0Squared)
        val s0 = zp.multiply(y0, z1Cubed)
        val s1 = zp.multiply(y1, z0Cubed)
        val h = zp.subtract(u1, u0)

        val i = zpSquare(h * 2)
        val j = zp.multiply(h, i)
        val r = (zp.subtract(s1, s0) * 2) % modulus
        val rSquared = zpSquare(r)
        val v = zp.multiply(u0, i)

        val x2 = (rSquared - j - v * 2) % modulus
        val y2temp0 = zp.subtract(v, x2)
        val y2temp1 = zp.multiply(r, y2temp0)
        val y2temp2 = (zp.multiply(s0, j) * 2) % modulus
        val y2 = zp.subtract(y2temp1, y2temp2)
        val zSum = z0 + z1
        val z2temp0 = zpSquare(zSum)
        val z2temp1 = z2temp0 - z0Squared - z1Squared
        val z2 = zp.multiply(z2temp1, h)

        EcPointProj(x2, y2, z2)
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

        EcPointProj(x2, y2, z2)
      }
    }
    ret
  }

  def pdblByProj(p: EcPointProj) = paddByProj(p, p)

  def pmult(k: IntZ, p: EcPointAffine) = {
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

  def pmultByProj(k: IntZ, p: EcPointProj) = {
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