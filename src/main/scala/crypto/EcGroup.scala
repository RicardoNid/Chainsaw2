package org.datenlord
package crypto

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

/** model of Short Weierstrass curves
 *
 * @see [[http://hyperelliptic.org/EFD/g1p/auto-shortw.html]]
 */
case class EcGroup(modulus: IntZ, a: IntZ, b: IntZ) {

  val zp: Ring[IntZ] = Zp(modulus)
  val polyRing = UnivariateRing(zp, "x")
  val curveExpr = s"x ^ 3 + $a * x + $b"
  val curve = polyRing(curveExpr)

  /** get all points in the EC group, works only for small p
   */
  def getAllPoints: Seq[EcPointAffine] = {

    def getSquareRoot(value: IntZ): Seq[IntZ] = {
      require(modulus < 1024)
      val p = modulus.toBigInt.toInt
      (0 until p).filter(i => zp.multiply(i, i) == value % modulus).map(asBigInteger)
    }

    def getPoints(x: IntZ): Seq[EcPointAffine] =
      getSquareRoot(curve.evaluate(x)).map(y => EcPointAffine(x, y))

    val p = modulus.toBigInt.toInt
    (0 until p).map(asBigInteger).flatMap(getPoints)
  }

  def isOnCurve(eccPoint: EcPointAffine) =
    if (eccPoint == EcZeroAffine) true
    else curve.evaluate(eccPoint.x) == eccPoint.y * eccPoint.y % modulus

  /** initialize a point in EC group
   */
  def apply(x: IntZ, y: IntZ): EcPointAffine = {
    val ret = EcPointAffine(x, y)
    assert(isOnCurve(ret), s"point ($x, $y) is not on curve $curveExpr")
    ret
  }

  /** caution! padd & pdbl are actually two different operations in affine coordinates, we merge them for simplicity while using pmul
   *
   * @see [[http://hyperelliptic.org/EFD/g1p/auto-shortw.html]]
   */
  def padd(p0: EcPointAffine, p1: EcPointAffine): EcPointAffine = {
    val (x0, x1, y0, y1) = (p0.x, p1.x, p0.y, p1.y)
    val ret = { // 5 situations in total
      if (p0 == EcZeroAffine) p1 // situation 1
      else if (p1 == EcZeroAffine) p0 // situation 2
      else if (p0 == p1.inverse(this)) EcZeroAffine // situation 3
      else if (p0 == p1) { // situation 4
        val lambda = zp.divideExact(x0 * x0 * 3 + a, y0 * 2)
        val x2 = (lambda * lambda - x0 - x1) % modulus
        val y2 = (lambda * (x0 - x2) - y0) % modulus
        EcPointAffine(x2, y2)
      } else { // situation 5
        val lambda = zp.divideExact(y1 - y0, x1 - x0)
        val x2 = (lambda * lambda - x0 - x1) % modulus
        val y2 = (lambda * (x0 - x2) - y0) % modulus
        EcPointAffine(x2, y2)
      }
    }
    ret
  }

  // caution! padd & pdbl are actually two different operations in affine coordinates, we merge them for simplicity while using pmul
  def pdbl(p: EcPointAffine) = padd(p, p)

  /** By far the fastest impl for PADD on BLS-377 using Jacobi projective coordinates, 11S + 5M required
   *
   * @see [[https://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl]]
   */
  def paddJacobi(p0: EcPointProj, p1: EcPointProj) = {
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

  // caution! padd & pdbl are actually two different operations in Jacobi projective coordinates, we merge them for simplicity while using pmul
  def pdblJacobi(p: EcPointProj) = paddJacobi(p, p)

  /** algorithm for padd & pdbl on homogeneous projective coordinate, 12M required
   *
   * @see ''Complete addition formulas for prime order elliptic curves, Joost Renes, Craig Costello, Lejla Batina'' for the algorithm
   * @see ''PipeMSM: Hardware Acceleration for Multi-Scalar Multiplication, Charles. F. Xavier'' [[https://eprint.iacr.org/2022/999.pdf]] for the pipeline
   */
  def paddHomo(p0: EcPointProj, p1: EcPointProj) = {
    val (x1, x2, y1, y2, z1, z2) = (p0.x, p1.x, p0.y, p1.y, p0.z, p1.z)
    // parallel version with a well-designed pipeline
    // stage0
    val m00 = zp.multiply(x1, x2) // x1x2
    val m01 = zp.multiply(y1, y2) // y1y2
    val m02 = zp.multiply(z1, z2) // z1z2
    val a00 = zp.add(x1, y1) // x1+y1
    val a01 = zp.add(x2, y2) // x2+y2
    val a02 = zp.add(x1, z1) // x1+z1
    val a03 = zp.add(x2, z2) // x2+z2
    val a04 = zp.add(y1, z1) // y1+z1
    val a05 = zp.add(y2, z2) // y2+z2
    // stage1
    val a10 = zp.add(m00, m01) // x1x2 + y1y2
    val a11 = zp.add(m00, m02) // x1x2 + z1z2
    val a12 = zp.add(m01, m02) // y1y2 + z1z2
    val m10 = zp.multiply(a00, a01) // x1x2 + y1y2 + x1y2 + x2y1
    val m11 = zp.multiply(a02, a03) // x1x2 + z1z2 + x1z2 + x2z1
    val m12 = zp.multiply(a04, a05) // y1y2 + z1z2 + y1z2 + y2z1
    // stage 2
    val tri20 = m02 * 3 // 3z1z2
    val s20 = zp.subtract(m10, a10) // x1y2 + x2y1
    val s21 = zp.subtract(m11, a11) // x1z2 + x2z1
    val s22 = zp.subtract(m12, a12) // y1z2 + y2z1
    // stage 3
    val tri30 = m00 * 3 // 3x1x2
    val a30 = zp.add(m01, tri20) // y1y2 + 3z1z2
    val s30 = zp.subtract(m01, tri20) // y1y2 - 3z1z2
    val tri31 = s21 * 3 // 3(x1z2 + x2z1)
    // stage 4
    val m40 = zp.multiply(s30, s20) // (y1y2 - 3z1z2)(x1y2 + y2x1)
    val m41 = zp.multiply(tri31, s22) // 3(x1z2 + x2z1)(y1z2 + y2z1)
    val m42 = zp.multiply(a30, s30) // (y1y2 + 3z1z2)(y1y2 - 3z1z2
    val m43 = zp.multiply(tri30, tri31) // 9(x1x2)(x1z2 + x2z1)
    val m44 = zp.multiply(a30, s22) // (y1y2 + 3z1z2)(y1z2 + y2z1)
    val m45 = zp.multiply(tri30, s20) // 3x1x2(x1y2 + x2y1)
    // stage 5
    val s50 = zp.subtract(m40, m41)
    val a50 = zp.add(m42, m43)
    val a51 = zp.add(m44, m45)

    //    val x3 = ((x1 * y2 + x2 * y1) * (y1 * y2 - z1 * z2 * 3) - ((y1 * z2 + y2 * z1) * (x1 * z2 + x2 * z1) * 3) + modulus) % modulus
    //    val y3 = ((y1 * y2 + z1 * z2 * 3) * (y1 * y2 - z1 * z2 * 3) + ((x1 * z2 + x2 * z1) * x1 * x2 * 9) + modulus) % modulus
    //    val z3 = ((y1 * z2 + y2 * z1) * (y1 * y2 + z1 * z2 * 3) + ((y1 * x2 + y2 * x1) * x1 * x2 * 3) + modulus) % modulus
    //
    //    logger.info(s"x3: ${s50 == x3}")
    //    logger.info(s"y3: ${a50 == y3}")
    //    logger.info(s"z3: ${a51 == z3}")

    EcPointProj(s50, a50, a51)
  }

  // for homogeneous projective coordinates which use complete formula, pdbl is the same as padd
  def pdblHomo(p: EcPointProj) = paddHomo(p, p)

  // scala multiplication template for different coordinates
  def multBase[T](k: IntZ, p: T, padd: (T, T) => T) = {
    var temp = p
    k.toBigInt.toString(2).tail.foreach { bit =>
      temp = padd(temp, temp)
      if (bit == '1') temp = padd(temp, p)
    }
    temp
  }

  def pmult(k: IntZ, p: EcPointAffine) = multBase(k, p, padd)

  def pmultJacobi(k: IntZ, p: EcPointProj) = multBase(k, p, paddJacobi)

  def pmultHomo(k: IntZ, p: EcPointProj) = multBase(k, p, paddHomo)
}

object EcGroup {

}

case class EcPointAffine(x: IntZ, y: IntZ) {

  def toProjective = if (this == EcZeroAffine) EcPointProj(1, 1, 0) else EcPointProj(x, y, asBigInteger(1))

  def +(that: EcPointAffine)(implicit eccGroup: EcGroup) = eccGroup.padd(this, that)

  def dbl(implicit eccGroup: EcGroup) = eccGroup.pdbl(this)

  def *(that: IntZ)(implicit eccGroup: EcGroup) = eccGroup.pmult(that, this)

  def inverse(implicit eccGroup: EcGroup) = EcPointAffine(x, (-y) % eccGroup.modulus)

}

object EcZeroAffine extends EcPointAffine(null, null)

/** ecliptic curve point in projective(Jacobi or homogeneous) coordinates
 */
case class EcPointProj(x: IntZ, y: IntZ, z: IntZ) {

  def toAffine(jacobi: Boolean)(implicit eccGroup: EcGroup) = {
    if (isZero) EcZeroAffine
    else {
      val denominators = if (jacobi) (z.pow(2), z.pow(3)) else (z, z)
      val xAffine = eccGroup.zp.divideExact(x, denominators._1)
      val yAffine = eccGroup.zp.divideExact(y, denominators._2)
      EcPointAffine(xAffine, yAffine)
    }
  }

  def +(that: EcPointProj)(implicit eccGroup: EcGroup) = eccGroup.paddJacobi(this, that)

  def dbl(implicit eccGroup: EcGroup) = eccGroup.pdblJacobi(this)

  def *(that: IntZ)(implicit eccGroup: EcGroup) = eccGroup.pmultJacobi(that, this)

  def isZero = z.intValue() == 0
}