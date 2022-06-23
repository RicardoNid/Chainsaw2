import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

val ring = Zp(3329)
val PolyRingOnZp = UnivariateRing(ring, "x")

val poly0: UnivariatePolynomial[IntZ] = PolyRingOnZp("x + 1")
val poly1 = PolyRingOnZp("x^2 + 2 * x + 1")

println(poly0 + poly1)

def fromCoeffs(coeffs: Seq[IntZ]) = {
  val string = coeffs.zipWithIndex
    .filter(_._1 != asBigInteger(0))
    .map { case (coeff, index) => s"$coeff * x^$index" }
    .mkString(" + ")
  println(string)
  PolyRingOnZp(string)
}

implicit class polyUtil(poly: UnivariatePolynomial[IntZ]) {

  val modulus = PolyRingOnZp("x^256 + 1")

  def toCoeffs: Seq[IntZ] =
    (0 until 256).map(poly.get)

  def +:+(that: UnivariatePolynomial[IntZ]) =
    (poly + that) % modulus

  def -:-(that: UnivariatePolynomial[IntZ]) =
    (poly - that) % modulus

  def *:*(that: UnivariatePolynomial[IntZ]) =
    (poly * that) % modulus
}

// x^128 + 1
val coeffsIntZ: Seq[IntZ] = (1 +: Seq.fill(127)(0) :+ 1).map(asBigInteger)
val poly128: UnivariatePolynomial[IntZ] = fromCoeffs(coeffsIntZ)
poly128 * poly128
poly128 *:* poly128

val seq  = (0 until 8)
val half = seq.length / 2
seq.take(half).zip(seq.takeRight(half)).map{ case (i, i1) => i + i1}
seq.take(half).zip(seq.takeRight(half).reverse).map{ case (i, i1) => i + i1}



