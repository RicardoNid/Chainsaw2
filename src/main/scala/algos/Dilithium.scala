package org.datenlord
package algos

import cc.redberry.rings
import rings.poly.PolynomialMethods._
import rings.scaladsl.{UnivariatePolynomial, _}
import syntax._
import rings.primes._

object Dilithium {

  implicit class modOps(bigInt: BigInt) {

    def mod(implicit modulus: BigInt): BigInt = BigInt(Zp(modulus)(bigInt).toByteArray)

    def modpm(implicit modulus: BigInt): BigInt = {
      val temp = mod(modulus)
      val ret = if (temp > (modulus / 2)) temp - modulus
      else temp
      ret
    }

    def norm(implicit modulus: BigInt) = bigInt.modpm.abs
  }

  implicit class polyOps(poly: UnivariatePolynomial[IntZ]) {

    def coeffs = (0 to 256).map(poly.get).map(intz => BigInt(intz.toByteArray))

    def norm(implicit modulus: BigInt) = poly.coeffs.maxBy(_.norm)

  }

  val q = (BigInt(1) << 23) - (BigInt(1) << 13) + BigInt(1) // bit width = 23
  val zq = Zp(q)
  val polyRing = UnivariateRing(zq, "x")
  val poly = polyRing("x ^ 256 + 1")

  def SampleInBall(rho: Seq[Boolean]) = {

  }

  def main(args: Array[String]): Unit = {

    implicit val q = (BigInt(1) << 23) - (BigInt(1) << 13) + BigInt(1) // bit width = 23
    println(poly.norm)


  }


  def centeredReduction(data: BigInt, modulus: BigInt) = Zp(modulus)(data)

  def decompose(data: BigInt, modulus: BigInt) = {

  }
}
