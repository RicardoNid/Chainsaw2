import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

// MNT4753 parameters

val a: IntZ = Zp(BigInt(5))(3)
val b: IntZ = Zp(BigInt(5))(3)

a + b
