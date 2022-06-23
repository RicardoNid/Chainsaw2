package org.datenlord
package algos

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

object RingsTest extends App {

  val zp = Zp(13)
  val a,b = zp(12)
  println(a + b)

}
