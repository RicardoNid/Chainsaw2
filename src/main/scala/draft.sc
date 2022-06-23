import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

val x  = BigInt("1110000", 2)
val y  = BigInt("1111100", 2)

val overlap =  x.toString(2).reverse.zip(y.toString(2).reverse).dropWhile(_ == ('0', '0')).length

import org.datenlord.algos.MSM.scalarModulus

scalarModulus.toString(10)