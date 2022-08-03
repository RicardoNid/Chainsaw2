package org.datenlord
package algos

import algos.ZPrizeMSM.baseModulus

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

case class MontNumber(value: IntZ) {

  def *(that: MontNumber)(implicit field: MontField) = field.montMult(this, that)

  def +(that: MontNumber)(implicit field: MontField) = field.montAdd(this, that)

  def -(that: MontNumber)(implicit field: MontField) = field.montSub(this, that)

  def toIntZ(implicit field: MontField) = field.fromMont(this)

  def toBigInt(implicit field: MontField) = field.fromMont(this).toBigInt
}

case class MontField(modulus: IntZ) {

  // preparing parameters
  val zp = Zp(modulus)
  val R: IntZ = asBigInteger(BigInt(1) << modulus.bitLength())
  val RInverse: IntZ = R.modInverse(modulus)
  val RSquare = (R * R).mod(modulus)
  val NPrime = ((R * RInverse - 1) / modulus).mod(modulus)

  def apply(value: IntZ) = toMont(value)

  def montRedc(x: IntZ) = {
    val golden = zp.multiply(x, RInverse)

    val T = x // full multiplication when using montMult
    val TLow = T % R
    val m = (TLow * NPrime) % R // low-bits multiplication + constant multiplication
    val t = (T + m * modulus) / R // constant multiplication(modulus is a constant)

    // TODO: find an algo to skip this mux
    val det = t - modulus
    val ret = if (det >= 0) det else t // t \in [0, 2N)

    assert(ret == golden, s"\ngolden: $golden, \nyours : $ret")
    ret
  }

  def toMont(x: IntZ) = MontNumber(montRedc(x * RSquare))

  def fromMont(x: MontNumber) = montRedc(x.value)

  def montMult(x: MontNumber, y: MontNumber) = MontNumber(montRedc(x.value.multiply(y.value)))

  def montAdd(x: MontNumber, y: MontNumber) = MontNumber(x.value.add(y.value).mod(modulus))

  def montSub(x: MontNumber, y: MontNumber) = MontNumber(x.value.subtract(y.value).mod(modulus))
}