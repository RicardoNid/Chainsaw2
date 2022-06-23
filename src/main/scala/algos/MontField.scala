package org.datenlord
package algos

import algos.MSM.baseModulus

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

    val T = x
    val TLow = T % R
    val m = (TLow * NPrime) % R
    val t = (T + m * modulus) / R

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

object MontField {
  def testMont() = {
    implicit val baseField = MontField(baseModulus)

    def test(a: BigInt, b: BigInt) = {
      val montA = baseField(a)
      val montB = baseField(b)
      val ret0 = (a * b) % baseModulus
      val ret1 = (montA * montB).toBigInt
      println(s"\ngolden: $ret0, \nyours : $ret1")
      assert(ret0 == ret1, s"\ngolden: $ret0, \nyours : $ret1")
    }

    Seq.tabulate(3, 3)((i, j) => test(baseModulus / 2 - i, baseModulus / 2 + j))
    logger.info("montgomery multiplication is correct")
  }

  def main(args: Array[String]): Unit = {
    testMont()
  }
}