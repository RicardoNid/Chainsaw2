package org.datenlord
package arithmetic

import arithmetic.MultplierMode._

import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import spinal.core._
import spinal.lib._

case class ModularMultConfig(modulus: BigInt, square: Boolean = false) extends TransformBase {

  val width = modulus.bitLength

  val multByLut = (x: UInt, y: UInt) => {
    val product = x * y
    product.addAttribute("use_dsp", "no")
    product.d(1)
  }

  // parameters
  val zp = Zp(modulus)
  val R: IntZ = asBigInteger(BigInt(1) << modulus.bitLength)
  val RInverse: IntZ = R.modInverse(modulus)
  val RSquare = (R * R).mod(modulus)
  val NPrime = ((R * RInverse - 1) / modulus).mod(modulus)

  val multConfig0 = if (square) KaratsubaConfig(width, mode = Square) else KaratsubaConfig(width, mode = Full)
  val multConfig1 = KaratsubaConfig(width, Low, constant = NPrime.toBigInt, byDsp = true)
  val multConfig2 = KaratsubaConfig(width, Full, constant = modulus, byDsp = false)

  override def impl(dataIn: Seq[Any]) = {
    val bigInts = dataIn.asInstanceOf[Seq[BigInt]]
    Seq((bigInts.head * bigInts.last * RInverse.toBigInt) % modulus)
  }

  override val size = if (square) (1, 1) else (2, 1)

  override def latency = Seq(multConfig0, multConfig1, multConfig2).map(_.latency).sum

  override def implH = MontgomeryModularMultiplier(this)
}

case class MontgomeryModularMultiplier(config: ModularMultConfig) extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(width bits), 2))
  override val dataOut = master Flow Fragment(Vec(UInt(width bits), 1))

  // sub modules
  val fullMult = multConfig0.implH
  val lowMult = multConfig1.implH
  val constantMult = multConfig2.implH

  val x = dataIn.fragment.head
  val y = dataIn.fragment.last

  if (square) fullMult.dataIn.fragment := Vec(x) else fullMult.dataIn.fragment := Vec(x, y)
  fullMult.skipControl()
  val T = fullMult.dataOut.fragment.head
  val TLow = T.takeLow(width).asUInt // T % R

  lowMult.dataIn.fragment := Vec(TLow)
  lowMult.skipControl()
  val m = lowMult.dataOut.fragment.head

  constantMult.dataIn.fragment := Vec(m)
  constantMult.skipControl()
  val prod = constantMult.dataOut.fragment.head

  val latencyOfT = multConfig1.latency + multConfig2.latency
  val t = (T.d(latencyOfT) +^ prod).takeHigh(width + 1).asUInt

  dataOut.fragment := Vec(Mux(t >= U(modulus, width bits), t - modulus, t).resize(width))
  autoValid()
  autoLast()
}
