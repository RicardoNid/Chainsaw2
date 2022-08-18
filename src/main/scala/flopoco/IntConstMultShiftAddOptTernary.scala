package org.datenlord
package flopoco

import spinal.core._

import scala.language.postfixOps

case class IntConstMultShiftAddOptTernaryConfig(widthIn: Int, constant: BigInt)
  extends Flopoco {

  override val operatorName = "IntConstMultShiftAddOptTernary"
  override val params = Seq(("wIn", widthIn), ("constant", constant))
  override val widthsIn = Seq(widthIn)

  /** black box used in synthesis
   */
  override def blackbox = IntConstMultShiftAddOptTernary(this)

  /** rtl model used in simulation
   */
  override def model(dataIn: Seq[Bits]) = dataIn.map(_.asUInt * constant).map(_.asBits)

  override def impl(dataIn: Seq[Any]) = dataIn.asInstanceOf[Seq[BigInt]].map(_ * constant)

  override val size = (1, 1)
}

case class IntConstMultShiftAddOptTernary(config: IntConstMultShiftAddOptTernaryConfig)
  extends FlopocoBlackBox {

  import config._

  val X0 = in UInt (widthIn bits)
  val R = out UInt (widthIn + constant.bitLength bits)
  R.setName(s"R_c$constant")

  override def asNode = (dataIn: Seq[Bits]) => {
    val core = this
    core.X0 := dataIn.head.asUInt
    Seq(core.R.asBits)
  }

}
