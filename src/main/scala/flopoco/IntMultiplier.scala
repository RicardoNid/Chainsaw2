package org.datenlord
package flopoco

import xilinx.XilinxDeviceFamily.UltraScale

import spinal.core._

case class IntMultiplierConfig(widthX: Int, widthY: Int, maxDSP: Int, signed: Boolean) extends Flopoco {
  override val operatorName = "IntMultiplier"
  override val params = Seq(("wX", widthX), ("wY", widthY), ("maxDSP", maxDSP), ("signedIO", if (signed) 1 else 0), ("useKaratsuba", 1))
  override val frequency = 800 MHz
  override val family = UltraScale
  override val widthsIn = Seq(widthX, widthY)

  override def blackbox = IntMultiplier(this)

  override def model(dataIn: Seq[Bits]) =
    if (!signed) Seq(dataIn.map(_.asUInt).reduce(_ * _).asBits)
    else Seq(dataIn.map(_.asSInt).reduce(_ * _).asBits)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  override val size = (2, 1)
}

case class IntMultiplier(config: IntMultiplierConfig) extends FlopocoBlackBox {

  import config._

  val X = in Bits (widthX bits)
  val Y = in Bits (widthY bits)
  val R = out Bits (widthX + widthY bits)

  override def asNode = (dataIn: Seq[Bits]) => {
    this.X := dataIn(0)
    this.Y := dataIn(1)
    Seq(this.R)
  }
}
