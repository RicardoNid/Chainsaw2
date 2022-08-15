package org.datenlord
package flopoco

import spinal.core._

// TODO: find out the shape list
case class BaseMultiplierDSPSuperTilesXilinxConfig(shape: Int) extends Flopoco {

  override val operatorName = this.getClass.getName
  override val params = Seq(("shape", shape), ("pipelined", 1))
  override val widthsIn = Seq(18, 27)

  /** black box used in synthesis
   */
  override def blackbox = BaseMultiplierDSPSuperTilesXilinx(this)

  /** rtl model used in simulation
   */
  override def model(dataIn: Seq[Bits]) = Seq(dataIn.asInstanceOf[Seq[UInt]].reduce(_ * _).asBits)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  override val size = (2, 1)
}

case class BaseMultiplierDSPSuperTilesXilinx(config: BaseMultiplierDSPSuperTilesXilinxConfig) extends FlopocoBlackBox {

  import config._
  val X = in Bits(widthsIn(0) bits)
  val Y = in Bits(widthsIn(1) bits)
  val R = out Bits(widthsIn.sum bits)

  override def asNode = (dataIn: Seq[Bits]) => {
    this.X := dataIn(0)
    this.Y := dataIn(1)
    Seq(this.R)
  }
}