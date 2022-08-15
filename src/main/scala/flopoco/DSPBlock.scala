package org.datenlord
package flopoco

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

// TODO: finish & verify this
case class DSPBlockConfig(widths: Seq[Int], xSigned: Boolean, ySigned: Boolean, preAdd: Int, postAdd: Int) extends Flopoco {

  val Seq(x, y, z) = widths

  override val operatorName = "DSPBlock"
  val usePreAdder = preAdd == 1 || preAdd == -1
  val preAdderSubtracts = preAdd == -1
  val usePostAdder = postAdd == 1

  override val params = Seq(("wX", x), ("wY", y), ("wZ", z), ("xIsSigned", xSigned.toInt), ("yIsSigned", ySigned.toInt), ("isPipelined", 1),
    ("usePreAdder", usePreAdder.toInt), ("preAdderSubtracts", preAdderSubtracts.toInt), ("usePostAdder", usePostAdder.toInt))
  override val widthsIn = if(usePreAdder) widths.head +: widths else widths

  /** black box used in synthesis
   */
  override def blackbox = DSPBlock(this)

  /** rtl model used in simulation
   */
  override def model(dataIn: Seq[Bits]) = {
    val ret = {
      if (preAdd == 0 && postAdd == 0) (dataIn(0).asUInt * dataIn(1).asUInt).d(1).asBits
      else if (preAdd != 0 && postAdd == 0) ((dataIn(0).asUInt +^ dataIn(1).asUInt).d(1) * dataIn(2).asUInt.d(1)).d(1).asBits
      else if (preAdd == 0 && postAdd != 0) ((dataIn(0).asUInt * dataIn(1).asUInt).d(1) +^ dataIn(2).asUInt.d(1)).d(1).asBits
      else (((dataIn(0).asUInt +^ dataIn(1).asUInt).d(1) * dataIn(2).asUInt.d(1)) +^ dataIn(3).asUInt.d(2)).d(1).asBits
    }
    Seq(ret)
  }

  override def impl(dataIn: Seq[Any]) = {
    val bigints = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = {
      if (preAdd == 0 && postAdd == 0) bigints(0) * bigints(1)
      else if (preAdd != 0 && postAdd == 0) (bigints(0) + bigints(1)) * bigints(2)
      else if (preAdd == 0 && postAdd != 0) bigints(0) * bigints(1) + bigints(2)
      else (bigints(0) + bigints(1)) * bigints(2) + bigints(3)
    }
    Seq(ret)
  }

  override val size =
    if (preAdd == 0 && postAdd == 0) (2,1)
    else if (preAdd != 0 && postAdd == 0) (3,1)
    else if (preAdd == 0 && postAdd != 0) (3,1)
    else (4,1)
}

case class DSPBlock(config: DSPBlockConfig) extends FlopocoBlackBox {

  import config._

  val X = if (preAdd != 0) null else in Bits (x bits)
  val X1 = if (preAdd != 0) in Bits (x bits) else null
  val X2 = if (preAdd != 0) in Bits (x bits) else null
  val Y = in Bits (y bits)
  val Z = if (postAdd != 0) in Bits (z bits) else null

  val widthOut =
    if (preAdd == 0 && postAdd == 0) x + y
    else if (preAdd != 0 && postAdd == 0) x + 1 + y
    else if (preAdd == 0 && postAdd != 0) ((x + y) max z) + 1
    else ((x + 1 + y) max z) + 1

  val R = out Bits (widthOut bits)

  override def asNode = (dataIn: Seq[Bits]) => {
    Seq(X, X1, X2, Y, Z).filter(_ != null).zip(dataIn).foreach { case (port, data) => port := data }
    Seq(this.R)
  }
}
