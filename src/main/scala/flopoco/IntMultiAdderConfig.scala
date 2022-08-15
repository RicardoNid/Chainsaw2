package org.datenlord
package flopoco

import xilinx.XilinxDeviceFamily._

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class IntMultiAdderConfig(widthIn: Int, n: Int, signed: Boolean) extends Flopoco {
  override val operatorName = "IntMultiAdder"
  override val params = Seq(("signedIn", if(signed) 1 else 0), ("n", n), ("wIn", widthIn))
  override val frequency = 800 MHz
  override val family = UltraScale
  override val widthsIn = Seq.fill(n)(widthIn)

  override def blackbox = IntMultiAdder(this)

  override def model(dataIn: Seq[Bits]) =
    if (!signed) Seq(dataIn.map(_.asUInt).reduceBalancedTree(_ +^ _).asBits)
    else Seq(dataIn.map(_.asSInt).reduceBalancedTree(_ +^ _).asBits)

  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)

  override val size = (n, 1)
}

case class IntMultiAdder(config: IntMultiAdderConfig) extends FlopocoBlackBox {

  import config._

  val X = in Vec(Bits(widthIn bits), n)
  X.zipWithIndex.foreach { case (int, i) => int.setName(s"X$i") }
  val widthOut = widthsIn.head + log2Up(n)
  val R = out Bits (widthOut bits)

  override def asNode = (dataIn: Seq[Bits]) => {
    val core = this
    core.X := Vec(dataIn)
    Seq(core.R)
  }
}