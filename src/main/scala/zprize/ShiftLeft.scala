package org.datenlord
package zprize
import spinal.core.Bits

case class ShiftLeft(shift:Int, width:Int) extends Combinational{

  override def comb(dataIn: Seq[Bits]) = dataIn.map(_ << shift)

  override def name = s"shift_$shift"

  override val impl = (dataIn: Seq[Any]) => dataIn.asInstanceOf[Seq[BigInt]].map(_ << shift)

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width + shift))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}

case class Resize(widthIn:Int, widthOut:Int) extends Combinational{

  override def comb(dataIn: Seq[Bits]): Seq[Bits] = dataIn.map(_.resize(widthOut))

  override def name = s"resize_${widthIn}_$widthOut"

  override val impl = (dataIn: Seq[Any]) => dataIn.asInstanceOf[Seq[BigInt]]

  override var inputTypes = Seq(UIntInfo(widthIn))
  override var outputTypes = Seq(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}
