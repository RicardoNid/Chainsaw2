package org.datenlord
package zprize

import spinal.core._

case class Split(width: Int, lowWidth: Int) extends Combinational {

  def comb(dataIn: Seq[Bits]): Seq[Bits] = {
    val (a, b) = dataIn.head.splitAt(lowWidth)
    Seq(a, b)
  }

  override def name = s"split_${width}_${lowWidth}"

  override def impl(dataIn: Seq[Any])  =  {
    val (high, low) = dataIn.asInstanceOf[Seq[BigInt]].head.split(lowWidth)
    Seq(high, low)
  }

  override var inputTypes = Seq(UIntInfo(width))
  override var outputTypes = Seq(UIntInfo(width - lowWidth), UIntInfo(lowWidth))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
}
