package org.datenlord
package arithmetic

import breeze.math._
import spinal.core._
import spinal.lib._

case class ComplexLUTConfig(coeffs: Seq[Complex], dataType: HardType[SFix]) extends TransformConfig {

  val inputBitWidth = log2Up(coeffs.length)

  override val size = (1, 1)

  override def latency = 1

  override def impl(dataIn: Seq[_]) = Seq(coeffs(dataIn.head.asInstanceOf[BigInt].toInt))

  override def implH = ComplexLUT(this)

  override def implHBits = TransformBitsWrapper(ComplexLUT(this))
}

case class ComplexLUT(config: ComplexLUTConfig) extends TransformModule[UInt, ComplexFix] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(inputBitWidth bits)))
  override val dataOut = master Flow Fragment(Vec(ComplexFix(dataType), 1))

  val coeffHard = coeffs.map(CF(_, dataType))
  val ROM = Mem(coeffHard)

  dataOut.fragment := Vec(ROM.readSync(dataIn.fragment.head))
  autoValid()
  autoLast()
}