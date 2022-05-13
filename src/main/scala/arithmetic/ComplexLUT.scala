package org.datenlord
package arithmetic

import breeze.math._
import spinal.core._
import spinal.lib._

case class ComplexLUTConfig(coeffs: Seq[Complex], dataType: HardType[SFix]) extends TransformConfig {

  val inputBitWidth = log2Up(coeffs.length)

  override def latency = 1

  override def inputFlow = FullyPipelinedFlow(1)

  override def outputFlow = FullyPipelinedFlow(1)

  override def bit2ComplexTransform(dataIn: Seq[BigInt]) = Seq(coeffs(dataIn.head.toInt))


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