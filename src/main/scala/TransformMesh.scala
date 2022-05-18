package org.datenlord

import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

/** transform with repetition
 *
 */
case class TransformMesh(base: TransformConfig, repetition: Repetition) extends TransformConfig {

  require(base.spaceFold == 1 && base.timeFold == 1)

  override val size = repetition.expand(base.size)

  override def ⊗(factor: Int, step: Int = -1) = TransformMesh(base, repetition.⊗(factor, step))

  override def ∏(factor: Int) = TransformMesh(base, repetition.∏(factor))

  override def impl(dataIn: Seq[Any]) = repetition.divide(dataIn).flatMap { segment =>
    var temp = segment
    (0 until repetition.timeFactor).foreach(_ => temp = base.impl(temp))
    temp
  }

  override def latency = base.latency * repetition.timeFactor

  override def flowFormat = PeriodicFlow(base, repetition, Reuse.unit)

  def implForTest[Tin <: Data : ClassTag, TOut <: Data : ClassTag](typeIn: HardType[Tin], typeOut: HardType[TOut]) = {
    val meshConfig = this
    new TransformModule[Tin, TOut] {
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(typeIn, size._1))
      override val dataOut = master Flow Fragment(Vec(typeOut, size._2))

      val core: TransformModule[Bits, Bits] = implH
      core.dataIn.assignFromBits(dataIn.asBits)
      dataOut.assignFromBits(core.dataOut.asBits)
    }
  }

  override def implH = {
    val meshConfig = this
    new TransformModule[Bits, Bits] {

      val cores = Seq.tabulate(repetition.spaceFactor, repetition.timeFactor)((_, _) => base.implH)
      val wrappers = cores.map(_.map(_.getWrapper()))
      val inBitWidth = wrappers.head.head._1.fragment.head.getBitsWidth
      val outBitWidth = wrappers.head.head._2.fragment.head.getBitsWidth

      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(inBitWidth bits), size._1))
      override val dataOut = master Flow Fragment(Vec(Bits(outBitWidth bits), size._2))

      val dataIns = repetition.divide(dataIn.fragment)
      wrappers.zip(dataIns).foreach { case (row, data) =>
        val segment = ChainsawFlow(Vec(data.map(_.asBits)), dataIn.valid, dataIn.last)
        row.head._1 << segment
        row.prevAndNext { case (prev, next) => next._1 << prev._2 }
      }
      dataOut.fragment.zip(wrappers.flatMap(_.last._2.fragment))
        .foreach { case (out, core) => out.assignFromBits(core) }

      autoLast()
      autoValid()
    }
  }
}

object TransformMesh extends App {

  import breeze.math._

  val dataType = HardType(SFix(2 exp, -13 exp))
  val coeffs = Seq(Complex(1, 0), Complex(2, 0))
  val config = arithmetic.ComplexLUTConfig(coeffs, dataType)
  val data = Array(0, 1).map(BigInt(_))

  val mesh = config ⊗ 2

  SpinalConfig(netlistFileName = "lut.sv").generateSystemVerilog((config ⊗ 2).implForTest(UInt(1 bits), ComplexFix(dataType)))
  TransformTest.test(mesh.implForTest(UInt(1 bits), ComplexFix(dataType)), data)
  VivadoSynth(mesh.implForTest(UInt(1 bits), ComplexFix(dataType)))
}
