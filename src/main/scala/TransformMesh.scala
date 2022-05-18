package org.datenlord

import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag

/** transform with repetition
 *
 */
case class TransformMesh(base: TransformConfig, repetition: Repetition) extends TransformConfig {

  override val size = repetition.expand(base.size)

  override def ⊗(factor: Int, step: Int = -1) = TransformMesh(base, repetition.⊗(factor, step))

  override def ∏(factor: Int) = TransformMesh(base, repetition.∏(factor))

  def impl[T: ClassTag](dataIn: Array[T]) = repetition.divide(dataIn).map(_.toSeq).map(base.impl)

  override def latency = base.latency * repetition.timeFactor

  override def flowFormat = PeriodicFlow(base, repetition, Reuse.unit)

  override def implH = {
    val meshConfig = this
    new TransformModule[Bits, Bits] {

      val cores = Seq.tabulate(repetition.spaceFactor, repetition.timeFactor)((_, _) => base.implHBits)
      val example = cores.head.head
      val widthIn = example.dataIn.fragment.head.getBitsWidth
      val widthOut = example.dataOut.fragment.head.getBitsWidth

      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(widthIn bits), size._1))
      override val dataOut = master Flow Fragment(Vec(Bits(widthOut bits), size._2))

      val dataIns = repetition.divide(dataIn.fragment)
      cores.zip(dataIns).foreach { case (row, data) =>
        row.head.dataIn.fragment.assignFromBits(Vec(data).asBits)
        row.head.dataIn.valid := dataIn.valid
        row.head.dataIn.last := dataIn.last
      }
      cores.foreach { row =>
        row.prevAndNext { case (prev, next) =>
          next.dataIn.fragment.assignFromBits(prev.dataIn.fragment.asBits)
          next.dataIn.valid := prev.dataIn.valid
          next.dataIn.last := prev.dataIn.last
        }
      }
      println(cores.length)
      println(size._2)
      dataOut.fragment.zip(cores.flatMap(_.last.dataOut.fragment))
        .foreach{ case (out, core) => out := core}
      autoLast()
      autoValid()
    }
  }

  override def implHBits = implH
}

object TransformMesh extends App {
  import breeze.math._
  val dataType = HardType(SFix(2 exp, -13 exp))
  val coeffs = Seq(Complex(1,0), Complex(2,0))
  val config = arithmetic.ComplexLUTConfig(coeffs, dataType)
  val data = Array(0,1).map(BigInt(_))

  val mesh = config ⊗ 2
  println((config ⊗ 2).impl(data).mkString(" "))

  SpinalConfig().generateSystemVerilog((config ⊗ 2).implH)
  TransformTest.test(mesh.implH,data)
}
