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

  override def flowFormat = PeriodicFlow(base,repetition, Reuse.unit)

  override def implH = {
    val meshConfig = this
    new TransformModule[Bits, Bits]  {
      override val config = meshConfig
      override val dataIn = slave Flow Fragment(Vec(Bits(4 bits), size._1))
      override val dataOut = master Flow Fragment(Vec(Bits(4 bits), size._2))

      val core = base.implH
      repetition.divide(dataIn.fragment)

    }
  }

}

object TransformMesh extends App {
  val config = flowConverters.StreamPermutationConfig(Seq(2, 3, 0, 1), 2, 2)
  val data = Array(0,1,2,3,4,5,6,7).map(BigInt(_))
  println((config ⊗ 2).impl(data).mkString(" "))
}
