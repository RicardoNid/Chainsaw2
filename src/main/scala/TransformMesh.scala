package org.datenlord

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.reflect.ClassTag

/** transform with repetition
 *
 */
//case class TransformMesh(transform: TransformConfig, repetition: Repetition) extends TransformConfig {
//
//  override def ⊗(factor: Int, step: Int = -1) = TransformMesh(transform, repetition.⊗(factor, step))
//
//  override def ∏(factor: Int) = TransformMesh(transform, repetition.∏(factor))
//
//  def impl[T: ClassTag](dataIn: Array[T]) = repetition.divide(dataIn).map(_.toSeq).map(transform.impl)
//
//  override def latency = transform.latency * repetition.timeFactor
//
//  override def inputFlow = ???
//
//  override def outputFlow = ???
//
//  override def implH = ???
//}
//
//object TransformMesh extends App {
//  val config = flowConverters.StreamPermutationConfig(Seq(2, 3, 0, 1), 2, 2)
//  val data = Array(0,1,2,3,4,5,6,7).map(BigInt(_))
//  println((config ⊗ 2).impl(data).mkString(" "))
//}
