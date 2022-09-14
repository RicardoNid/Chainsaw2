package org.datenlord
package examples

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TransformExampleConfig(widthIn:Int) extends TransformBase {
  override def impl(dataIn: Seq[Any]) = dataIn.asInstanceOf[Seq[BigInt]].scan(BigInt(0))(_ + _)

  override val size = (2,1)

  override def latency = 1

  override def implH = TransformExample(this)
}

case class TransformExample(config: TransformExampleConfig)
  extends TransformModule[UInt, UInt] {

  import config._

  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), inputPortWidth))
  override val dataOut = master Flow Fragment(Vec(UInt(widthIn bits), outputPortWidth))

  dataOut.fragment.head := dataIn.fragment.reduce(_ +^ _).d(1)

  autoValid()
  autoLast()
}

//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.lib.fsm._
//
//case class TransformExample() extends TransformBase {
//  override def impl(dataIn: Seq[Any]) = ???
//
//  override val size = _
//
//  override def latency = ???
//
//  override def implH = TransformExample(this)
//}
//
//case class TransformExample(config: TransformExampleConfig) extends TransformModule[, ] {
//  import config._
//  override val dataIn = slave Flow Fragment(Vec(, inputWidth))
//  override val dataOut = master Flow Fragment(Vec(, outputWidth))
//
//  autoValid()
//  autoLast()
//}