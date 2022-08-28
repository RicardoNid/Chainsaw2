//package org.datenlord
//package arithmetic
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.lib.fsm._
//
//case class PipelinedAdderConfig(widthIn: Int) extends TransformBase {
//  override def impl(dataIn: Seq[Any]) = Seq(dataIn.asInstanceOf[Seq[BigInt]].sum)
//
//  override val size = (2, 1)
//
//  override def latency = (widthIn + binaryAddLimit - 1) / binaryAddLimit
//
//  override def implH = PipelinedAdder(this)
//}
//
//case class PipelinedAdder(config: PipelinedAdderConfig) extends TransformModule[UInt, UInt] {
//
//  import config._
//
//  override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), 2))
//
//  val xSegments = dataIn.fragment.head.
//
//  override val dataOut = master Flow Fragment(Vec(UInt(widthIn + 1 bits), 1))
//
//}
