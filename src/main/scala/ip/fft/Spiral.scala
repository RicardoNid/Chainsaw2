package org.datenlord
package ip.fft

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class Spiral() extends BlackBox {

  val clk, reset, next = in Bool()
  val next_out = out Bool()
  val X = in Vec(UInt(16 bits), 256)
  val Y = out Vec(UInt(16 bits), 256)
  X.zipWithIndex.foreach { case (x, i) => x.setName(s"X$i") }
  Y.zipWithIndex.foreach { case (y, i) => y.setName(s"Y$i") }

  setDefinitionName("dft_top")
  addRTLPath("/home/ltr/IdeaProjects/Chainsaw2/fft512.v")
}

case class SpiralWrapper() extends Component {

  val next = in Bool()
  val next_out = out Bool()
  val X = in Vec(UInt(16 bits), 256)
  val Y = out Vec(UInt(16 bits), 256)

  val core = Spiral()
  core.next := next
  core.X := X
  Y := core.Y
  next_out := core.next_out

  core.mapCurrentClockDomain(core.clk, core.reset)
}

object SpiralWrapper extends App {
  RtlGen(SpiralWrapper(), "spiral")
  VivadoImpl(SpiralWrapper(), "spiralFft")
}
