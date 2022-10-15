package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SpiralFft(n: Int, streamWidth: Int, bitWidth: Int)
  extends BlackBox {

  setDefinitionName("dft_top")

  val dataIn = in Vec(Bits(bitWidth bits), streamWidth * 2)
  val clk, reset, next = in Bool()
  val dataOut = out Vec(Bits(bitWidth bits), streamWidth * 2)
  val next_out = out Bool()

  dataIn.zipWithIndex.foreach { case (bits, i) => bits.setName(s"X$i") }
  dataOut.zipWithIndex.foreach { case (bits, i) => bits.setName(s"Y$i") }

  mapClockDomain(clock = clk, reset = reset)
  addRTLPath(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/fft/S${n}SW${streamWidth}R8.v")
}

case class SpiralFftWrapper(n: Int, streamWidth: Int, bitWidth: Int) extends Component {
  val dataIn = in Vec(Bits(bitWidth bits), streamWidth * 2)
  val next = in Bool()
  val dataOut = out Vec(Bits(bitWidth bits), streamWidth * 2)
  val next_out = out Bool()

  val core = SpiralFft(n, streamWidth, bitWidth)

  core.dataIn := dataIn
  core.next := next
  dataOut := core.dataOut
  next_out := core.next_out
}

object SpiralFftWrapper extends App {
//  RtlGen(SpiralFftWrapper(512,32,16), "spiralFft")
  VivadoImpl(SpiralFftWrapper(512,64,16))
}
