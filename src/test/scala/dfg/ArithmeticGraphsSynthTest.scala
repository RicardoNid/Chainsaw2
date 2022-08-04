package org.datenlord
package dfg

import arithmetic.MultplierMode.{FULL, HALF, SQUARE}

import org.scalatest.flatspec.AnyFlatSpec


class ArithmeticGraphsSynthTest extends AnyFlatSpec {

  val testWidth = 377
  val zprizeModulus = algos.ZPrizeMSM.baseModulus

  def graphAdd = ArithmeticGraphs.addGraph(testWidth, 0)

  def graphSub = ArithmeticGraphs.subGraph(testWidth, 0)

  def graphFull = ArithmeticGraphs.karatsubaGraph(testWidth, 0, FULL)

  def graphLow = ArithmeticGraphs.karatsubaGraph(testWidth, 0, HALF)

  def graphSquare = ArithmeticGraphs.karatsubaGraph(testWidth, 0, SQUARE)

  def graphMontMult = ArithmeticGraphs.montgomeryGraph(testWidth, 0, zprizeModulus, false, false)

  def graphMontSquare = ArithmeticGraphs.montgomeryGraph(testWidth, 0, zprizeModulus, true, false)

  behavior of "arithmetic graphs"

  ignore should "synth for full" in VivadoSynth(graphFull.toTransform(), "graphFull")
  ignore should "synth for low" in VivadoSynth(graphLow.toTransform(), "graphLow")
  ignore should "synth for square" in VivadoSynth(graphSquare.toTransform(), "graphSquare")
  ignore should "synth for mont mult" in VivadoSynth(graphMontMult.toTransform(), "graphModularMult")
  ignore should "synth for mont square" in VivadoSynth(graphMontSquare.toTransform(), "graphModularSquare")

}
