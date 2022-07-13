package org.datenlord
package dfg

import arithmetic.MultplierMode.{Full, Low, Square}

import org.scalatest.flatspec.AnyFlatSpec


class ArithmeticGraphsSynthTest extends AnyFlatSpec {

  val testWidth = 377
  val zprizeModulus = algos.ZPrizeMSM.baseModulus

  def graphAdd = ArithmeticGraphs.addGraph(testWidth)

  def graphSub = ArithmeticGraphs.subGraph(testWidth)

  def graphFull = ArithmeticGraphs.karatsubaGraph(testWidth, Full)

  def graphLow = ArithmeticGraphs.karatsubaGraph(testWidth, Low)

  def graphSquare = ArithmeticGraphs.karatsubaGraph(testWidth, Square)

  def graphMontMult = ArithmeticGraphs.montgomeryGraph(testWidth, zprizeModulus, false, false)
  def graphMontSquare = ArithmeticGraphs.montgomeryGraph(testWidth, zprizeModulus, true, false)

  "synth" should "synth for full" in VivadoSynth(graphFull.toTransform(), "graphFull")
  it should "synth for low" in VivadoSynth(graphLow.toTransform(), "graphLow")
  it should "synth for square" in VivadoSynth(graphSquare.toTransform(), "graphSquare")
  it should "synth for mont mult" in VivadoSynth(graphMontMult.toTransform(), "graphModularMult")
  it should "synth for mont square" in VivadoSynth(graphMontSquare.toTransform(), "graphModularSquare")

}
