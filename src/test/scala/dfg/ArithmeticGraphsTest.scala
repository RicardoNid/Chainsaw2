package org.datenlord
package dfg

import arithmetic.MultplierMode._

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConversions._
import scala.util.Random
import dfg.OpType._

class ArithmeticGraphsTest extends AnyFlatSpec {

  val testCount = 10
  val testWidth = 377

  val graphAdd = ArithmeticGraphs.addGraph(testWidth)

  "addGraph" should "work" in {
    val data = (0 until testCount * 2).map(_ => Random.nextBigInt(testWidth))
    data.grouped(2).toSeq.foreach(slice => assert(graphAdd.evaluateS(slice).head == slice.sum))
    TransformTest.test(graphAdd.toTransform(), data)
  }

  //  it should "synth" in VivadoSynth(graphAdd.toTransform(100), "graphAdd")

  val graphSub = ArithmeticGraphs.subGraph(testWidth)

  "subGraph" should "work" in {
    val xs = (0 until testCount).map(_ => Random.nextBigInt(testWidth - 1))
    val ys = (0 until testCount).map(_ => Random.nextBigInt(testWidth - 1))
    val data = xs.zip(ys).flatMap { case (x, y) => Seq(x + y, y) }
    data.grouped(2).toSeq.foreach(slice => assert(graphSub.evaluateS(slice).head == slice(0) - slice(1)))
    TransformTest.test(graphSub.toTransform(), data)
  }

  //  it should "synth" in VivadoSynth(graphSub.toTransform(100), "graphSub")

  Random.setSeed(42)
  val data = (0 until testCount * 2).map(_ => Random.nextBigInt(testWidth))

  def showCost(graph: RingDag) = {
    logger.info(s"latency = ${graph.latency}")
    logger.info(s"number of full mults = ${graph.vertexSet().count(v => v.opType == FullMult)}")
    logger.info(s"number of low mults = ${graph.vertexSet().count(v => v.opType == LowMult)}")
    logger.info(s"number of square mults = ${graph.vertexSet().count(v => v.opType == SquareMult)}")
    logger.info(s"number of all mults = ${graph.vertexSet().count(v => v.opType == FullMult || v.opType == LowMult || v.opType == SquareMult)}")
    logger.info(s"number of adds = ${graph.vertexSet().count(v => v.opType == Add || v.opType == Sub)}")
  }

  // get a brand new graph every time we need it
  def graphFull = ArithmeticGraphs.karatsubaGraph(testWidth, Full)

  def graphLow = ArithmeticGraphs.karatsubaGraph(testWidth, Low)

  def graphSquare = ArithmeticGraphs.karatsubaGraph(testWidth, Square)

  "karatsubaGraph" should "show cost" in {
    logger.info(s"cost of full")
    showCost(graphFull.validate(35))
    logger.info("--------")
    logger.info(s"cost of low")
    showCost(graphLow.validate(25))
    logger.info("--------")
    logger.info(s"cost of square")
    showCost(graphSquare.validate(28))
    logger.info("--------")
  }

  // 35,25,28
  // 39,26,29

  it should "work for full multiplication on software" in data.grouped(2).toSeq.foreach(slice => assert(graphFull.evaluateS(slice).head == slice.product))
  it should "work for full multiplication on hardware" in TransformTest.test(graphFull.toTransform(35), data)

  it should "work for low-bit multiplication on software" in data.grouped(2).toSeq.foreach(slice => assert(graphLow.evaluateS(slice).head == slice.product % (BigInt(1) << testWidth)))
  it should "work for low-bit multiplication on hardware" in TransformTest.test(graphLow.toTransform(25), data)

  it should "work for square multiplication on software" in data.foreach(int => assert(graphSquare.evaluateS(Seq(int, int)).head == int * int))
  it should "work for square multiplication on hardware" in TransformTest.test(graphSquare.toTransform(28), data)

  it should "synth for full" in VivadoSynth(graphFull.toTransform(35), "graphFull")
  it should "synth for low" in VivadoSynth(graphLow.toTransform(25), "graphLow")
  it should "synth for square" in VivadoSynth(graphSquare.toTransform(28), "graphSquare")

}
