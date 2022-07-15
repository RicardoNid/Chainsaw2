//package org.datenlord
//package dfg
//
//import org.scalatest.flatspec.AnyFlatSpec
//
//import scala.collection.mutable.ArrayBuffer
//import scala.util.Random
//import ArithmeticGraphs.addGraph
//
//class RingDagTest extends AnyFlatSpec {
//
//  val width = 377 // 3 * 191 - 1
//  val data = (0 until 4).map(_ => Random.nextBigInt(width))
//  val golden = data.sum
//
//  val bigGraph = new RingDag
//  val inputs = (0 until 4).map(i => bigGraph.addInput(s"in_$i", width))
//  val outputs = (0 until 1).map(i => bigGraph.addOutput(s"out_$i", width + 2))
//
//  val g0 = addGraph(width)
//  val g1 = addGraph(width)
//  val g2 = addGraph(width + 1)
//
//  val mid0 = bigGraph.addGraphsAfter(g0, inputs.take(2))
//  val mid1 = bigGraph.addGraphsAfter(g1, inputs.takeRight(2))
//  bigGraph.addGraphBetween(g2, mid0 ++ mid1, outputs)
//
//  "RingDag" should "has correct software implementation" in assert(bigGraph.evaluateS(data).head == golden)
//
//  "checkWidths" should "work" in bigGraph.checkWidths
//
//  "break bundles" should "work" in {
//    val latency0 = bigGraph.latencyLowerBound
//    bigGraph.breakBundles()
//    val latency1 = bigGraph.latencyLowerBound
//    TransformTest.test(bigGraph.toTransform(), data)
//  }
//
//}