package org.datenlord
package dfg

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import ArithmeticGraphs.addGraph

class RingDagTest extends AnyFlatSpec {

  val width = 377
  val data = (0 until 4).map(_ => Random.nextBigInt(width))
  val golden = data.sum

  val bigGraph = new RingDag
  val inputs = (0 until 4).map(i => bigGraph.setInput(s"in_$i", width))
  val outputs = (0 until 1).map(i => bigGraph.setOutput(s"out_$i", width + 2))

  val g0 = addGraph(width)
  val g1 = addGraph(width)
  val g2 = addGraph(width + 1)

  val mid0 = bigGraph.addGraphsAfter(g0, inputs.take(2))
  val mid1 = bigGraph.addGraphsAfter(g1, inputs.takeRight(2))
  bigGraph.addGraphBetween(g2, mid0 ++ mid1, outputs)

  "RingDag" should "has correct software implementation" in assert(bigGraph.evaluateS(data).head == golden)

  it should "has correct widths" in bigGraph.checkWidths

  it should "be optimized correctly" in {
    bigGraph.validate(100)
    assert(bigGraph.latency == 4)
    assert(bigGraph.evaluateS(data).head == golden)
    TransformTest.test(bigGraph.toTransform(), data)
  }

}