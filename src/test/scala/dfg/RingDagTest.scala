package org.datenlord
package dfg

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.assert

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class RingDagTest extends AnyFlatSpec {

  def bigAdderGraph(name: String, width: Int) = {

    val baseWidth = 127

    implicit val graph = new RingDag
    val x = graph.setInput(s"${name}_x", width)
    val y = graph.setInput(s"${name}_y", width)
    val z = graph.setOutput(s"${name}_z", width + 1)

    val splitPoints = (0 until (width - 1) / baseWidth).reverse.map(i => (i + 1) * baseWidth)
    val xs = x.split(splitPoints).reverse // low -> high
    val ys = y.split(splitPoints).reverse

    logger.info(s"x segments: ${xs.length}")

    val carries = ArrayBuffer[RingPort]()
    val sums = ArrayBuffer[RingPort]()

    xs.zip(ys).foreach { case (x, y) =>
      val (carry, sum) =
        if (carries.nonEmpty) x.+^(y, carries.last)
        else x +^ y
      carries += carry
      sums += sum
    }

    val ret = carries.last.merge(sums.reverse) // high -> low
    graph.addEdge(ret, z)

    //                  graph.validate()
    graph
  }

  val width = 377
  val data = (0 until 4).map(_ => Random.nextBigInt(width))
  val golden = data.sum

  val bigGraph = new RingDag
  val inputs = (0 until 4).map(i => bigGraph.setInput(s"in_$i", width))
  val outputs = (0 until 1).map(i => bigGraph.setOutput(s"out_$i", width + 2))
  val mids = (0 until 2).map(i => RingVarVertex(s"mid_$i", width + 1))
  mids.foreach(bigGraph.addVertex)

  val g0 = bigAdderGraph("g0", width)
  val g1 = bigAdderGraph("g1", width)
  val g2 = bigAdderGraph("g2", width + 1)

  bigGraph.addGraphBetween(g0, inputs.take(2), Seq(mids(0)(0)))
  bigGraph.addGraphBetween(g1, inputs.takeRight(2), Seq(mids(1)(0)))
  bigGraph.addGraphBetween(g2, mids.map(_.apply(0)), outputs)

  "RingDag" should "has correct software implementation" in assert(bigGraph.evaluateS(data).head == golden)

  it should "has correct widths" in bigGraph.checkWidths

  it should "be optimized correctly" in {
    bigGraph.validate()
    assert(bigGraph.latency == 4)
    assert(bigGraph.evaluateS(data).head == golden)
  }

}