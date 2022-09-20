package org.datenlord
package examples

import arithmetic.CpaConfig
import dfg.RingDag

object DfgExample {

  def cpaGraph = {
    val golden = (data: Seq[BigInt]) => Seq(data.sum)
    implicit val graph: RingDag = new RingDag(s"cpa", golden)

    val inputs = (0 until 4).map(i => graph.addInput(s"input$i", 192))
    val output = graph.addOutput("output", 192)
    val mids = inputs.grouped(2).toSeq.flatMap(CpaConfig(192, BinarySubtractor).apply(_: _*))
    output := CpaConfig(192, TernaryAdder).apply(mids: _*).head
    graph
  }

  def main(args: Array[String]): Unit = {
    cpaGraph.validate().toPng("cpa")
  }

}
