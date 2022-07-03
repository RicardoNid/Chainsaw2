package org.datenlord
package dfg

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ImplVertex[TSoft, THard <: Data](
                                        override val name: String,
                                        override val latency: Int,
                                        val implS: Seq[TSoft] => Seq[TSoft] = (data: Seq[TSoft]) => data,
                                        val implH: Seq[THard] => Seq[THard] = (data: Seq[THard]) => data
                                      ) extends TimingVertex(name, latency)

class ImplEdge(val inOrder: Int, val outOrder: Int) extends ChainsawEdge

class ImplPort[TSoft, THard <: Data](val vertex: ImplVertex[TSoft, THard], val order: Int)

object ImplPort {
  def apply[TSoft, THard <: Data](vertex: ImplVertex[TSoft, THard], order: Int): ImplPort[TSoft, THard] = new ImplPort(vertex, order)
}


object IoVertex {
  def apply[TSoft, THard <: Data](name: String) = new ImplVertex[TSoft, THard](name, 0)
}

// directed acyclic graph
class ImplDag[TSoft, THard <: Data] extends TimingDag {

  type Vertex = ImplVertex[TSoft, THard]
  type Edge = ImplEdge
  type Port = ImplPort[TSoft, THard]

  // to keep the order of I/O
  val inputs = ArrayBuffer[Vertex]()
  val outputs = ArrayBuffer[Vertex]()

  def addInput() = {
    val in = IoVertex[TSoft, THard](s"input_${inputs.length}")
    addVertex(in)
    inputs += in
    in
  }

  def addOutput() = {
    val out = IoVertex[TSoft, THard](s"input_${inputs.length}")
    addVertex(out)
    outputs += out
    out
  }

  def addEdge(source: Port, target: Port) = {
    val e = new ImplEdge(target.order, source.order)
    super.addEdge(source.vertex, target.vertex, e)
    setEdgeWeight(e, 0)
    e
  }

  def addEdgeWithWeight(source: Port, target: Port, weight: Double) = {
    val e = addEdge(source, target)
    setEdgeWeight(e, weight)
    e
  }

  def sourcesOf(v: Vertex) = incomingEdgesOf(v).map(getEdgeSource)

  def targetsOf(v: Vertex) = outgoingEdgesOf(v).map(getEdgeTarget)

  def implH = (dataIns: Seq[THard]) => {

    val signalMap = mutable.Map[Vertex, Seq[THard]]()

    // vertices already implemented
    def implemented: Seq[Vertex] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[Vertex] = vertexSet().toSeq.asInstanceOf[Seq[Vertex]].diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[Vertex] = remained.filter(v => sourcesOf(v).forall(implemented.contains(_)))

    def implVertex(target: Vertex): Unit = {
      val incomingEdges = incomingEdgesOf(target).asInstanceOf[Seq[Edge]].sortBy(_.inOrder)
      val dataIns = incomingEdges.map(e => signalMap(getEdgeSource(e).asInstanceOf[Vertex])(e.outOrder))
      val rets = target.implH(dataIns)
      signalMap += target -> rets
    }

    // initialize signalMap
    inputs.zip(dataIns).foreach { case (input, bits) => signalMap += input -> Seq(bits) }
    // implement vertices until no available vertices exist
    while (nextStage.nonEmpty) nextStage.foreach(implVertex)

    if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")

    outputs.flatMap(signalMap(_))
  }

  def implS = (dataIns: Seq[TSoft]) => {

    val signalMap = mutable.Map[Vertex, Seq[TSoft]]()

    // vertices already implemented
    def implemented: Seq[Vertex] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[Vertex] = vertexSet().toSeq.asInstanceOf[Seq[Vertex]].diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[Vertex] = remained.filter(v => sourcesOf(v).forall(implemented.contains(_)))

    def implVertex(target: Vertex): Unit = {
      println(s"implementing $target")
      val incomingEdges = incomingEdgesOf(target).toSeq.asInstanceOf[Seq[Edge]].sortBy(_.inOrder)
      val dataIns = incomingEdges.map(e => signalMap(getEdgeSource(e).asInstanceOf[Vertex])(e.outOrder))
      val rets = target.implS(dataIns)
      signalMap += target -> rets
    }

    // initialize signalMap
    inputs.zip(dataIns).foreach { case (input, bits) => signalMap += input -> Seq(bits) }
    // implement vertices until no available vertices exist
    while (nextStage.nonEmpty) nextStage.foreach(implVertex)

    if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")

    outputs.flatMap(signalMap(_))
  }
}

object ImplDag extends App {
  val add = (dataIn: Seq[BigInt]) => Seq(dataIn.sum)
  val addH = (dataIn: Seq[UInt]) => Seq(dataIn.reduce(_ + _))

  case class Add(override val name: String) extends ImplVertex(name, 1, add, addH)

  val testGraph = new ImplDag[BigInt, UInt]

  val inputs = (0 until 4).map(_ => testGraph.addInput())
  val Seq(i0, i1, i2, i3) = inputs

  val adders = (0 until 3).map(i => Add(s"add$i"))
  val Seq(add0, add1, add2) = adders
  adders.foreach(testGraph.addVertex(_))

  val o0 = testGraph.addOutput()

  testGraph.addEdgeWithWeight(ImplPort(i0, 0), ImplPort(add0, 0), 0)
  testGraph.addEdgeWithWeight(ImplPort(i1, 0), ImplPort(add0, 0), 0)
  testGraph.addEdgeWithWeight(ImplPort(i2, 0), ImplPort(add1, 0), 0)
  testGraph.addEdgeWithWeight(ImplPort(i3, 0), ImplPort(add1, 0), 0)
  testGraph.addEdgeWithWeight(ImplPort(add0, 0), ImplPort(add2, 0), 0)
  testGraph.addEdgeWithWeight(ImplPort(add1, 0), ImplPort(add2, 1), 0)
  testGraph.addEdgeWithWeight(ImplPort(add2, 0), ImplPort(o0, 0), 0)

  val sum = testGraph.implS
  val dataIns = (0 until 4).map(BigInt(_))
  println(sum(dataIns))
  println(testGraph.validate())

}
