package org.datenlord
package zprize

import org.datenlord.dfg.Dag
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import zprize.{ChainsawGenerator, DagPort, DagVertex}

import scala.collection.mutable.ArrayBuffer

// TODO: appropriate metadata for consistency
case class PassThrough(width: Int) extends ChainsawGenerator {
  override val name = "void"
  override val impl = (dataIn: Seq[Any]) => dataIn
  override var inputWidths = Seq(width)
  override var outputWidths = Seq(width)
  override val inputType = HardType(Bits())
  override val outputType = HardType(Bits())
  override var latency = 0

  override def implH = null // this shouldn't be called anyway
}

object InputVertex {
  def apply(width: Int)(implicit ref: Dag) = {
    val vertex = DagVertex(PassThrough(width))
    vertex.setName(s"i_${ref.inputs.length}")
    ref.addVertex(vertex)
    ref.inputs += vertex
    vertex.out(0)
  }
}

object OutputVertex {
  def apply(width: Int)(implicit ref: Dag) = {
    val vertex = DagVertex(PassThrough(width))
    vertex.setName(s"o_${ref.outputs.length}")
    ref.addVertex(vertex)
    ref.outputs += vertex
    vertex.in(0)
  }
}

class DagEdge(val inOrder: Int, val outOrder: Int) {

  def source[THard <: Data](implicit ref: Dag): DagVertex = ref.getEdgeSource(this)

  def sourcePort[THard <: Data](implicit ref: Dag) = ref.getEdgeSource(this).out(outOrder)

  def target[THard <: Data](implicit ref: Dag): DagVertex = ref.getEdgeTarget(this)

  def targetPort[THard <: Data](implicit ref: Dag) = ref.getEdgeTarget(this).in(inOrder)

  def weight[THard <: Data](implicit ref: Dag): Double = ref.getEdgeWeight(this)

  def toStringInGraph[THard <: Data](implicit ref: Dag): String =
    s"$source -> $weight -> $target"

  override def toString = ""
}


abstract class Dag()
  extends DirectedWeightedMultigraph[DagVertex, DagEdge](classOf[DagEdge]) with ChainsawGenerator {

  implicit val ref: Dag = this

  type V = DagVertex
  type E = DagEdge
  type Port = DagPort

  val inputs = ArrayBuffer[V]()
  val outputs = ArrayBuffer[V]()

  /** --------
   * construction methods
   * -------- */
  // override & deprecate the superclass method to warn users not to use it
  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V) = super.addEdge(sourceVertex, targetVertex)

  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V, e: E): Boolean = super.addEdge(sourceVertex, targetVertex, e)

  // provide our method
  def addEdge(source: Port, target: Port, weight: Double = 0): E = {
    require(source.direction == Out && target.direction == In)
    val e = new E(target.order, source.order)
    assert(super.addEdge(source.vertex, target.vertex, e))
    setEdgeWeight(e, weight)
    e
  }

  def addVertexWithDrivers(target: V, srcs: Port*): Unit = {
    require(srcs.forall(_.direction == Out))
    addVertex(target)
    srcs.zipWithIndex.foreach { case (port, i) => addEdge(port, target.in(i)) }
  }

  /** add a subgraph into this graph
   *
   * @param starts inputs of the subgraph will be connected to the starts
   * @return outputs of the subgraph which can be used for further construction
   */
  def addGraphsAfter(subGraph: Dag, starts: Seq[Port]): Seq[DagPort] = {
    require(subGraph.inputs.length == starts.length)
    require(starts.forall(_.direction == Out))
    //    require(starts.forall(_.vertex.outDegree == 0))
    // add
    Graphs.addGraph(this, subGraph) // add all vertices and edges of that to this, but the edge weights won't be copied
    subGraph.edgeSet().foreach(e => setEdgeWeight(e, subGraph.getEdgeWeight(e))) // copy the edge weights
    // link
    starts.zip(subGraph.inputs).foreach { case (port, in) => addEdge(port, in.in(0)) }
    // return output ports of source graph, which are part of this graph now
    subGraph.outputs.map(_.out(0))
  }

  /** --------
   * methods for retiming
   * -------- */
  def isComb: Boolean = edgeSet().forall(_.weight == 0)

  def makeComb(): Unit = if (!isComb) edgeSet().foreach(e => setEdgeWeight(e, 0.0))

  /** do retiming according to the retiming solution
   *
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def retiming(solution: Map[V, Int]): Unit = edgeSet().foreach { e =>
    val (targetValue, sourceValue) = (solution(getEdgeTarget(e)), solution(getEdgeSource(e)))
    val weight = targetValue + e.targetPort.relativeTime - (sourceValue + e.sourcePort.relativeTime)
    setEdgeWeight(e, weight)
  }

  def autoPipeline(): Dag = AutoPipeline(this)

  /** --------
   * methods for rewriting
   * -------- */
  //  def simplify() = Simplify(this)


  /** --------
   * methods for implementation
   * -------- */
  override def implH = DagImplH(this)

  def assureAcyclic(): Unit = assert(!new alg.cycle.CycleDetector(this).detectCycles())

  override var inputWidths = Seq(-1)
  override var outputWidths = Seq(-1)
  var retimingInfo = Map[V, Int]()
  override var latency = -1 // placeholder which will be overwritten by updateHardwareData

  def updateHardwareData(): Unit = {
    inputWidths = inputs.map(_.gen.inputWidths.head)
    outputWidths = outputs.map(_.gen.inputWidths.head)
    utilEstimation = vertexSet().map(_.gen.utilEstimation).reduce(_ + _)
    fmaxEstimation = {
      val value = vertexSet().map(_.gen.fmaxEstimation.toDouble).min
      HertzNumber(value)
    }
  }

  def updateLatency(): Unit = latency = retimingInfo(outputs.head) - retimingInfo(inputs.head)

  /** --------
   * methods for readability & visualization
   -------- */
  def pathToString(path: GraphPath[V, E]) = {
    path.getVertexList.zip(path.getEdgeList)
      .map { case (v, e) => s"$v -> ${e.weight} -> " }.mkString("\n") +
      path.getEndVertex.toString
  }

  override def toString =
    s"vertices:\n${vertexSet().mkString("\n")}\n" +
      s"edges:\n${edgeSet().map(_.toStringInGraph).mkString("\n")}"
}
