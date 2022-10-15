package org.datenlord
package zprize

import org.datenlord.dfg.{Dag, RingDag}
import org.jgrapht._
import org.jgrapht.alg.connectivity.ConnectivityInspector
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
case class IoGenerator(width: Int, numericType: NumericTypeInfo, direction: Direction) extends ChainsawGenerator {
  override def name = if (direction == In) "in" else "out"

  override val impl = (dataIn: Seq[Any]) => dataIn
  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var inputTypes = Seq(numericType)
  override var outputTypes = Seq(numericType)
  override var latency = 0

  override def implH = null // this shouldn't be called anyway
}

object InputVertex {
  def apply(width: Int, numericType: NumericTypeInfo)(implicit ref: Dag) = {
    val vertex = DagVertex(IoGenerator(width, numericType, In))
    vertex.setName(s"i_${ref.inputs.length}")
    ref.addVertex(vertex)
    ref.inputs += vertex
    vertex.out(0)
  }
}

object OutputVertex {
  def apply(width: Int, numericType: NumericTypeInfo)(implicit ref: Dag) = {
    val vertex = DagVertex(IoGenerator(width, numericType, Out))
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
  def addGraphBetween(subGraph: Dag, starts: Seq[Port], ends: Seq[Port]) = {
    // check
    require(subGraph.inputs.length == starts.length, s"${subGraph.inputs.length} != ${starts.length}")
    require(starts.forall(_.direction == Out))
    require(subGraph.outputs.length == ends.length, s"${subGraph.outputs.length} != ${ends.length}")
    require(ends.forall(_.direction == In))
    // add
    val verticesMap = subGraph.vertexSet()
      .filterNot(_.isIo(subGraph))
      .toSeq.map(v => v -> v.cloneTo(this)).toMap
    subGraph.edgeSet()
      .filterNot(e => e.source(subGraph).isIo(subGraph) || e.target(subGraph).isIo(subGraph))
      .foreach { e =>
        val oldSourcePort = e.sourcePort(subGraph)
        val oldTargetPort = e.targetPort(subGraph)
        val sourcePort = verticesMap(oldSourcePort.vertex).out(oldSourcePort.order)
        val targetPort = verticesMap(oldTargetPort.vertex).in(oldTargetPort.order)
        addEdge(sourcePort, targetPort)
      }
    // link
    val startTargets = subGraph.inputs.map { in =>
      val target = verticesMap(in.targets(subGraph).head)
      val order = in.targetPorts(subGraph).head.order
      DagPort(target, order, In)
    }
    val endSources = subGraph.outputs.map { out =>
      val source = verticesMap(out.sources(subGraph).head)
      val order = out.sourcePorts(subGraph).head.order
      DagPort(source, order, Out)
    }
    starts.zip(startTargets).foreach { case (port, in) => addEdge(port, in) }
    ends.zip(endSources).foreach { case (port, out) => addEdge(out, port) }
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
  def flatten(): Dag = { // flatten all vertices which are Dags themselves
    Flatten(this)
    logger.info(s"\n----do retiming after flatten----")
    updateLatency()
    updateEstimation()
    this
  }


  /** --------
   * methods for implementation
   * -------- */
  override def implH: ChainsawModule = DagImplH(this)

  def setVerticesAsNaive(): Unit = vertexSet().foreach(_.gen.setAsNaive)


  /** --------
   * methods for getting metadata
   -------- */
  override var inputTypes = Seq(UIntInfo(1))
  override var outputTypes = Seq(UIntInfo(1))
  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  var retimingInfo = Map[V, Int]()
  override var latency = -1 // placeholder which will be overwritten by updateHardwareData

  def updateIO(): Unit = {
    inputTypes = inputs.map(_.gen.inputTypes.head)
    outputTypes = outputs.map(_.gen.outputTypes.head)
    // TODO: methods for generating frame format according to vertices and Dag topology
    inputFormat = inputNoControl
    outputFormat = outputNoControl
  }

  def updateEstimation(): Unit = {
    utilEstimation = vertexSet().map(_.gen.utilEstimation).reduce(_ + _)
    fmaxEstimation = {
      val value = vertexSet().map(_.gen.fmaxEstimation.toDouble).min
      HertzNumber(value)
    }
  }

  def updateLatency(): Unit = {
    autoPipeline()
    latency = retimingInfo(outputs.head) - retimingInfo(inputs.head)
  }

  def graphDone(): Unit = {
    updateIO()
    updateEstimation()
    updateLatency()
    doDrc()
  }

  def assureAcyclic(): Unit = assert(!new alg.cycle.CycleDetector(this).detectCycles(), "dag must be acyclic")

  def assureConnected(): Unit = assert(new ConnectivityInspector(this).isConnected, "dag must be a connected graph")

  override def doDrc(): Unit = {
    super.doDrc()
    assureAcyclic()
    assureConnected()
    vertexSet().toSeq.diff(inputs).foreach { v =>
      v.inPorts.foreach(port => assert(edgeSet().exists(e => e.targetPort == port), s"inPort $port has no driver"))
    } // all inPorts must be driven
    vertexSet().toSeq.diff(outputs).foreach { v =>
      v.outPorts.foreach(port => if (!edgeSet().exists(e => e.sourcePort == port)) logger.warn(s"outPort $port is not used"))
    }
    val notTimed = retimingInfo.keys.toSeq.diff(vertexSet().toSeq)
    assert(notTimed.isEmpty, s"vertices ${notTimed.mkString(" ")} have no retiming value")
  }

  /** --------
   * methods for readability & visualization
   * -------- */
  def pathToString(path: GraphPath[V, E]) = {
    path.getVertexList.zip(path.getEdgeList)
      .map { case (v, e) => s"$v -> ${e.weight} -> " }.mkString("\n") +
      path.getEndVertex.toString
  }

  def toPng(graphName: String = name) = ToPng(this, graphName)

  s"vertices:\n${vertexSet().mkString("\n")}\n" +
    s"edges:\n${edgeSet().map(_.toStringInGraph).mkString("\n")}"

}
