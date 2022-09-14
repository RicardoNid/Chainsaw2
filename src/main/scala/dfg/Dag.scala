package org.datenlord
package dfg

import org.jgrapht._
import org.jgrapht.alg.shortestpath.FloydWarshallShortestPaths
import org.jgrapht.graph._
import spinal.core._

import java.util.function.Supplier
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

//object OpType extends Enumeration {
//  val Var = Value // variable (opposite to operation)
//  val FullMult, LowMult, HighMult, SquareMult, BASEADD, BASESUB, ADD, SUB, ADDC, SUBC, MERGE, SPLIT, RESIZE, MUX, AND, SHIFT, COMPRESS, KARA, CPA, BCM = Value // Ring operations
//  type OpType = Value
//
//  def fromMultMode(mode:MultiplierMode) = {
//    mode match {
//      case FULL => FullMult
//      case LSB => LowMult
//      case MSB => HighMult
//      case SQUARE => SquareMult
//    }
//  }
//}

object Direction extends Enumeration {
  val In, Out = Value
  type Direction = Value
}

import dfg.Direction._

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object VarVertex {
  def apply[THard <: Data](name: String) =
    new DagVertex(name, 0, Var,  (data: Seq[THard]) => data)
}

class DagPort[THard <: Data](val vertex: DagVertex[THard], val order: Int, val direction: Direction)

object DagPort {
  def apply[THard <: Data](vertex: DagVertex[THard], order: Int, direction: Direction): DagPort[THard] = new DagPort(vertex, order, direction)
}

class Dag[THard <: Data](val name: String)
  extends DirectedWeightedMultigraph[DagVertex[THard], DagEdge](classOf[DagEdge]) {

  implicit val ref: Dag[THard] = this

  setEdgeSupplier(new Supplier[E] {
    override def get() = {
      val e = new E(0, 0)
      println(e.hashCode())
      e
    }
  })

  type V = DagVertex[THard]
  type E = DagEdge
  type Port = DagPort[THard]

  val inputs = ArrayBuffer[V]()
  val outputs = ArrayBuffer[V]()
  var retimingInfo = Map[V, Int]()

  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V) = super.addEdge(sourceVertex, targetVertex)

  @deprecated
  override def addEdge(sourceVertex: V, targetVertex: V, e: E) = super.addEdge(sourceVertex, targetVertex, e)

  def addEdge(source: Port, target: Port, weight: Double = 0) = {
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

  // basic methods for retiming
  def isComb: Boolean = edgeSet().forall(_.weight == 0)

  def makeComb(): Unit = if (!isComb) edgeSet().foreach(e => setEdgeWeight(e, 0.0))

  def autoPipeline() = AutoPipeline(this)

  /**
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def retiming(solution: Map[V, Int]): Unit = edgeSet().foreach(e => setEdgeWeight(e, getEdgeWeight(e) + solution(getEdgeTarget(e)) - solution(getEdgeSource(e))))

  /** Retiming for minimum latency under feasibility constraints
   */
  def validate() = {
    makeComb()
    simplify()
    autoPipeline()
    this
  }

  def implH = ImplH(this)

  def evaluateH(dataIns: Seq[THard]) = implH.apply(dataIns)

  def simplify() = Simplify(this)

  def addGraphsAfter(sourceGraph: Dag[THard], starts: Seq[Port]): Seq[DagPort[THard]] = {
    require(sourceGraph.inputs.length == starts.length)
    require(starts.forall(_.direction == Out))
    //    require(starts.forall(_.vertex.outDegree == 0))
    // add
    Graphs.addGraph(this, sourceGraph) // add all vertices and edges of that to this, but the edge weights won't be copied
    sourceGraph.edgeSet().foreach(e => setEdgeWeight(e, sourceGraph.getEdgeWeight(e))) // copy the edge weights
    // link
    starts.zip(sourceGraph.inputs).foreach { case (port, in) => addEdge(port, in.in(0)) }
    // return output ports of source graph, which are part of this graph now
    sourceGraph.outputs.map(_.out(0))
  }

  def addGraphBetween(source: Dag[THard], starts: Seq[Port], ends: Seq[Port]): Unit = {
    require(source.outputs.length == ends.length)
    require(ends.forall(_.direction == In))
    require(ends.forall(_.vertex.inDegree == 0))
    // add & link inputs
    val outputs = addGraphsAfter(source, starts)
    // link outputs
    ends.zip(outputs).foreach { case (port, out) => addEdge(out, port) }
  }

  def assureAcyclic(): Unit = assert(!new alg.cycle.CycleDetector(this).detectCycles())

  def getIoPath: Seq[V] = {
    var current = inputs.head
    val path = ArrayBuffer[V](current)
    while (!outputs.contains(current)) {
      current = current.targets.head
      path += current
    }
    logger.info(s"io path ${path.mkString("->")}")
    path
  }

  /** This latency is true only when the graph is homogeneous
   */
  def latency: Int = getIoPath.init.zip(getIoPath.tail)
    .map { case (s, t) => getEdge(s, t).weight}.sum.toInt

  def pathToString(path: GraphPath[V, E]) = {
    path.getVertexList.zip(path.getEdgeList)
      .map { case (v, e) => s"$v -> ${e.weight} -> " }.mkString("\n") +
      path.getEndVertex.toString
  }

  def latencyLowerBound = {
    makeComb()
    edgeSet().toSeq.foreach(e => setEdgeWeight(e, -e.source.latency))
    val algo = new FloydWarshallShortestPaths(this)
    val paths = inputs.flatMap(in => outputs.map(out => algo.getPath(in, out)))
    val shortestPath = paths.filterNot(_ == null).minBy(_.getWeight)
    logger.info(s"longest path:\n${pathToString(shortestPath)}")
    val min = shortestPath.getWeight
    makeComb()
    logger.info(s"minimum latency of current graph is ${-min}")
    -min
  }

  /** Add design rules here, invoked before impl
   *
   */
  def doDrc() = {
    assureAcyclic()
    assert(vertexSet().filter(_.latency > 0).forall(_.outDegree > 0))
  }

  override def toString =
    s"vertices:\n${vertexSet().mkString("\n")}\n" +
      s"edges:\n${edgeSet().map(_.toStringInGraph).mkString("\n")}"
}