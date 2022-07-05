package org.datenlord
package dfg

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht._
import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph._
import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object OpType extends Enumeration {
  val Var = Value // variable (opposite to operation)
  val FullMult, LowMult, SquareMult, Add, Merge, Split = Value // Ring operations
  type OpType = Value
}

object Direction extends Enumeration {
  val In, Out = Value
  type Direction = Value
}

import dfg.OpType._
import dfg.Direction._

/** This is for vertices which do no operations, this can be used as input, output or intermediate variables
 *
 */
object VarVertex {
  def apply[TSoft, THard <: Data](name: String) =
    new DagVertex(name, 0, Var, (data: Seq[TSoft]) => data, (data: Seq[THard]) => data)
}

class DagPort[TSoft, THard <: Data](val vertex: DagVertex[TSoft, THard], val order: Int)

object DagPort {
  def apply[TSoft, THard <: Data](vertex: DagVertex[TSoft, THard], order: Int): DagPort[TSoft, THard] = new DagPort(vertex, order)
}


class Dag[TSoft, THard <: Data](val name: String)
  extends DirectedWeightedMultigraph[DagVertex[TSoft, THard], DagEdge](classOf[DagEdge]) {

  implicit val ref = this

  type V = DagVertex[TSoft, THard]
  type E = DagEdge
  type Port = DagPort[TSoft, THard]

  val inputs = ArrayBuffer[V]()
  val outputs = ArrayBuffer[V]()

  @deprecated
  override def addEdge(sourceVertex: DagVertex[TSoft, THard], targetVertex: DagVertex[TSoft, THard]) = super.addEdge(sourceVertex, targetVertex)

  @deprecated
  override def addEdge(sourceVertex: DagVertex[TSoft, THard], targetVertex: DagVertex[TSoft, THard], e: E) = super.addEdge(sourceVertex, targetVertex, e)

  def addEdge(source: Port, target: Port, weight: Double = 0) = {
    val e = new E(target.order, source.order)
    super.addVertex(source.vertex)
    super.addVertex(target.vertex)
    super.addEdge(source.vertex, target.vertex, e)
    setEdgeWeight(e, weight)
    e
  }

  def addInput(name: String) = {
    val in = VarVertex[TSoft, THard](name)
    addVertex(in)
    inputs += in
    in
  }

  def addOutput(name: String) = {
    val out = VarVertex[TSoft, THard](name)
    addVertex(out)
    outputs += out
    out
  }

  def retiming(solution: Map[V, Int]): Unit = {
    edgeSet().foreach { e =>
      val newWeight = getEdgeWeight(e) + solution(getEdgeTarget(e)) - solution(getEdgeSource(e))
      setEdgeWeight(e, newWeight)
    }
  }

  /** Do retiming on this DFG, such that constraints for vertices are satisfied with minimum number of registers
   *
   * @return graph after validation
   */
  def validate() = {
    // build graph with dummy vertices
    val originalEdges = edgeSet().toSeq
    val edgeWeights = mutable.Map[E, Double]()
    vertexSet()
      .filter(v => outgoingEdgesOf(v).size() > 1)
      .foreach { v =>
        val outgoingEdges = outgoingEdgesOf(v).toSeq
        val k = outgoingEdges.size
        val latencies = outgoingEdges.map(getEdgeWeight)
        val latencyMax = latencies.max
        val targets = outgoingEdges.map(getEdgeTarget)
        val dummy = VarVertex[TSoft, THard](s"${v.name}_dummy")
        addVertex(dummy)
        val dummyEdges = targets.zip(latencies).map { case (target, latency) => addEdge(target(0), dummy(0), latencyMax - latency) }
        (outgoingEdges ++ dummyEdges)
          .foreach(e => edgeWeights += (e -> 1 / k.toDouble))
      }
    // declare model
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
    // declare variables
    val vertices = vertexSet().toSeq
    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.toString, -20, 20))
    assert(variables.distinct.length == vertices.length)
    val variableMap = Map(vertices.zip(variables): _*)
    // construct cost function for minimum number of registers
    val coeffs: Seq[Double] = vertices.map { v =>
      incomingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum -
        outgoingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum
    }
    val expr = variables.zip(coeffs)
      .map { case (variable, coeff) => variable * coeff }
      .reduce(_ + _)
    minimize(expr)
    // add feasibility constraints
    val constraints = originalEdges.map { e =>
      val source = getEdgeSource(e)
      val target = getEdgeTarget(e)
      variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
    }
    subjectTo(constraints: _*)
    // set linear programming problem and solve it
    start()
    import scala.math.round
    val solution = variableMap.map(pair => pair._1 -> round(pair._2.value.get).toInt)
    retiming(solution)
    release()
    // remove dummy vertices
    vertexSet().filter(_.name.endsWith("dummy")).foreach(removeVertex)
    this
  }

  // TODO: merge implH and implS
  def implH: Seq[THard] => Seq[THard] = (dataIns: Seq[THard]) => {

    validate()

    val signalMap = mutable.Map[V, Seq[THard]]()

    // vertices already implemented
    def implemented: Seq[V] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[V] = vertexSet().toSeq.diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[V] = remained.filter(v => v.sources.forall(implemented.contains(_)))

    def implVertex(target: V): Unit = {
      val incomingEdges = incomingEdgesOf(target).toSeq.sortBy(_.inOrder)
      val dataIns = incomingEdges.map { e =>
        val signal = signalMap(getEdgeSource(e))(e.outOrder)
        signal.d(getEdgeWeight(e).toInt - getEdgeSource(e).latency)
      }
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

    val signalMap = mutable.Map[V, Seq[TSoft]]()

    // vertices already implemented
    def implemented: Seq[V] = signalMap.keys.toSeq

    // vertices not implemented yet
    def remained: Seq[V] = vertexSet().toSeq.diff(implemented)

    // vertices ready to be implemented
    def nextStage: Seq[V] = remained.filter(v => v.sources.forall(implemented.contains(_)))

    def implVertex(target: V): Unit = {
      val incomingEdges = incomingEdgesOf(target).toSeq.sortBy(_.inOrder)
      val dataIns = incomingEdges.map(e => signalMap(getEdgeSource(e))(e.outOrder))
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

  def evaluateS(dataIns: Seq[TSoft]) = implS.apply(dataIns)

  def evaluateH(dataIns: Seq[THard]) = implH.apply(dataIns)

  def eliminateIntermediates() = {
    val inters = vertexSet().filter(_.opType == Var).filterNot(_.isIo)
    inters.foreach { inter =>
      val source = inter.sourcePorts.head
      val target = inter.targetPorts.head
      val weight = getEdgeWeight(incomingEdgesOf(inter).head) + getEdgeWeight(outgoingEdgesOf(inter).head)
      addEdge(source, target, weight)
    }
    inters.foreach(removeVertex)

    this
  }

  def addGraphBetween(source: Dag[TSoft, THard], inputs: Seq[Port], outputs: Seq[Port]): Unit = {
    require(source.inputs.length == inputs.length)
    require(source.outputs.length == outputs.length)
    Graphs.addGraph(this, source) // add all vertices and edges of that to this
    source.edgeSet().foreach(e => setEdgeWeight(e, source.getEdgeWeight(e)))
    // replace input ports
    inputs.zip(source.inputs).foreach { case (port, in) =>
      val originalWeights = source.outgoingEdgesOf(in).map(e => source.getEdgeWeight(e))
      in.targetPorts.zip(originalWeights).foreach { case (targetPort, weight) =>
        addEdge(port, targetPort, weight)
      }
    }
    outputs.zip(source.outputs).foreach { case (port, out) =>
      val originalWeights = source.incomingEdgesOf(out).map(e => source.getEdgeWeight(e))
      out.sourcePorts.zip(originalWeights).foreach { case (sourcePort, weight) =>
        addEdge(sourcePort, port, weight)
      }
    }
    // remove IOs
    source.inputs.foreach(removeVertex)
    source.outputs.foreach(removeVertex)
  }

  def latency = {
    val algo = new DijkstraShortestPath(this)
    val path = algo.getPath(inputs.head, outputs.head)
    path.getWeight.toInt
  }

  override def toString =
    s"vertices:\n${vertexSet().mkString("\n")}\n" +
      s"edges:\n${edgeSet().map(_.toStringInGraph).mkString("\n")}"
}
