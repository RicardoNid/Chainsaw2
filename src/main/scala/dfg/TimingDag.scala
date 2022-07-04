package org.datenlord
package dfg

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht.graph._
import org.jgrapht.{GraphPath, Graphs}

import scala.collection.JavaConversions._
import scala.collection.mutable

class ChainsawVertex(val name: String)

class ChainsawEdge

class TimingVertex(name: String, val latency: Int) extends ChainsawVertex(name)

class DummyVertex(name: String) extends TimingVertex(name, 0)

class TimingDag extends DirectedWeightedMultigraph[TimingVertex, ChainsawEdge](classOf[ChainsawEdge]) {

  override def addEdge(source: TimingVertex, target: TimingVertex) = {
    val e = new ChainsawEdge
    super.addEdge(source, target, e)
    setEdgeWeight(e, 0)
    e
  }

  def addEdgeWithWeight(source: TimingVertex, target: TimingVertex, weight: Double) = {
    val e = addEdge(source, target)
    setEdgeWeight(e, weight)
    e
  }

  def retiming(solution: Map[TimingVertex, Int]): Unit = {
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
    val edgeWeights = mutable.Map[ChainsawEdge, Double]()
    vertexSet()
      .filter(v => outgoingEdgesOf(v).size() > 1)
      .foreach { v =>
        val outgoingEdges = outgoingEdgesOf(v).toSeq
        val k = outgoingEdges.size
        val latencies = outgoingEdges.map(getEdgeWeight)
        val latencyMax = latencies.max
        val targets = outgoingEdges.map(getEdgeTarget)
        val dummy = new DummyVertex(s"${v.name}_dummy")
        addVertex(dummy)
        val dummyEdges = targets.zip(latencies).map { case (target, latency) => addEdgeWithWeight(target, dummy, latencyMax - latency) }
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
    vertexSet().filter(_.isInstanceOf[DummyVertex]).foreach(removeVertex)
    this
  }

  def edgeToString(e: ChainsawEdge) = s"${getEdgeSource(e)} -> ${getEdgeTarget(e)} : ${getEdgeWeight(e)}"

  def pathToString(path: GraphPath[TimingVertex, ChainsawEdge]) =
    path.getVertexList.zip(path.getEdgeList).map { case (vertex, edge) =>
      s"$vertex -> ${getEdgeWeight(edge)} -> "
    }.mkString("") + path.getVertexList.last.toString

  override def toString = {
    val vertices = vertexSet().toSeq.mkString("\n")
    val edges = edgeSet().toSeq.map(edgeToString).mkString("\n")
    s"vertices:\n$vertices\nedges:\n$edges"
  }
}