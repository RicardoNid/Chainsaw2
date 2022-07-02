package org.datenlord
package dfg

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht._
import org.jgrapht.alg.shortestpath.FloydWarshallShortestPaths
import org.jgrapht.graph._

import scala.collection.JavaConversions._
import scala.collection.mutable

class ChainsawVertex(val name: String)

class TimingVertex(val latency: Int, name: String) extends ChainsawVertex(name)

class DummyVertex(name: String) extends TimingVertex(0, name)

class VariableVertex(name: String) extends TimingVertex(0, name) {
  def -(that: VariableVertex) = Inequality(this, that, 0)

  override def toString = s"variable $name"
}

case class Inequality(target: VariableVertex, source: VariableVertex, value: Double) {
  def <=(value: Double) = Inequality(target, source, value)
}

class ChainsawEdge

class TimingDag extends SimpleDirectedWeightedGraph[TimingVertex, ChainsawEdge](classOf[ChainsawEdge]) {

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

  // validate the DFG with minimum registers
  def validate = {
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
    val variables = vertices.map(v => MPFloatVar(v.name, -20, 20))
    val variableMap = Map(vertices.zip(variables): _*)
    // construct cost
    val coeffs = vertices.map { v =>
      incomingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum -
        outgoingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum
    }
    val expr = variables.zip(coeffs)
      .map { case (variable, coeff) => variable * coeff }
      .reduce(_ + _)
    //    println(expr)
    // add feasibility constraints
    originalEdges.foreach { e =>
      val source = getEdgeSource(e)
      val target = getEdgeTarget(e)
      val inequality = variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
      //      println(s"$inequality")
      add(inequality)
    }
    minimize(expr)
    start()

    val solution = variableMap.map(pair => pair._1 -> pair._2.value.get.toInt)
    retiming(solution)

    release()

    vertexSet().filter(_.isInstanceOf[DummyVertex]).foreach(removeVertex)
  }

  override def toString = {
    val vertices = vertexSet().map(_.name).mkString(", ")
    val edges = edgeSet().map(e => s"${getEdgeSource(e).name} -> ${getEdgeTarget(e).name} : ${getEdgeWeight(e)}").mkString(" , ")
    s"vertices: $vertices\nedges: $edges"
  }
}

class ConstraintGraph extends TimingDag {

  val referenceVertex = new VariableVertex("reference")
  addVertex(referenceVertex)

  def addVariable(v: VariableVertex): Unit = {
    if (!vertexSet().contains(v)) {
      addVertex(v)
      addEdgeWithWeight(referenceVertex, v, 0)
    }
  }

  def addConstraint(target: VariableVertex, source: VariableVertex, value: Double): Unit = {
    addVariable(target)
    addVariable(source)
    addEdgeWithWeight(source, target, value)
  }

  def addInequality(inequality: Inequality): Unit =
    addConstraint(inequality.target, inequality.source, inequality.value)

  def getSolution = {
    val algo = new FloydWarshallShortestPaths(this)
    val paths = algo.getPaths(referenceVertex)
    vertexSet().toSeq.map(v => v -> paths.getWeight(v))
  }
}

object ConstraintGraph {
  def main(args: Array[String]): Unit = {
    //    val Seq(r1, r2, r3, r4) = (0 until 4).map(i => new VariableVertex(s"r$i"))
    //    val cg = new ConstraintGraph
    //    cg.addInequality(r1 - r2 <= 0)
    //    cg.addInequality(r3 - r1 <= 5)
    //    cg.addInequality(r4 - r1 <= 4)
    //    cg.addInequality(r4 - r3 <= -1)
    //    cg.addInequality(r3 - r2 <= 2)
    //    println(cg.getSolution.mkString("\n"))

    val testGraph = new TimingDag

    val vertices = (0 until 3).map(i => new TimingVertex(1, s"add$i"))
    val Seq(add0, add1, add2) = vertices
    val in = new TimingVertex(0, "in")
    val out = new TimingVertex(0, "out")

    testGraph.addVertex(in)
    vertices.foreach(testGraph.addVertex)
    testGraph.addVertex(out)

    Graphs.addIncomingEdges(testGraph, add0, Seq(in))
    Graphs.addIncomingEdges(testGraph, add1, Seq(in, add0))
    Graphs.addIncomingEdges(testGraph, add2, Seq(in, add1))
    Graphs.addIncomingEdges(testGraph, out, Seq(add0, add1, add2))

    println(testGraph)
    // example0
    //    testGraph.validate
    //    println(testGraph)
    // example1
    testGraph.outgoingEdgesOf(out).foreach(testGraph.setEdgeWeight(_, 10))
    testGraph.validate
    println(testGraph)

  }
}
