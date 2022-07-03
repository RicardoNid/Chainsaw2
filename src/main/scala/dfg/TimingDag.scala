package org.datenlord
package dfg

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import org.jgrapht.Graphs
import org.jgrapht.graph._

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

  // validate the DFG with minimum registers in place
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
    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.name, -20, 20))
    val variableMap = Map(vertices.zip(variables): _*)
    // construct cost
    val coeffs: Seq[Double] = vertices.map { v =>
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
    // set linear programming problem and solve it
    minimize(expr)
    start()
    val solution = variableMap.map(pair => pair._1 -> pair._2.value.get.toInt)
    retiming(solution)
    release()
    // remove dummy vertices
    vertexSet().filter(_.isInstanceOf[DummyVertex]).foreach(removeVertex)
    this
  }

  override def toString = {
    val vertices = vertexSet().toSeq.map(_.name).mkString("\n")
    val edges = edgeSet().toSeq.map(e => s"${getEdgeSource(e).name} -> ${getEdgeTarget(e).name} : ${getEdgeWeight(e)}").mkString("\n")
    s"vertices:\n$vertices\nedges:\n$edges"
  }
}

object TimingDag extends App {
  val testGraph = new TimingDag

  val vertices = (0 until 3).map(i => new TimingVertex(s"add$i", 1))
  val Seq(add0, add1, add2) = vertices
  val in = new TimingVertex("in", 0)
  val out = new TimingVertex("out", 0)

  testGraph.addVertex(in)
  vertices.foreach(testGraph.addVertex)
  testGraph.addVertex(out)

  Graphs.addIncomingEdges(testGraph, add0, Seq(in))
  Graphs.addIncomingEdges(testGraph, add1, Seq(in, add0))
  Graphs.addIncomingEdges(testGraph, add2, Seq(in, add1))
  Graphs.addIncomingEdges(testGraph, out, Seq(add0, add1, add2))

  // example0
  //  testGraph.validate
  //  println(testGraph)
  // example1
  testGraph.incomingEdgesOf(out).foreach(testGraph.setEdgeWeight(_, 10))
  println(testGraph)
  testGraph.validate
  println(testGraph)
}