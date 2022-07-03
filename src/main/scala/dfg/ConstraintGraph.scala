package org.datenlord
package dfg

import org.jgrapht._
import org.jgrapht.alg.shortestpath.FloydWarshallShortestPaths

import scala.collection.JavaConversions._

class VariableVertex(name: String) extends TimingVertex(name, 0) {
  def -(that: VariableVertex) = Inequality(this, that, 0)

  override def toString = s"variable $name"
}

case class Inequality(target: VariableVertex, source: VariableVertex, value: Double) {
  def <=(value: Double) = Inequality(target, source, value)
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
  }
}