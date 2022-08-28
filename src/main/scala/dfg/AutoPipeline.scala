package org.datenlord
package dfg

import ilog.concert.IloNumVar
import ilog.cplex.IloCplex
import spinal.core._

import scala.collection.JavaConversions._
import scala.math.log

object AutoPipeline {
  /** in-place rewrite method that allocate latencies on edges which: 1. guarantee enough latency spaces for vertices 2. has minimum overall latency for graph, solved by linear programming
   *
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def apply[ THard <: Data](dag: Dag[ THard]): Dag[ THard] = {

    implicit val refDag: Dag[ THard] = dag
    // TODO: find out why the cost function for minimum register number built by fan out theorem(from VLSI DSP) always failed in cplex(and sometimes failed in optimus)
    // TODO: will it be better if we use rational number?
    // declare model
    val cplex = new IloCplex()
    // declare variables
    val vertices = dag.vertexSet().toSeq.toArray
    val lowerBounds = vertices.map(_ => 0.0)
    val upperBounds = vertices.map(_ => 200.0)

    val variables: Array[IloNumVar] = cplex.numVarArray(vertices.length, lowerBounds, upperBounds)
    val variableMap = vertices.zip(variables).toMap
    // construct cost function for minimum number of registers
    val coeffForSub = Array(1.0, -1.0)
    val io = Array(variableMap(dag.outputs.head), variableMap(dag.inputs.head))
    val cost = cplex.scalProd(io, coeffForSub) // shortest overall latency
    cplex.addMinimize(cost)
    // add feasibility constraints
    dag.edgeSet().toSeq.foreach { e =>
      val source = variableMap(e.source)
      val target = variableMap(e.target)
      val expr = cplex.scalProd(Array(source, target), coeffForSub)
      cplex.addLe(expr, dag.getEdgeWeight(e) - e.source.latency)
      //      logger.info(s"add constraint: ${e.source} - ${e.target} < ${getEdgeWeight(e) - e.source.latency}")
    }

    dag.inputs.init.zip(dag.inputs.tail).foreach { case (a, b) =>
      val inputA = variableMap(a)
      val inputB = variableMap(b)
      val expr = cplex.scalProd(Array(inputA, inputB), coeffForSub)
      cplex.addEq(expr, 0)
    }

    // set linear programming problem and solve it
    cplex.solve()
    import scala.math.round
    val values = cplex.getValues(variables).map(round).map(_.toInt)
    val minValue = values.min
    val solution = vertices.zip(values.map(_ - minValue)).toMap
    logger.info(s"solution: ${solution.mkString("\n")}")
    logger.info(s"solution status: ${cplex.getStatus}")
    logger.info(s"solution latency = ${round(cplex.getObjValue())}")
    cplex.end()
    // retiming by the solution
    dag.retiming(solution)
    dag
  }
}

//  old version implemented by optimus ojSolver, constraint by shortest path for shortest latency
//  def validateByBound = {
//    makeHomo()
//    // declare model
//    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
//    // declare variables, set bound for variables
//    val vertices = vertexSet().toSeq
//    val targetLatency = latencyLowerBound
//    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.toString, 0, targetLatency))
//    val variableMap = Map(vertices.zip(variables): _*)
//    // add feasibility constraints
//    val constraints = edgeSet().toSeq.map { e =>
//      val source = getEdgeSource(e)
//      val target = getEdgeTarget(e)
//      variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
//    }
//    subjectTo(constraints: _*)
//    // set linear programming problem and solve it
//    start()
//    import scala.math.round
//    val solution = variableMap.map { pair =>
//      val value = pair._2.value.get
//      if ((value - round(value)).abs > 0.1) logger.warn(s"bad solution $value")
//      pair._1 -> round(value).toInt
//    }
//    retiming(solution)
//    release()
//    this
//  }

//  old version implemented by optimus ojSolver & cost function from VLSI DSP for minimum registers
//  def validate() = {
//
//    makeHomo()
//    // build graph with dummy vertices, and record weights of edges
//    val originalEdges = edgeSet().toSeq
//    val edgeWeights = mutable.Map[E, Double]()
//    val ks = ArrayBuffer[Int]()
//    vertexSet().toSeq
//      .filter(v => outgoingEdgesOf(v).size() > 1)
//      .foreach { v =>
//        val outgoingEdges = outgoingEdgesOf(v).toSeq
//        val k = outgoingEdges.size
//        ks += k
//        val latencies = outgoingEdges.map(getEdgeWeight)
//        val latencyMax = latencies.max
//        val targets = outgoingEdges.map(getEdgeTarget)
//        val dummy = VarVertex[ THard](s"${v.name}_dummy")
//        addVertex(dummy)
//        val dummyEdges = targets.zip(latencies).map { case (target, latency) => addEdge(target.out(0), dummy.in(0), latencyMax - latency) }
//        (outgoingEdges ++ dummyEdges)
//          .foreach(e => edgeWeights += (e -> 1 / k.toDouble))
//      }
//    // declare model
//    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
//    // declare variables
//    val vertices = vertexSet().toSeq
//    //    val targetLatency = latencyLowerBound
//    val variables: Seq[MPFloatVar] = vertices.map(v => MPFloatVar(v.toString, 0, 50))
//    val variableMap = Map(vertices.zip(variables): _*)
//    // construct cost function for minimum number of registers
//    val coeffs: Seq[Double] = vertices.map { v =>
//      incomingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum -
//        outgoingEdgesOf(v).map(e => edgeWeights.getOrElse(e, 1.0)).sum
//    }
//    val cost = variables.zip(coeffs)
//      .map { case (variable, coeff) => variable * coeff }
//      .reduce(_ + _)
//    logger.info(s"optimus coeffs ${coeffs.mkString(" ")}")
//    logger.info(s"optimus expr $cost")
//    minimize(cost)
//    // add feasibility constraints
//    val constraints = originalEdges.map { e =>
//      val source = getEdgeSource(e)
//      val target = getEdgeTarget(e)
//      val expr = variableMap(source) - variableMap(target) <:= getEdgeWeight(e) - source.latency
//      logger.info(s"$source - $target < ${getEdgeWeight(e) - e.source.latency}")
//      expr
//    }
//    subjectTo(constraints: _*)
//    // set linear programming problem and solve it
//    start()
//    import scala.math.round
//    val solution = variableMap.map { pair =>
//      val value = pair._2.value.get
//      if ((value - round(value)).abs > 0.1) logger.warn(s"bad solution $value")
//      pair._1 -> round(value).toInt
//    }
//    retiming(solution)
//    release()
//    //     remove dummy vertices
//    vertexSet().filter(_.name.endsWith("dummy")).foreach(removeVertex)
//    this
//  }