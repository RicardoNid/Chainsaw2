package org.datenlord
package zprize


import ilog.concert.IloNumVar
import ilog.cplex.IloCplex
import spinal.core._

import scala.collection.JavaConversions._
import scala.math.log

object AutoPipeline {
  /** in-place rewrite method that allocate latencies on edges which: 1. guarantee enough latency spaces for vertices 2. has minimum overall latency for graph, solved by linear programming
   *
   * @return dag with a valid solution and weights on edges
   * @see ''Parhi, Keshab K.. “Vlsi Digital Signal Processing Systems: Design And Implementation.” (2007).'' Chapter 4
   */
  def apply(dag: Dag, maximumLatency: Int = 500): Dag = {

    implicit val refDag: Dag = dag

    // pre
    dag.makeComb()

    // declare cplex model
    val cplex = new IloCplex()
    // declare vertices as variables
    val vertices = dag.vertexSet().toSeq.toArray
    val lowerBounds = vertices.map(_ => 0.0)
    val upperBounds = vertices.map(_ => maximumLatency.toDouble)
    val variables: Array[IloNumVar] = cplex.numVarArray(vertices.length, lowerBounds, upperBounds)
    val variableMap = vertices.zip(variables).toMap // vertex -> variable
    val coeffForSub = Array(1.0, -1.0) // coeff for expression a - b

    // setting a goal for optimization
    if (dag.inputTimes == null) { // outputs defined
      val inputVars = dag.inputs.map(variableMap).toArray
      val coeffs = inputVars.map(_ => 1.0)
      val cost = cplex.scalProd(inputVars, coeffs)
      cplex.addMaximize(cost)
    } else if (dag.outputTimes == null) { // inputs defined
      val outputVars = dag.outputs.map(variableMap).toArray
      val coeffs = outputVars.map(_ => 1.0)
      val cost = cplex.scalProd(outputVars, coeffs)
      cplex.addMinimize(cost)
    } else if (dag.outputTimes != null && dag.inputTimes != null) { // both defined
      // define latency
      val io = Array(variableMap(dag.outputs.head), variableMap(dag.inputs.head))
      val cost = cplex.scalProd(io, coeffForSub) // shortest overall latency
      cplex.addMinimize(cost) // goal: minimum latency
    } else throw new IllegalArgumentException("neither inputTimes nor outputTimes is defined")

    // setting constraints for input/output
    if (dag.inputTimes != null) dag.inputs.zip(dag.inputTimes).prevAndNext { case (prev, next) =>
      val prevVar = variableMap(prev._1)
      val nextVar = variableMap(next._1)
      val expr = cplex.scalProd(Array(prevVar, nextVar), coeffForSub)
      cplex.addEq(expr, prev._2 - next._2)
    }

    if (dag.outputTimes != null) dag.outputs.zip(dag.outputTimes).prevAndNext { case (prev, next) =>
      val prevVar = variableMap(prev._1)
      val nextVar = variableMap(next._1)
      val expr = cplex.scalProd(Array(prevVar, nextVar), coeffForSub)
      cplex.addEq(expr, prev._2 - next._2)
    }

    // add feasibility constraints between vertices(modules)
    dag.edgeSet().toSeq.foreach { e =>
      val sourceVar = variableMap(e.source)
      val targetVar = variableMap(e.target)
      // targetVar + targetRelative - (sourceVar + sourceRelative) >= 0
      val expr = cplex.scalProd(Array(sourceVar, targetVar), coeffForSub)
      cplex.addLe(expr, e.targetPort.relativeTime - e.sourcePort.relativeTime)
      if(verbose >= 1) logger.info(s"${e.source} - ${e.target} <= ${e.targetPort.relativeTime} - ${e.sourcePort.relativeTime}")
    }

    // solve the LP problem
    cplex.solve()

    // rounding, toInt is equivalent to ceil, which is not appropriate
    import scala.math.round
    val values = cplex.getValues(variables).map(round).map(_.toInt)
    val minValue = values.min
    val solution = vertices.zip(values.map(_ - minValue)).toMap
    logger.info(
      s"\n----retiming report of ${refDag.name}----" +
        s"\n\tsolution status = ${cplex.getStatus}" +
        s"\n\tsolution latency = ${round(cplex.getObjValue())}"
    )
    cplex.end()

    // retiming by the solution
    dag.retimingInfo = solution
    dag.retiming(solution)
    dag
  }
}

