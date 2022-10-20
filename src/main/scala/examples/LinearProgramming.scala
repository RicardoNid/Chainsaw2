package org.datenlord
package examples

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

import collection.JavaConversions._
import collection.JavaConverters._

object LinearProgramming extends App {

  // using optimus
  implicit val model: MPModel = MPModel(SolverLib.oJSolver)

  val variables = (0 until 5).map(i => MPFloatVar(s"r$i", -20, 20))
  val Seq(r1, r2, r3, r4, r5) = variables

  minimize(r2 - r3 - r4 + r5)
  add(r1 - r3 <:= 1)
  add(r1 - r4 <:= 2)
  add(r2 - r1 <:= 1)
  add(r3 - r2 <:= 0)
  add(r3 - r5 <:= 1)
  add(r4 - r2 <:= 0)
  add(r4 - r5 <:= 0)

  add(r1 - r2 <:= 0)
  add(r1 - r3 <:= 0)
  add(r1 - r4 <:= 1)
  add(r1 - r5 <:= 1)

  add(r2 - r3 <:= 1)
  add(r2 - r4 <:= 2)
  add(r2 - r5 <:= 2)

  add(r3 - r1 <:= 0)
  add(r3 - r2 <:= -1)
  add(r3 - r4 <:= 2)

  add(r4 - r1 <:= 0)
  add(r4 - r2 <:= -1)
  add(r4 - r3 <:= 1)

  start()

  println(variables.map(v => s"${v.toString()} -> ${v.value}").mkString(" "))

  release()

  //import ilog.concert.IloException
  //import ilog.concert.IloNumVar
  //import ilog.cplex.IloCplex

  // using cplex
  //  val cplex = new IloCplex()
  //
  //  val lb = Array(0.0,0.0,0.0)
  //  val ub = Array(40.0,Double.MaxValue,Double.MaxValue)
  //  val vars = cplex.numVarArray(3, lb, ub)
  //  val costCoeffs = Array(1,2,3)
  //  cplex.addMaximize(cplex.scalProd(vars, costCoeffs))
  //  val constraintCoeffs0 = Array(-1,1,0)
  //  val constraintCoeffs1 = Array(1,-3,1)
  //  cplex.addLe(cplex.scalProd(vars, constraintCoeffs0), 20)
  //  cplex.addLe(cplex.scalProd(vars, constraintCoeffs1), 30)
  //  cplex.solve()
  //  cplex.output().println("Solution status = " + cplex.getStatus)
  //  cplex.output().println("Solution value = " + cplex.getObjValue())
  //  cplex.getValues(vars).foreach(println)

}
