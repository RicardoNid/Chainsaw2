package org.datenlord
package examples

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
