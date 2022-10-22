package org.datenlord
package examples

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

import collection.JavaConversions._
import collection.JavaConverters._

object Optimus extends App {

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
}
