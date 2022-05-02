package org.datenlord
package dfg

object example {

  import spinal.core._

  class Add() extends DspNode[UInt]{
    override val exeTime = 0.5
    override val delay = 0
    override val period = 1
    override val impl = (dataIn: Array[UInt]) => Array(dataIn(0) + dataIn(1))

    override def toString = "add"
  }

  class Mult() extends DspNode[UInt]{
    override val exeTime = 0.5
    override val delay = 0
    override val period = 1
    override val impl = (dataIn: Array[UInt]) => Array(dataIn(0) * dataIn(1))

    override def toString = "mult"
  }

  def main(args: Array[String]): Unit = {

    val dfg = new Dfg()

    val a0, a1 = new Add()
    val m0, m1, m2 = new Mult()
    val e0, e1, e2, e3 = new DspEdge(defaultSchedule, 0, 0)

    dfg.addVertex(m0)
    dfg.addVertex(m1)
    dfg.addVertex(m2)
    dfg.addVertex(a0)
    dfg.addVertex(a1)

    dfg.addEdge(m0, a0, e0)
    dfg.addEdge(a0, a1, e1)
    dfg.addEdge(m1, a0, e2)
    dfg.addEdge(m2, a1, e3)

    println(dfg)
  }



}
