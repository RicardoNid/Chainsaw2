package org.datenlord

import dfg.{RingDag, RingPort, RingVertex}

import spinal.core.{UInt, Vec}

/** a subclass of TransformBase which 1.is a fully-pipelined module 2. provides the information needed to be implemented as a vertex
 *
 */
abstract class TransformDfg extends TransformBase {

  val name: String

  val opType: OperatorType

  val widthsIn, widthsOut: Seq[Int]

  def asFunc: Seq[UInt] => Seq[UInt] = (dataIn: Seq[UInt]) => {
    val core = this.implH.asInstanceOf[TransformModule[UInt, UInt]]

    val mismatch = dataIn.length != widthsIn.length
    if (mismatch) logger.warn(s"port width mismatch on $name operator: expected ${widthsIn.length}, yours ${dataIn.length}")
    val loss = dataIn.zip(widthsIn).exists { case (actual, expected) => actual.getBitsWidth > expected }
    if (loss) logger.warn(s"dangerous truncation on $name operator: (${dataIn.map(_.getBitsWidth).mkString(" ")}) -> (${widthsIn.mkString(" ")})")

    core.dataIn.fragment := Vec(dataIn.zip(widthsIn).map { case (actual, expect) => actual.resize(expect) })
    core.skipControl()
    core.dataOut.fragment.asInstanceOf[Seq[UInt]]
  }

  def asRingVertex: RingVertex = new RingVertex(s"$name+$latency", latency, asFunc, opType, widthsIn, widthsOut)

  def asRingOp(inputs: Seq[RingPort])(implicit dag: RingDag): Seq[RingPort] = {
    val vertex = asRingVertex
    dag.addVertexWithDrivers(vertex, inputs: _*)
    vertex.outPorts
  }

  def apply(inputs: RingPort*)(implicit dag: RingDag): Seq[RingPort] = asRingOp(inputs)

}
