package org.datenlord
package dfg

import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable

/** this is the algorithm used to build hardware from dags
 *
 */
object ImplH {

  def apply[THard <: Data](dag: Dag[THard]): Seq[THard] => Seq[THard] = (dataIns: Seq[THard]) => {

    // declare "background graph"
    import dag._
    implicit val ref: Dag[THard] = dag

    doDrc()

    // containers
    val signalMap = mutable.Map[V, Seq[THard]]() // vertex with its output ports

    def implemented: Seq[V] = signalMap.keys.toSeq // vertices already implemented

    def remained: Seq[V] = vertexSet().toSeq.diff(implemented) // vertices not implemented yet

    def nextStage: Seq[V] = remained.filter(v => v.sources.forall(implemented.contains(_))) // vertices ready to be implemented

    // method to implement a single vertex
    def implVertex(target: V): Unit = {
      val incomingEdges = incomingEdgesOf(target).toSeq.sortBy(_.inOrder) // ordered incoming edges
      val dataIns = incomingEdges.map { e => // get the signals
        val signal = signalMap(getEdgeSource(e))(e.outOrder)
        signal.d(getEdgeWeight(e).toInt - getEdgeSource(e).latency) // latency of the vertex(module) itself must be excluded
      }
      val rets = target.implH(dataIns)
      signalMap += target -> rets
    }

    // create input ports(initialize signalMap for input vertices)
    inputs.zip(dataIns).foreach { case (input, bits) => signalMap += input -> Seq(bits) }

    // implement vertices step by step until no available vertices exist
    while (nextStage.nonEmpty) nextStage.foreach(implVertex)

    if (remained.nonEmpty) logger.warn(s"isolated nodes exist:\n${remained.mkString(" ")}")

    outputs.flatMap(signalMap(_))
  }

}
