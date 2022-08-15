package org.datenlord
package dfg

import dfg.OpType._

import spinal.core.{Data, assert}

import scala.collection.JavaConversions._

object Simplify {
  /** simplify the graph bt removing redundant vertices and edges
   */
  def apply[ THard <: Data](dag: Dag[ THard]) = {

    implicit val refDag: Dag[ THard] = dag

    // find and remove inters which are connectors without actual functionality
    val inters = dag.vertexSet().toSeq
      .filter(_.opType == Var)
      .filterNot(_.isIo)
      .filter(v => v.inDegree == 1 && v.outDegree == 1)
    inters.foreach { inter => // remove one by one
      val source = inter.sourcePorts.head
      val target = inter.targetPorts.head
      val weight = inter.incomingEdges.head.weight + inter.outgoingEdges.head.weight
      dag.addEdge(source, target, weight)
      assert(dag.removeVertex(inter))
    }
    logger.info(s"${inters.length} redundant vertices eliminated")
    // TODO: more strategies for simplification
    dag
  }
}
