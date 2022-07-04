package org.datenlord
package dfg

import dfg.OpType.OpType

import spinal.core.Data
import scala.collection.JavaConversions._

class DagVertex[TSoft, THard <: Data](
                                       val name: String,
                                       val latency: Int,
                                       val opType: OpType,
                                       val implS: Seq[TSoft] => Seq[TSoft] = (data: Seq[TSoft]) => data,
                                       val implH: Seq[THard] => Seq[THard] = (data: Seq[THard]) => data
                                     ) {
  def apply(portOrder: Int) = DagPort(this, portOrder)

  def sources(implicit ref: Dag[TSoft, THard]) =
    ref.incomingEdgesOf(this)
      .toSeq.sortBy(_.inOrder)
      .map(ref.getEdgeSource)

  def sourcePorts(implicit ref: Dag[TSoft, THard]) =
    ref.incomingEdgesOf(this)
      .toSeq.sortBy(_.inOrder)
      .map(e => DagPort(ref.getEdgeSource(e), e.inOrder))

  def targets(implicit ref: Dag[TSoft, THard]) =
    ref.outgoingEdgesOf(this)
      .toSeq.sortBy(_.outOrder)
      .map(ref.getEdgeTarget)

  def targetPorts(implicit ref: Dag[TSoft, THard]) =
    ref.outgoingEdgesOf(this)
      .toSeq.sortBy(_.outOrder)
      .map(e => DagPort(ref.getEdgeTarget(e), e.outOrder))
}
