package org.datenlord
package dfg

import dfg.OpType.OpType
import dfg.Direction._
import spinal.core.Data
import scala.collection.JavaConversions._

class DagVertex[TSoft, THard <: Data](
                                       val name: String,
                                       val latency: Int,
                                       val opType: OpType,
                                       // TODO: remove implS and TSoft
                                       val implS: Seq[TSoft] => Seq[TSoft] = (data: Seq[TSoft]) => data,
                                       val implH: Seq[THard] => Seq[THard] = (data: Seq[THard]) => data
                                     ) {

  //  def apply(portOrder: Int) = DagPort(this, portOrder)

  def in(portOrder: Int) = DagPort(this, portOrder, In)

  def out(portOrder: Int) = DagPort(this, portOrder, Out)

  def inDegree(implicit ref: Dag[TSoft, THard]) = ref.inDegreeOf(this)

  def outDegree(implicit ref: Dag[TSoft, THard]) = ref.outDegreeOf(this)

  def incomingEdges(implicit ref: Dag[TSoft, THard]) = ref.incomingEdgesOf(this)

  def sources(implicit ref: Dag[TSoft, THard]) =
    this.incomingEdges
      .toSeq.sortBy(_.inOrder)
      .map(ref.getEdgeSource)

  def sourcePorts(implicit ref: Dag[TSoft, THard]) =
    this.incomingEdges
      .toSeq.sortBy(_.inOrder)
      .map(e => DagPort(ref.getEdgeSource(e), e.outOrder, Out))

  def outgoingEdges(implicit ref: Dag[TSoft, THard]) = ref.outgoingEdgesOf(this)

  def targets(implicit ref: Dag[TSoft, THard]) =
    this.outgoingEdges
      .toSeq.sortBy(_.outOrder)
      .map(ref.getEdgeTarget)

  def targetPorts(implicit ref: Dag[TSoft, THard]) =
    ref.outgoingEdgesOf(this)
      .toSeq.sortBy(_.outOrder)
      .map(e => DagPort(ref.getEdgeTarget(e), e.inOrder, In))

  def isIo(implicit ref: Dag[TSoft, THard]) = ref.inputs.contains(this) || ref.outputs.contains(this)
}
