package org.datenlord
package zprize

import zprize.{ChainsawGenerator, DagPort}

import scala.collection.JavaConversions._ // as JGraphT is based on Java

class DagVertex(val gen: ChainsawGenerator)(implicit ref: Dag) {

  import gen._

  ref.addVertex(this)

  // TODO: without this, all I/O vertices have the same hashCode, but why?
  override def hashCode() = super.hashCode()

  override def equals(obj: Any) = this.hashCode() == obj.hashCode()

  generatorList(gen.hashCode()) += 1
  var vertexName = s"${gen.moduleName}_${generatorList(gen.hashCode())}"

  // generate DagPorts
  def in(portOrder: Int) = { // get one port
    assert(portOrder < inputWidths.length)
    DagPort(this, portOrder, In)
  }

  def out(portOrder: Int) = {
    assert(portOrder < outputWidths.length)
    DagPort(this, portOrder, Out)
  }

  /** --------
   * methods for connections
   * -------- */

  /** --------
   * methods for accessing neighbors
   * -------- */
  def inPorts: Seq[DagPort] = inputWidths.indices.map(in) // get all ports

  def outPorts: Seq[DagPort] = outputWidths.indices.map(out)

  // number of ports
  def inCount = inputWidths.length

  def outCount = outputWidths.length

  def inDegree(implicit ref: Dag) = ref.inDegreeOf(this)

  def outDegree(implicit ref: Dag) = ref.outDegreeOf(this)

  // sources and targets
  def incomingEdges(implicit ref: Dag) =
    ref.incomingEdgesOf(this).toSeq

  def sourcePorts(implicit ref: Dag) =
    this.incomingEdges.sortBy(_.inOrder)
      .map(e => DagPort(ref.getEdgeSource(e), e.outOrder, Out))

  def sources(implicit ref: Dag) =
    this.incomingEdges.sortBy(_.inOrder)
      .map(ref.getEdgeSource)

  def outgoingEdges(implicit ref: Dag) =
    ref.outgoingEdgesOf(this).toSeq

  def targetPorts(implicit ref: Dag) =
    ref.outgoingEdgesOf(this)
      .toSeq.sortBy(_.outOrder)
      .map(e => DagPort(ref.getEdgeTarget(e), e.inOrder, In))

  def targets(implicit ref: Dag) =
    this.outgoingEdges.sortBy(_.outOrder)
      .map(ref.getEdgeTarget)

  // I/O attribute
  def isInput(implicit ref: Dag) = ref.inputs.contains(this)

  def isOutput(implicit ref: Dag) = ref.outputs.contains(this)

  def isIo(implicit ref: Dag) = isInput || isOutput

  def setName(name: String): Unit = vertexName = name

  override def toString = s"$vertexName"

  override def clone(): DagVertex = new DagVertex(gen)
}

object DagVertex {
  def apply(gen: ChainsawGenerator)(implicit ref: Dag): DagVertex = new DagVertex(gen)(ref)
}
