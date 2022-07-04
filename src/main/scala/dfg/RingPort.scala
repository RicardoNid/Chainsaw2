package org.datenlord
package dfg

import spinal.core.UInt

/** Consisting of a vertex and its output order, RingPort can be used as a variable in datapath, this class makes the description of RingDag more natural
 *
 * @param order output order of the vertex, the specific output variable is specified by this parameter
 */
case class RingPort(override val vertex: RingVertex, override val order: Int)
  extends DagPort[RingInt, UInt](vertex, order) {

  def width = vertex.widthsOut(order)

  def split(splitPoints: Seq[Int])(implicit dag: RingDag) = {
    val splitVertex = SplitVertex(s"${vertex.name}_split", width, splitPoints)
    dag.addVertex(splitVertex)
    dag.addEdge(this, RingPort(splitVertex, 0))
    (0 until splitPoints.length + 1).map(RingPort(splitVertex, _))
  }

  def +^(that: RingPort, carry: RingPort = null)(implicit dag: RingDag): (RingPort, RingPort) = {
    val widthsIn = if (carry != null) Seq(width, that.width, 1) else Seq(width, that.width)
    val addVertex = AddVertex(s"$this+$that", widthsIn)
    dag.addVertex(addVertex)
    dag.addEdge(this, RingPort(addVertex, 0))
    dag.addEdge(that, RingPort(addVertex, 1))
    if (carry != null) dag.addEdge(carry, RingPort(addVertex, 2))
    (RingPort(addVertex, 0), RingPort(addVertex, 1))
  }

  def merge(tail: Seq[RingPort])(implicit dag: RingDag) = {
    val all = this +: tail
    val mergeVertex = MergeVertex(s"${vertex.name}_merge", all.map(_.width))
    dag.addVertex(mergeVertex)
    all.zipWithIndex.foreach { case (port, i) => dag.addEdge(port, RingPort(mergeVertex, i)) }
    RingPort(mergeVertex, 0)
  }

  override def toString = s"${vertex}_$order"
}
