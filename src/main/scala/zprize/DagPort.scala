package org.datenlord
package zprize

import org.datenlord.Direction
import org.datenlord.zprize.DagVertex

case class DagPort(vertex: DagVertex, order: Int, direction: Direction) {

  // TODO: more utils for connection
  def :=(that: DagPort)(implicit ref: Dag): Unit = ref.addEdge(that, this)

  def relativeTime = direction match {
    case In =>
      if (vertex.gen.inputTimes == null) 0
      else vertex.gen.inputTimes(order)
    case Out =>
      if (vertex.gen.outputTimes == null) 0 + vertex.gen.latency
      else vertex.gen.outputTimes(order) + vertex.gen.latency
  }

}
