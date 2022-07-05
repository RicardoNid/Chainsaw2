package org.datenlord
package dfg

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class DagEdge(val inOrder: Int, val outOrder: Int) {

  def source[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]) = ref.getEdgeSource(this)

  def sourcePort[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]) = ref.getEdgeSource(this).out(outOrder)

  def target[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]) = ref.getEdgeTarget(this)

  def targetPort[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]) = ref.getEdgeTarget(this).in(inOrder)

  def weight[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]) = ref.getEdgeWeight(this)

  def toStringInGraph[TSoft, THard <: Data](implicit ref: Dag[TSoft, THard]): String =
    s"$source -> $weight -> $target"


}
