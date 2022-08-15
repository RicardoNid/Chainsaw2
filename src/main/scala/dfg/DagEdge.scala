package org.datenlord
package dfg

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class DagEdge(val inOrder: Int, val outOrder: Int) {

  def source[ THard <: Data](implicit ref: Dag[ THard]) = ref.getEdgeSource(this)

  def sourcePort[ THard <: Data](implicit ref: Dag[ THard]) = ref.getEdgeSource(this).out(outOrder)

  def target[ THard <: Data](implicit ref: Dag[ THard]) = ref.getEdgeTarget(this)

  def targetPort[ THard <: Data](implicit ref: Dag[ THard]) = ref.getEdgeTarget(this).in(inOrder)

  def weight[ THard <: Data](implicit ref: Dag[ THard]) = ref.getEdgeWeight(this)

  def toStringInGraph[ THard <: Data](implicit ref: Dag[ THard]): String =
    s"$source -> $weight -> $target"

  override def toString = ""
}
