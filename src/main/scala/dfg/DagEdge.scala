package org.datenlord
package dfg

class DagEdge(val inOrder: Int, val outOrder: Int) {

  def source(implicit ref: Dag[_, _]) = ref.getEdgeSource(this)
  def target(implicit ref: Dag[_, _]) = ref.getEdgeTarget(this)

  def weight(implicit ref: Dag[_, _]) = ref.getEdgeWeight(this)

  def toStringInGraph(implicit ref: Dag[_, _]): String =
    s"$source -> $weight -> $target"


}
