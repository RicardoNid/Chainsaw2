package org.datenlord
package dfg

class DspEdge(val schedule: Schedule, val inOrder: Int, val outOrder: Int){
  override def toString = "edge"
}