package org.datenlord
package dfg

import org.jgrapht.graph._

import scala.collection.JavaConversions._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


class Dfg extends DirectedWeightedPseudograph[DspNode[_], DspEdge](classOf[DspEdge]){

  def period = edgeSet().map(_.schedule)




}
