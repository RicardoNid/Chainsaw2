package org.datenlord
package dfg

import org.jgrapht.graph._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

class Dfg extends DirectedWeightedPseudograph[DspNode[_], DspEdge](classOf[DspEdge]){

  def period = edgeSet().map(_.schedule)


}
