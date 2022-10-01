package org.datenlord
package algos

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._
import org.jgrapht.alg.cycle._


import scala.collection.JavaConversions._

class LdpcGen {

}

case class CompactTannerGraph() extends SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge]) {
  val detector = new CycleDetector(this).findCycles()

}

case class ShiftMatrix(matrix: Array[Array[Int]]) {

  def getCompactTannerGraph = {

  }

}
