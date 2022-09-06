package org.datenlord
package dfg

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/** in-place rewrite method that breaks all merge-split constructs into pieces
 *
 */
object BreakBundles {
  def apply(ringDag: RingDag): RingDag = {

    implicit val refDag: RingDag = ringDag

    def getCandidates: Seq[(RingVertex, RingVertex)] = ringDag.vertexSet().toSeq
      .filter(_.opType == Merge)
      .filter(_.targets.length == 1)
      .filter(_.targets.head.opType == Split)
      .map(merge => (merge, merge.targets.head)) // merge-split
      .asInstanceOf[Seq[(RingVertex, RingVertex)]]

    def break(pair: (RingVertex, RingVertex)): Unit = {
      // get split points
      val (merge, split) = pair
      val mergeSplits = merge.widthsIn.reverse.scan(0)(_ + _).init
      val splitSplits = split.widthsOut.reverse.scan(0)(_ + _).init
      val splitPoints = (mergeSplits ++ splitSplits).sorted.distinct

      // sources of original merge-split
      val starts = merge.sourcePorts.map(RingPort.fromDagPort)

      // split scheme of inputs, low -> high
      val mergeBuffer = Seq.fill(merge.widthsIn.length)(ArrayBuffer[Int]())
      splitPoints.foreach { p =>
        val index = mergeSplits.lastIndexWhere(_ <= p) // find last where p >= value
        val value = mergeSplits(index)
        if ((p - value) > 0) mergeBuffer(index) += (p - value)
      }

      val mids = starts.zip(mergeBuffer.reverse.map(_.reverse)).flatMap { case (port, splits) =>
        if (splits.isEmpty) Seq(port)
        else port.split(splits)
      }.reverse

      // merge schemes of outputs, low -> high
      val splitBuffer = Seq.fill(split.widthsOut.length)(ArrayBuffer[Int]())
      splitPoints.zipWithIndex.foreach { case (p, index) =>
        val group = splitSplits.lastIndexWhere(_ <= p) // find last where p >= value
        splitBuffer(group) += index
      }

      val ends = splitBuffer.reverse.map { segmentIndices =>
        val group = segmentIndices.map(mids(_)).reverse
        if (group.length == 1) group.head
        else group.head.merge(group.tail)
      }

      // link ends to original targets
      ends.zipWithIndex.foreach { case (end, i) =>
        val targetsPorts = split.outgoingEdges.filter(_.outOrder == i).map(_.targetPort)
        targetsPorts.foreach(target => ringDag.addEdge(end, target))
      }

      ringDag.removeVertex(merge)
      ringDag.removeVertex(split)
    }

    logger.info("start breaking bundles")
    while (getCandidates.nonEmpty) {
      val candidates = getCandidates
      logger.info(s"\t${candidates.size} pairs of merge-split found")
      logger.info(s"\t${ringDag.vertexSet().size()} vertices before")
      candidates.foreach(break)
      logger.info(s"\t${ringDag.vertexSet().size()} vertices after")
    }
    logger.info("end breaking bundles")

    ringDag
  }
}
