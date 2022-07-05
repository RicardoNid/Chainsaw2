package org.datenlord
package dfg

import arithmetic.MultplierMode._

import org.datenlord.dfg.OpType.OpType
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import OpType._

/** This is for crypto implementation on FPGAs of Ultrascale family, each child class of RingVertex has a corresponding hardware implementation on Ultrascale device
 *
 * @param opType     type of operation
 * @param widthCheck check whether the input widths are valid, according to opType
 */
class RingVertex
(
  name: String, latency: Int,
  implS: Seq[BigInt] => Seq[BigInt], implH: Seq[UInt] => Seq[UInt],
  opType: OpType,
  val widthsIn: Seq[Int], val widthsOut: Seq[Int], val widthCheck: Seq[Int] => Boolean
) extends DagVertex[BigInt, UInt](name, latency, opType, implS, implH) {

  def doWidthCheck(): Unit = assert(widthCheck(widthsIn), s"$opType vertex: widthsIn = ${widthsIn.mkString(" ")}, widthsOut = ${widthsOut.mkString(" ")}")

  override def toString = s"$name"
}


class RingDag extends Dag[BigInt, UInt]("ring") {

  override implicit val ref: RingDag = this

  def setInput(name: String, width: Int) = {
    val in = RingVarVertex(name, width)
    addVertex(in)
    inputs += in
    RingPort(in, 0)
  }

  def setOutput(name: String, width: Int) = {
    val out = RingVarVertex(name, width)
    addVertex(out)
    outputs += out
    RingPort(out, 0)
  }

  // break the merge-split construct into pieces
  def breakBundles() = {
    eliminateIntermediates()
    val candidates = vertexSet()
      .filter(_.opType == Merge)
      .filter(_.targets.length == 1)
      .filter(_.targets.head.opType == Split)
      .map(merge => (merge, merge.targets.head)) // merge-split
    candidates.foreach { case (m, s) =>
      val (merge, split) = (m.asInstanceOf[RingVertex], s.asInstanceOf[RingVertex])

      def getNewSplits(widthsOfMerge: Seq[Int], widthsOfSplit: Seq[Int]) = {

        val mergeSplits = widthsOfMerge.reverse.scan(0)(_ + _).init
        val splitSplits = widthsOfSplit.reverse.scan(0)(_ + _).init
        val splitPoints = (mergeSplits ++ splitSplits).sorted.distinct

        // split schemes of inputs, low -> high
        val mergeBuffer = Seq.fill(widthsOfMerge.length)(ArrayBuffer[Int]())
        splitPoints.foreach { p =>
          val index = mergeSplits.lastIndexWhere(_ <= p) // find last where p >= value
          val value = mergeSplits(index)
          if ((p - value) > 0) mergeBuffer(index) += (p - value)
        }

        // merge schemes of outputs
        val splitBuffer = Seq.fill(widthsOfSplit.length)(ArrayBuffer[Int]())
        splitPoints.zipWithIndex.foreach { case (p, index) =>
          val group = splitSplits.lastIndexWhere(_ <= p) // find last where p >= value
          splitBuffer(group) += index
        }

        (mergeBuffer.reverse.map(_.reverse), splitBuffer.reverse)
      }

      val (mergeBuffer, splitBuffer) = getNewSplits(merge.widthsIn, split.widthsOut)

      val starts = merge.sourcePorts.map(RingPort.fromDagPort)

      val mids = starts.zip(mergeBuffer).flatMap { case (port, splits) =>
        if (splits.isEmpty) Seq(port)
        else port.split(splits)
      }.reverse

      val ends = splitBuffer.map { segmentIndices =>
        val group = segmentIndices.map(mids(_)).reverse
        if (group.length == 1) group.head
        else group.head.merge(group.tail)
      }

      ends.zip(split.targetPorts).foreach { case (port, target) => addEdge(port, target) }

      removeVertex(merge)
      removeVertex(split)
    }
  }


  /** Do retiming on this DFG, such that constraints for vertices are satisfied with minimum number of registers
   *
   * @return graph after validation
   */
  override def validate() = {
    breakBundles()
    super.validate()
  }

  def checkWidths(): Unit = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]].foreach(_.doWidthCheck)
}

object RingDag {

  def apply(): RingDag = new RingDag()

}

