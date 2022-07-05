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

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.JavaConversions._
import OpType._
import org.datenlord.dfg.Direction._

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


  override def in(portOrder: Int) = RingPort(this, portOrder, In)

  override def out(portOrder: Int) = RingPort(this, portOrder, Out)

  def doWidthCheck(): Unit = assert(widthCheck(widthsIn), s"$opType vertex: widthsIn = ${widthsIn.mkString(" ")}, widthsOut = ${widthsOut.mkString(" ")}")

  override def toString = s"$name"
}


class RingDag extends Dag[BigInt, UInt]("ring") {

  override implicit val ref: RingDag = this

  def setInput(name: String, width: Int) = {
    val in = RingVarVertex(name, width)
    addVertex(in)
    inputs += in
    in.out(0)
  }

  def setOutput(name: String, width: Int) = {
    val out = RingVarVertex(name, width)
    addVertex(out)
    outputs += out
    out.in(0)
  }

  // break the merge-split construct into pieces
  def breakBundles() = {

    checkHomo()

    val candidates = vertexSet()
      .filter(_.opType == Merge)
      .filter(_.targets.length == 1)
      .filter(_.targets.head.opType == Split)
      .map(merge => (merge, merge.targets.head)) // merge-split
    logger.info(s"${candidates.size} pairs of merge-split found")
    logger.info(s"${vertexSet().size()} vertices before")
    candidates.foreach { case (m, s) =>
      val (merge, split) = (m.asInstanceOf[RingVertex], s.asInstanceOf[RingVertex])

      def getNewSplits(widthsOfMerge: Seq[Int], widthsOfSplit: Seq[Int]) = {

        //        require(widthsOfMerge.length > 1 && widthsOfSplit.length > 1)
        val mergeSplits = widthsOfMerge.reverse.scan(0)(_ + _).init
        val splitSplits = widthsOfSplit.reverse.scan(0)(_ + _).init
        val splitPoints = (mergeSplits ++ splitSplits).sorted.distinct

        //        println(mergeSplits.mkString(" "))
        //        println(splitSplits.mkString(" "))
        //        println(splitPoints.mkString(" "))

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


      ends.zipWithIndex.foreach { case (end, i) =>
        val targetsPorts = split.outgoingEdges.filter(_.outOrder == i).map(_.targetPort)
        targetsPorts.foreach(target => addEdge(end, target))
      }

      //      assert(ends.length == split.targetPorts.length)
      //      ends.zip(split.targetPorts).foreach { case (port, target) => addEdge(port, target) }

      removeVertex(merge)
      removeVertex(split)

    }
    logger.info(s"2 vertices removed")
    logger.info(s"${vertexSet().size()} vertices after")
  }


  /** Do retiming on this DFG, such that constraints for vertices are satisfied with minimum number of registers
   *
   * @return graph after validation
   */
  override def validate(maxLatency: Int = 100) = {
    eliminateIntermediates()
    breakBundles()
    logger.info(s"merge-split removed")
    super.validate(maxLatency)
    this
  }

  def checkWidths(): Unit = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]].foreach(_.doWidthCheck())

  def toTransform(maxLatency: Int = 100) = {

    this.validate(maxLatency)
    logger.info(s"latency before impl ${this.latency}")

    val graphLatency = this.latency

    def config = new TransformBase {
      override def impl(dataIn: Seq[Any]) = evaluateS(dataIn.asInstanceOf[Seq[BigInt]])

      override val size = (inputs.length, outputs.length)

      override def latency = graphLatency

      logger.info(s"latency in config $latency")

      override def implH = module(this)
    }

    def module(theConfig: TransformConfig) =
      new TransformModule[UInt, UInt] {
        override val config = theConfig
        val widthIn = inputs.asInstanceOf[Seq[RingVertex]].head.widthsOut.head
        val widthOut = outputs.asInstanceOf[Seq[RingVertex]].head.widthsIn.head
        override val dataIn = slave Flow Fragment(Vec(UInt(widthIn bits), config.size._1))
        override val dataOut = master Flow Fragment(Vec(UInt(widthOut bits), config.size._2))
        dataOut.fragment := evaluateH(dataIn.fragment)
        autoValid()
        autoLast()
      }

    module(config)
  }
}

object RingDag {

  def apply(): RingDag = new RingDag()

}

