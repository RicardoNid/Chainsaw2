package org.datenlord
package dfg

import dfg.Direction._
import dfg.OpType.{OpType, _}

import spinal.core._
import spinal.lib._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/** This is for crypto implementation on FPGAs of Xilinx UltraScale family
 *
 * @param opType     type of operation
 * @param widthCheck check whether the input widths are valid, according to opType
 */
class RingVertex
(
  name: String, latency: Int,
  implS: Seq[BigInt] => Seq[BigInt], implH: Seq[UInt] => Seq[UInt],
  opType: OpType,
  val infosIn: Seq[Int], val infosOut: Seq[Int], val widthCheck: Seq[Int] => Boolean
) extends DagVertex[BigInt, UInt](name, latency, opType, implS, implH) {

  override def in(portOrder: Int) = RingPort(this, portOrder, In)

  override def out(portOrder: Int) = RingPort(this, portOrder, Out)

  def doWidthCheck(): Unit = assert(widthCheck(infosIn), s"$opType vertex: widthsIn = ${infosIn.mkString(" ")}, widthsOut = ${infosOut.mkString(" ")}")

  override def toString = s"$name"
}


class RingDag(name: String = "ring") extends Dag[BigInt, UInt](name) {

  override implicit val ref: RingDag = this

  def addInput(name: String, width: Int) = {
    val in = RingVarVertex(name, width)
    addVertex(in)
    inputs += in
    in.out(0)
  }

  def addOutput(name: String, width: Int) = {
    val out = RingVarVertex(name, width)
    addVertex(out)
    outputs += out
    out.in(0)
  }

  // break the merge-split construct into pieces
  def breakBundles(): Unit = {

    simplified()

    def getCandidates: Seq[(RingVertex, RingVertex)] = vertexSet().toSeq
      .filter(_.opType == Merge)
      .filter(_.targets.length == 1)
      .filter(_.targets.head.opType == Split)
      .map(merge => (merge, merge.targets.head)) // merge-split
      .asInstanceOf[Seq[(RingVertex, RingVertex)]]

    def break(pair: (RingVertex, RingVertex)): Unit = {
      // get split points
      val (merge, split) = pair
      val mergeSplits = merge.infosIn.reverse.scan(0)(_ + _).init
      val splitSplits = split.infosOut.reverse.scan(0)(_ + _).init
      val splitPoints = (mergeSplits ++ splitSplits).sorted.distinct

      // sources of original merge-split
      val starts = merge.sourcePorts.map(RingPort.fromDagPort)

      // split scheme of inputs, low -> high
      val mergeBuffer = Seq.fill(merge.infosIn.length)(ArrayBuffer[Int]())
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
      val splitBuffer = Seq.fill(split.infosOut.length)(ArrayBuffer[Int]())
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
        targetsPorts.foreach(target => addEdge(end, target))
      }

      removeVertex(merge)
      removeVertex(split)
    }

    logger.info("start breaking bundles")
    while (getCandidates.nonEmpty) {
      val candidates = getCandidates
      logger.info(s"\t${candidates.size} pairs of merge-split found")
      logger.info(s"\t${vertexSet().size()} vertices before")
      candidates.foreach(break)
      logger.info(s"\t${vertexSet().size()} vertices after")
    }
    logger.info("end breaking bundles")
  }


  /** Do retiming on this DFG, such that constraints for vertices are satisfied with minimum number of registers
   *
   * @return graph after validation
   */
  override def validate() = {
    breakBundles()
    super.validate()
    this
  }

  def checkWidths(): Unit = vertexSet().toSeq.asInstanceOf[Seq[RingVertex]].foreach(_.doWidthCheck())

  /** Add design rules here, invoked before impl
   *
   */
  override def doDrc(): Unit = {
    checkWidths()
    super.doDrc()
  }

  def toTransform(golden: Seq[BigInt] => Seq[BigInt] = implS) = {

    this.validate()
    this.showCost

    val graphLatency = this.latency
    logger.info(s"latency before impl ${graphLatency}")

    def config = new TransformBase {
      override def impl(dataIn: Seq[Any]) = golden.apply(dataIn.asInstanceOf[Seq[BigInt]])

      override val size = (inputs.length, outputs.length)

      override def latency = graphLatency

      override def implH = module(this)
    }

    def module(theConfig: TransformConfig) =
      new TransformModule[UInt, UInt] {
        override val config = theConfig
        val widthIn = inputs.asInstanceOf[Seq[RingVertex]].head.infosOut.head
        val widthOut = outputs.asInstanceOf[Seq[RingVertex]].head.infosIn.head
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

  def apply(name: String = "ring"): RingDag = new RingDag(name)

}

