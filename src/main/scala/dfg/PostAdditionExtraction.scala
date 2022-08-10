package org.datenlord
package dfg

import spinal.core._

import scala.collection.mutable.ArrayBuffer

object PostAdditionExtraction {

  /** in-place rewrite method that extract the post-addition tree and convert it into a compressor(CSA) tree
   *
   */
  def apply(ringDag: RingDag) = {

    implicit val ref: RingDag = ringDag
    require(ringDag.outputs.length == 1)
    val output = ringDag.outputs.head.asInstanceOf[RingVertex]

    // FIXME: shouldn't start from resize
    import OpType._
    val postTypes = Seq(ADDC, ADD, SUBC, SUB, SHIFT, RESIZE)

    type V = DagVertex[BigInt, UInt]
    type E = DagEdge
    type Port = DagPort[BigInt, UInt]

    case class Info(sign: Boolean, shift: Int) {
      def <<(shiftLeft: Int) = Info(sign, shift + shiftLeft)

      def unary_- = Info(!sign, shift)
    }

    val operands = ArrayBuffer[(E, Info)]()

    def extractShift(v: RingVertex) = v.infosOut.head.width - v.infosIn.head.width

    val postVertices = ArrayBuffer[V]()
    val postEdges = ArrayBuffer[E]()

    var currents: Seq[(E, Info)] =
      output.incomingEdges.map(e => (e, Info(true, 0)))

    while (currents.nonEmpty) {
      val starters = currents
        .filter(pair => !postTypes.contains(pair._1.source.opType))

      operands ++= starters
      postEdges ++= starters.map(_._1)

      val drivingVertices: Seq[(V, Info)] = currents.diff(starters)
        .map(pair => (pair._1.source, pair._2))
      postVertices ++= drivingVertices.map(_._1)

      val drivingEdges = drivingVertices.flatMap { case (v, info) =>
        v.opType match {
          case ADD => v.incomingEdges.map(e => (e, info))
          case ADDC => v.incomingEdges.map(e => (e, info))
          case SUBC =>
            val Seq(e0, e1) = v.incomingEdges
            Seq((e0, info), (e1, -info))
          case SUB =>
            val Seq(e0, e1) = v.incomingEdges
            Seq((e0, info), (e1, -info))
          case RESIZE => v.incomingEdges.map(e => (e, info))
          case SHIFT => v.incomingEdges.map(e => (e, info << extractShift(v.asInstanceOf[RingVertex])))
        }
      }
      currents = drivingEdges
      postEdges ++= drivingEdges.map(_._1)
    }

    logger.info(s"total: ${operands.length}")
    operands.foreach(op => logger.info(op._2.toString))

    val posOps = operands.filter(_._2.sign == true)
    val negOps = operands.filter(_._2.sign == false)

    val posInfos = posOps.map { case (e, info) =>
      ArithInfo(e.source.asInstanceOf[RingVertex].infosOut(e.outOrder).width, info.shift)
    }

    val negInfos = negOps.map { case (e, info) =>
      ArithInfo(e.source.asInstanceOf[RingVertex].infosOut(e.outOrder).width, info.shift)
    }

    logger.info(s"pos infos ${posInfos.mkString(" ")}, ${posInfos.length} in total")
    logger.info(s"neg infos ${negInfos.mkString(" ")}")

    // do rewrite
    val compress0 = CompressorVertex("COMPRESS+", posInfos)
    ringDag.addVertexWithDrivers(compress0, posOps.map(_._1.sourcePort): _*)

    val ret = if (negInfos.isEmpty) {
      val wo0 = compress0.widthsOut.head
      val cpa0 = compress0.out(0).resize(wo0) +:+^ compress0.out(1).resize(wo0)
      cpa0
    } else {
      val wo0 = compress0.widthsOut.head
      val cpa0 = compress0.out(0).resize(wo0) +:+^ compress0.out(1).resize(wo0)

      val compress1 = CompressorVertex("COMPRESS-", negInfos)
      ringDag.addVertexWithDrivers(compress1, negOps.map(_._1.sourcePort): _*)
      val wo1 = compress1.widthsOut.head
      val cpa1 = compress1.out(0).resize(wo1) +:+^ compress1.out(1).resize(wo1) // pass
      cpa0 -:- cpa1
    }

    val resized = ret.resize(output.widthsIn.head)
    logger.info(s"output width ${output.widthsIn.head}")
    ringDag.addEdge(resized, output.in(0))

    postEdges.foreach(ringDag.removeEdge)
    postVertices.foreach(ringDag.removeVertex)

  }

}
