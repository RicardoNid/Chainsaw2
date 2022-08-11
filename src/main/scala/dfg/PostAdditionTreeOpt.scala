package org.datenlord
package dfg

import org.datenlord.arithmetic.BmcConfig
import spinal.core._

import scala.collection.mutable.ArrayBuffer

object PostAdditionTreeOpt {

  /** in-place rewrite method that extract the post-addition tree and convert it into a compressor(CSA) tree
   *
   */
  def apply(ringDag: RingDag): Unit = {

    implicit val ref: RingDag = ringDag
    if (ringDag.outputs.length != 1) return // when it is not a tree(has no root), skip this phase

    /** --------
     * preparing for tree extraction
     * -------- */
    type V = DagVertex[BigInt, UInt]
    type E = DagEdge
    type Port = DagPort[BigInt, UInt]

    import OpType._
    val postTypes = Seq(ADDC, ADD, SUBC, SUB, SHIFT, RESIZE) // types which are part of post-addition tree

    def isInTree(v: V) = postTypes.contains(v.opType)

    // get shift value of a shift vertex by in/out width
    def extractShift(v: RingVertex) = v.infosOut.head.width - v.infosIn.head.width

    /** --------
     * start tree extraction by backward search
     * -------- */
    // containers for result
    val operands = ArrayBuffer[(E, ArithInfo)]() // operands of addition(leaves of the tree, an edge) and its info
    val postVertices = ArrayBuffer[V]() // vertices involved(tobe removed)
    val postEdges = ArrayBuffer[E]() // edges involved(tobe removed)
    val root = ringDag.outputs.head.asInstanceOf[RingVertex] // output vertex is the root of addition tree

    // maintaining a list of edges for each step, initialized as incoming edges of the root
    var currents: Seq[(E, ArithInfo)] = root.incomingEdges.map(e => (e, ArithInfo(0, 0, sign = true)))

    while (currents.nonEmpty) { // search until no edges can be reached
      // an edge is a leaf when its driver is not part of the tree
      val leaves = currents.filterNot { case (edge, _) => isInTree(edge.source) }
      leaves.foreach { case (edge, info) =>
        postEdges += edge
        operands += ((edge, info)) // leaves should be recorded as operands
      }

      // backward searching, starts from the remaining edges
      // get drivers
      val drivingVertices: Seq[(V, ArithInfo)] = currents.diff(leaves) // remained edges
        .map { case (edge, info) =>
          postVertices += edge.source
          (edge.source, info) // info should be passed through searching
        }

      // pass through the drivers, update infos according to different vertex type
      val drivingEdges = drivingVertices.flatMap { case (v, info) =>
        v.opType match {
          // keep
          case ADD => v.incomingEdges.map(e => (e, info))
          case ADDC => v.incomingEdges.map(e => (e, info))
          case RESIZE => v.incomingEdges.map(e => (e, info))
          // inverse
          case SUBC =>
            val Seq(e0, e1) = v.incomingEdges
            Seq((e0, info), (e1, -info))
          case SUB =>
            val Seq(e0, e1) = v.incomingEdges
            Seq((e0, info), (e1, -info))
          // shift
          case SHIFT => v.incomingEdges.map(e => (e, info << extractShift(v.asInstanceOf[RingVertex])))
        }
      }
      drivingEdges.foreach { case (edge, info) => postEdges += edge }

      currents = drivingEdges // update currents for next step
    }
    val allInfos = operands.map { case (e, info) => ArithInfo(e.sourcePort.asInstanceOf[RingPort].width, info.shift, info.sign) }

    /** --------
     * print infos
     * -------- */
    val posOps = operands.filter(_._2.sign == true)
    val negOps = operands.filter(_._2.sign == false)

    val posInfos = allInfos.filter(_.sign == true)
    val negInfos = allInfos.filter(_.sign == false)

    logger.info(s"number of operands: ${allInfos.length}")
    logger.info(s"number of different shifts: ${allInfos.map(_.shift).distinct.length}")
    allInfos.groupBy(_.shift).toSeq.sortBy(_._1).foreach { case (shift, infos) =>
      logger.info(s"operand at shift $shift: ${infos.map(_.width).mkString(" ")}")
    }
    logger.info(s"total bits for reduction ${allInfos.map(_.width).sum}")
    logger.info(s"positive bits for reduction ${allInfos.map(_.width).sum}")
    logger.info(s"negative bits for reduction ${allInfos.map(_.width).sum}")

    /** --------
     * construct a new tree(s)
     * -------- */
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

    VivadoImpl(BmcConfig(posInfos).implH, "testPostPart")
    VivadoImpl(BmcConfig(negInfos).implH, "testNegPart")

    val resized = ret.resize(root.widthsIn.head)
    logger.info(s"output width ${root.widthsIn.head}")
    ringDag.addEdge(resized, root.in(0))

    /** --------
     * remove the old tree
     * -------- */
    postEdges.foreach(ringDag.removeEdge)
    postVertices.foreach(ringDag.removeVertex)
  }
}
