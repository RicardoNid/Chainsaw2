package org.datenlord
package dfg

import arithmetic.{ArithInfo, BitHeapCompressorUseInversionConfig}

import spinal.core._

import scala.collection.mutable.ArrayBuffer

object RewriteAdditionTree {

  // FIXME: SHOULD WORK WHEN NO EXTRACTION CAN BE MADE
  // FIXME: SHOULD WORK WHEN FULL EXTRACTION CAN BE MADE

  /** in-place rewrite method that extract the addition tree and convert it into a compressor(CSA) tree
   *
   * a addition tree is a multi-operand addition
   */
  def apply(ringDag: RingDag): RingDag = {

    implicit val ref: RingDag = ringDag
    if (ringDag.outputs.length != 1) return ringDag // when it is not a tree(has no root), skip this phase

    /** --------
     * preparing for tree extraction
     * -------- */
    type V = DagVertex[UInt]
    type E = DagEdge
    type Port = DagPort[UInt]

    val postTypes = Seq(BinaryAdder, BinarySubtractor, Shift, Resize) // types which are part of post-addition tree

    def isInTree(v: V) = postTypes.contains(v.opType)

    // get shift value of a shift vertex by in/out width
    def extractShift(v: RingVertex) = v.widthsOut.head - v.widthsIn.head

    /** --------
     * start tree extraction by backward search
     * -------- */
    // containers for result
    val operands = ArrayBuffer[(E, ArithInfo)]() // operands of addition(leaves of the tree, an edge) and its info
    val postVertices = ArrayBuffer[V]() // vertices involved(tobe removed)
    val postEdges = ArrayBuffer[E]() // edges involved(tobe removed)
    val root = ringDag.outputs.head.asInstanceOf[RingVertex] // output vertex is the root of addition tree

    // maintaining a list of edges for each step, initialized as incoming edges of the root
    var currents: Seq[(E, ArithInfo)] = root.incomingEdges.map(e => (e, ArithInfo(0, 0, isPositive = true)))

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
          case BinaryAdder => v.incomingEdges.map(e => (e, info))
          case Resize => v.incomingEdges.map(e => (e, info))
          // inverse
          case BinarySubtractor =>
            val Seq(e0, e1) = v.incomingEdges
            Seq((e0, info), (e1, -info))
          // shift
          case Shift => v.incomingEdges.map(e => (e, info << extractShift(v.asInstanceOf[RingVertex])))
        }
      }
      drivingEdges.foreach { case (edge, info) => postEdges += edge }

      currents = drivingEdges // update currents for next step
    }
    val allInfos = operands.map { case (e, info) => ArithInfo(e.sourcePort.asInstanceOf[RingPort].width, info.weight, info.isPositive) }
    val allOps = operands.map(_._1.sourcePort.asInstanceOf[RingPort])

    val ret = BitHeapCompressorUseInversionConfig(allInfos).asRingOp(allOps).head

    val resized = ret.resize(root.widthsIn.head)
    logger.info(s"output width ${root.widthsIn.head}")
    ringDag.addEdge(resized, root.in(0))

    //    val compress = CompressorVertex("COMPRESS", allInfos)
    //    ringDag.addVertexWithDrivers(compress, operands.map(_._1.sourcePort): _*)
    //    val ret = compress.out(0)
    //    val resized = ret.resize(root.widthsIn.head)
    //    ringDag.addEdge(resized, root.in(0))

    /** --------
     * remove the old tree
     * -------- */
    postEdges.foreach(ringDag.removeEdge)
    postVertices.foreach(ringDag.removeVertex)

    ringDag
  }
}
