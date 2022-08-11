package org.datenlord
package dfg

import spinal.core._
import spinal.lib._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

class RingDag(name: String = "ring", val golden: Seq[BigInt] => Seq[BigInt])
  extends Dag[BigInt, UInt](name) {

  override implicit val ref: RingDag = this

  def addInput(name: String, info: ArithInfo) = {
    val in = RingVarVertex(name, info)
    addVertex(in)
    inputs += in
    in.out(0)
  }

  def addOutput(name: String, info: ArithInfo) = {
    val out = RingVarVertex(name, info)
    addVertex(out)
    outputs += out
    out.in(0)
  }

  def breakBundles() = BreakBundles(this)

  def mergePostAddition() = PostAdditionTreeOpt(this)

  /** get the graph prepared for hardware implementation
   *
   * @return graph after validation
   */
  override def validate() = {
    makeComb()
    simplify()
    breakBundles()
    mergePostAddition()
    simplify()
    autoPipeline()
    this
  }

  /** Add design rules here, invoked before impl
   *
   */
  override def doDrc(): Unit = {
    super.doDrc()
  }

  def toTransform = {

    this.validate()
    this.showCost

    val graphLatency = this.latency
    logger.info(s"graph before impl $this")
    logger.info(s"latency before impl ${graphLatency}")

    def config = new TransformBase {
      override def impl(dataIn: Seq[Any]) = golden.apply(dataIn.asInstanceOf[Seq[BigInt]])

      override val size = (inputs.length, outputs.length)

      override def latency = graphLatency

      override def implH: TransformModule[UInt, UInt] = module(this)
    }

    def module(theConfig: TransformConfig) =
      new TransformModule[UInt, UInt] {
        override val config = theConfig
        val widthsIn = inputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsIn)
        val widthsOut = outputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsOut)
        override val dataIn = slave Flow Fragment(Vec(widthsIn.map(w => UInt(w bits))))
        override val dataOut = master Flow Fragment(Vec(widthsOut.map(w => UInt(w bits))))
        dataOut.fragment := evaluateH(dataIn.fragment)
        autoValid()
        autoLast()
      }

    module(config)
  }

  def toPng(pngName: String = null) = {

    import org.jgrapht.ext.JGraphXAdapter
    import _root_.com.mxgraph.layout._
    import _root_.com.mxgraph.util.mxCellRenderer

    val graphAdapter = new JGraphXAdapter[V, E](this)

    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    layout.setMoveTree(true)
    layout.setEdgeRouting(false)
    layout.setResizeParent(false)
    layout.setLevelDistance(5)
    println(inputs.mkString(" "))

    val vertexMap = graphAdapter.getVertexToCellMap
    val edgeMap = graphAdapter.getEdgeToCellMap

    // set styles of vertices/edges
    def colorVertices(vertices: Seq[V], color: String) = {
      val targets = vertices.map(vertexMap.get(_)).toArray
      graphAdapter.setCellStyle(s"fillColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    def colorEdges(edges: Seq[E], color: String) = {
      val targets = edges.map(edgeMap.get(_)).toArray
      graphAdapter.setCellStyle(s"strokeColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    // find post-addition tree
    import OpType._
    val postTypes = Seq(ADDC, ADD, SUBC, SUB, SHIFT, RESIZE)

    val postVertices = ArrayBuffer[V]()
    val postEdges = ArrayBuffer[E]()

    var currents: Seq[E] = outputs.flatMap(_.incomingEdges)
    while (currents.nonEmpty) {
      val drivingVertices = currents.map(_.source).filter(v => postTypes.contains(v.opType))
      val drivingEdges = drivingVertices.flatMap(_.incomingEdges)
      postEdges ++= drivingEdges
      postVertices ++= drivingVertices
      currents = drivingEdges
    }

    colorVertices(postVertices, "CCCC00")
    colorEdges(postEdges, "CCCC00")

    layout.execute(graphAdapter.getDefaultParent)

    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val fileName = if (pngName == null) {
      name
    } else pngName
    val imgFile = new File(s"/home/ltr/IdeaProjects/Chainsaw2/src/main/resources/dfgGenerated/$fileName.png")
    ImageIO.write(image, "PNG", imgFile)
  }
}

object RingDag {

  def apply(name: String, golden: Seq[BigInt] => Seq[BigInt]): RingDag = new RingDag(name, golden)

}

