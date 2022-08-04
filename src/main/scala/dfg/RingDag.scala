package org.datenlord
package dfg

import spinal.core._
import spinal.lib._

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

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

  /** get the graph prepared for hardware implementation
   *
   * @return graph after validation
   */
  override def validate() = {
    makeComb()
    simplify()
    breakBundles()
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

  def toPng() = {

    import org.jgrapht.ext.JGraphXAdapter
    import _root_.com.mxgraph.layout._
    import _root_.com.mxgraph.util.mxCellRenderer

    val graphAdapter = new JGraphXAdapter[DagVertex[BigInt, UInt], DagEdge](this)

    val layout = new mxCompactTreeLayout(graphAdapter, false, true)
    layout.setMoveTree(true)
    layout.setEdgeRouting(false)
    layout.setResizeParent(false)
    layout.setLevelDistance(5)
    println(inputs.mkString(" "))

    // set styles of vertices/edges
    val vertexMap = graphAdapter.getVertexToCellMap
    val inputArray = inputs.map(vertexMap.get(_)).toArray
    graphAdapter.setCellStyle("fillColor=#CCCC00", inputArray.asInstanceOf[Array[Object]])

    layout.execute(graphAdapter.getDefaultParent)

    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val imgFile = new File(s"./$name.png")
    ImageIO.write(image, "PNG", imgFile)
  }
}

object RingDag {

  def apply(name: String, golden: Seq[BigInt] => Seq[BigInt]): RingDag = new RingDag(name, golden)

}

