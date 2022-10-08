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
  extends Dag[UInt](name) {

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

  def breakBundles() = BreakBundles(this)

  def rewritePostAdditionTree(): RingDag = RewriteAdditionTree(this)

  def reconstruct() = {
    makeComb()
    simplify()
    rewritePostAdditionTree()
    simplify().asInstanceOf[RingDag]
  }

  /** get the graph prepared for hardware implementation
   *
   * @return graph after validation
   */
  override def validate() = {
    makeComb()
    simplify()
    //    breakBundles()
    rewritePostAdditionTree()
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
    this.toPng(name)

    val graphLatency = this.latency

    logger.info(
      s"\n----impl report of $name ----" +
        s"\n\tlatency = $graphLatency"
    )

    def config = new TransformDfg {

      override val name = "name"
      // TODO: not this type
      override val opType = Custom
      override val widthsIn = inputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsIn)
      override val widthsOut = outputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsOut)

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
        dataIn.fragment.head
        override val dataOut = master Flow Fragment(Vec(widthsOut.map(w => UInt(w bits))))
        dataOut.fragment := evaluateH(dataIn.fragment)
        autoValid()
        autoLast()
      }

    module(config)
  }

  def asRingOp(dataIns: Seq[RingPort])(implicit dag: RingDag) = {

    this.validate()
    //    this.toPng(name)

    val graphLatency = this.latency
    logger.info(s"latency before impl $graphLatency")
    val graphName = this.name


    def config = new TransformDfg {

      override val name = graphName
      // TODO: not this type
      override val opType = Custom
      override val widthsIn = inputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsIn)
      override val widthsOut = outputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsOut)

      override def impl(dataIn: Seq[Any]) = golden.apply(dataIn.asInstanceOf[Seq[BigInt]])

      override val size = (dataIns.length, outputs.length)

      override def latency = graphLatency

      override def implH: TransformModule[UInt, UInt] = module(this)
    }

    def module(theConfig: TransformConfig) =
      new TransformModule[UInt, UInt] {
        override val config = theConfig
        val widthsIn = inputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsIn)
        val widthsOut = outputs.asInstanceOf[Seq[RingVertex]].flatMap(_.widthsOut)
        override val dataIn = slave Flow Fragment(Vec(widthsIn.map(w => UInt(w bits))))
        dataIn.fragment.head
        override val dataOut = master Flow Fragment(Vec(widthsOut.map(w => UInt(w bits))))
        dataOut.fragment := evaluateH(dataIn.fragment)
        autoValid()
        autoLast()
      }

    config.asRingOp(dataIns)(dag)
  }

  def toPng(pngName: String = "temp") = ToPng(this, pngName)

  override def clone: RingDag = {
    val newGraph = new RingDag(name, golden)
    val vertexMap = this.vertexSet().map(v => v -> v.clone()).toMap
    vertexMap.values.foreach(newGraph.addVertex)
    this.inputs.foreach(v => newGraph.inputs += vertexMap(v))
    this.outputs.foreach(v => newGraph.outputs += vertexMap(v))
    this.edgeSet().foreach { e =>
      val (source, target) = (vertexMap(e.source), vertexMap(e.target))
      val sourceId = e.outOrder
      val targetId = e.inOrder
      newGraph.addEdge(source.out(sourceId), target.in(targetId), e.weight)
    }
    newGraph.retimingInfo = this.retimingInfo.map { case (v, i) => vertexMap(v) -> i }
    newGraph
  }
}

object RingDag {

  def apply(name: String, golden: Seq[BigInt] => Seq[BigInt]): RingDag = new RingDag(name, golden)

}

