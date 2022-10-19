package org.datenlord
package zprize

import org.jgrapht._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random // as JGraphT is based on Java

case class Karatsuba(width: Int) extends Dag {

  override def name = s"karatsuba_$width"

  override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  val baseWidth = 17

  /** --------
   * primitives used in Karatusba
   * -------- */
  implicit class karatsubaUtil(port: DagPort) {
    def split2 = {
      val s = Split(port.width, port.width / 2).asVertex
      s := port
      (s.out(0), s.out(1))
    }

    def +^(that: DagPort) = {
      val add = Cpa(BinaryAdder, Seq(that.width max port.width), M2M).asVertex
      add := (port, that)
      add.out(0)
    }

    def +(that: DagPort) = {
      val add = Cpa(BinaryAdder, Seq(that.width max port.width), M2M, withCarry = false).asVertex
      add := (port, that)
      add.out(0)
    }

    def -(that: DagPort) = {
      val add = Cpa(BinarySubtractor, Seq(that.width max port.width), M2M).asVertex
      add := (port, that)
      add.out(0)
    }

    def *(that: DagPort) = {
      require(port.width <= baseWidth && that.width <= baseWidth)
      val mult = Multiplier(Seq(port.width, that.width)).asVertex
      mult := (port, that)
      mult.out(0)
    }

    def <<(shift: Int) = {
      val shiftLeft = ShiftLeft(shift, port.width).asVertex
      shiftLeft := port
      shiftLeft.out(0)
    }

    def resize(widthOut: Int) = {
      val re = Resize(port.width, widthOut).asVertex
      re := port
      re.out(0)
    }
  }

  def merge(operands: Seq[DagPort], operandInfos: Seq[OperandInfo]) = {
    val merge = CompressorTree(operandInfos).asVertex
    merge := (operands: _*)
    merge.out(0)
  }

  /** --------
   * full-DSP solution
   * -------- */


  /** --------
   * solution tradeoff
   * -------- */

  /** --------
   * graph construction
   * -------- */
  val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(width)))
  val o = OutputVertex(UIntInfo(width * 2))

  val productsBuffer = ArrayBuffer[Port]()

  def buildRecursively(a: Port, b: Port): Port = {
    require(a.width == b.width)
    if (a.width <= 17 && b.width <= 17) {
      val product = a * b
      productsBuffer += product
      product
    }
    else {
      val splitWidth = a.width / 2
      val (aH, aL) = a.split2
      val (bH, bL) = b.split2
      val aMerge = aH +^ aL
      val bMerge = bH +^ bL

      val high = buildRecursively(aH, bH)
      val low = buildRecursively(aL, bL)
      val all = buildRecursively(aMerge, bMerge)

      val cross = all - high - low
      val ret = (high << splitWidth * 2) + (cross << splitWidth) + low
      ret.resize(a.width * 2)
    }
  }

  o := buildRecursively(a, b)
  logger.info("latency before rewriting")
  AutoPipeline(this)

  /** --------
   * partial retiming
   * -------- */
  val products = productsBuffer.toSeq // results from part2(multipliers)
  val targets = Some(products.map(_.target))
  AutoPipeline(this, targets = targets)

  /** --------
   * post-addition rewriting
   * -------- */
  val pathFinder = new alg.shortestpath.AllDirectedPaths(this) // path finder from JGraphT
  val operandsAndInfos = ArrayBuffer[(Port, OperandInfo)]() // operands and their infos tobe added
  val redundantVertices = mutable.Set[DagVertex]() // vertices tobe removed

  val retimingBase = products.map(_.target).map(retimingInfo).min
  products.foreach { product =>
    // TODO: will maxPathLength be a trouble maker?
    val allPaths = pathFinder.getAllPaths(product.vertex, o.vertex, false, 30)
    allPaths.foreach { path =>
      var operandInfo = OperandInfo(product.width, 0, positive = true, retimingInfo(product.target) - retimingBase)
      path.getEdgeList.init.foreach { e =>
        redundantVertices += e.target
        e.target.gen match {
          case cpa: Cpa => if (cpa.adderType == BinarySubtractor && e.inOrder == 1) operandInfo = -operandInfo // else, keep
          case shiftLeft: ShiftLeft => operandInfo = operandInfo << shiftLeft.shift
          case resize: Resize => // do nothing
        }
      }
      operandsAndInfos += Tuple2(product, operandInfo)
    }
  }

  redundantVertices.foreach(removeVertex)
  o := merge(operandsAndInfos.map(_._1), operandsAndInfos.map(_._2)).resize(width * 2)

  /** --------
   * full retiming
   * -------- */

  graphDone()
}

object TestKara extends App {

  val gen = Karatsuba(128)
  gen.setVerticesAsNaive()
  gen.updateLatency()
  gen.toPng("kara128")
  RtlGen(gen.implH, "kara128")

  val data = Seq.fill(100)(BigInt(32, Random))
  ChainsawTest.test(gen, data)
}