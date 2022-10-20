package org.datenlord
package zprize

import org.jgrapht._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random // as JGraphT is based on Java

case class Karatsuba(width: Int, constant:Option[BigInt] = None) extends Dag {

  override def name = s"karatsuba_$width"

  override val impl = (dataIn: Seq[Any]) => Seq(dataIn.asInstanceOf[Seq[BigInt]].product)

  val baseWidth = 34
  val splitLimit = 96

  def getValidWidths(width: Int) = {
    val total = width.divideAndCeil(splitLimit)
    Seq.fill(total - 1)(splitLimit) :+ (width - (total - 1) * splitLimit)
  }

  var compensation = BigInt(0)

  /** --------
   * primitives used in Karatusba
   * -------- */
  implicit class karatsubaUtil(port: DagPort) {
    def split2 = {
      val s = Split(port.width, (port.width + 1) / 2).asVertex
      s := port
      (s.out(0), s.out(1))
    }

    def +^(that: DagPort) = {
      val add = Cpa(BinaryAdder, getValidWidths(that.width max port.width), S2S).asVertex
      add := (port, that)
      add.out(0)
    }

    def +(that: DagPort) = {
      val add = Cpa(BinaryAdder, getValidWidths(that.width max port.width), S2S, withCarry = false).asVertex
      add := (port, that)
      add.out(0)
    }

    def -(that: DagPort) = {
      val add = Cpa(BinarySubtractor, getValidWidths(that.width max port.width), S2S).asVertex
      add := (port, that)
      add.out(0)
    }

    def *(that: DagPort) = {
      require(port.width <= baseWidth && that.width <= baseWidth)
      val mult = Multiplier(Seq(port.width, that.width)).asVertex
      mult := (port, that)
      mult.out(0)
    }

    def karaWith(that: DagPort) = {
      val (aHigh, aLow) = port.split2
      val (bHigh, bLow) = that.split2
      val kara = Karabase(aLow.width, bLow.width).asVertex
      kara := (aHigh, aLow, bHigh, bLow)
      kara.outPorts
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
    val treeGen = CompressorTree(operandInfos)
    compensation = treeGen.compensation
    logger.info(s"bits into compressor tree: ${operandInfos.map(_.width).sum}")
    logger.info(s"latency of compressor tree: ${treeGen.latency}")
    val merge = treeGen.asVertex
    merge := (operands: _*)
    merge.outPorts
  }

  /** --------
   * full-DSP solution
   * -------- */
  // TODO: fine-tuned pipeline for pre addition
  val factors = Seq(2, 2, 2, 2, 2, 2) // decompositions generated by karasearch
  val segmentCount = factors.scan(1)(_ * _).reverse.takeWhile(count => width.divideAndCeil(count) <= splitLimit).last
  logger.info(s"segmentCount = $segmentCount, segmentWidth = ${width.divideAndCeil(segmentCount)}")

  /** --------
   * solution tradeoff
   * -------- */

  /** --------
   * graph construction
   * -------- */
  // TODO: more karatsuba construction method
  val Seq(a, b) = Seq.fill(2)(InputVertex(UIntInfo(width)))
  val o = OutputVertex(UIntInfo(width * 2))

  val products = ArrayBuffer[Port]()

  def buildRecursively(a: Port, b: Port): Port = {

    val splitWidth = (a.width + 1) / 2
    if (a.width <= baseWidth && b.width <= baseWidth) { // last layer, which is special

      val Seq(high, cross, low) = a karaWith b
      // FIXME: should directly port products from KaraBase to CompressorTree(no sync after multipliers)
      //      products ++= Seq(high, cross, low)
      val ret = (high << (splitWidth * 2)) + (cross << splitWidth) + low
      products += ret
      ret
    }
    else {
      val (aH, aL) = a.split2
      val (bH, bL) = b.split2
      val aMerge = aH +^ aL
      val bMerge = bH +^ bL

      val high = buildRecursively(aH, bH)
      val low = buildRecursively(aL, bL)
      val all = buildRecursively(aMerge, bMerge)

      val cross = all - high - low
      val ret = (high << (splitWidth * 2)) + (cross << splitWidth) + low
      ret.resize(a.width * 2)
    }
  }

  o := buildRecursively(a, b)
  AutoPipeline(this)
  toPng(s"kara${width}_before_rewriting")

  /** --------
   * partial retiming
   * -------- */
  val targets = Some(products.map(_.target))
  AutoPipeline(this, targets = targets)

  /** --------
   * post-addition rewriting
   * -------- */
  val pathFinder = new alg.shortestpath.AllDirectedPaths(this) // path finder from JGraphT
  val operandsAndInfos = ArrayBuffer[(Port, OperandInfo)]() // operands and their infos tobe added
  val redundantVertices = mutable.Set[DagVertex]() // vertices tobe removed

  val retimingBase = products.map(_.target).map(retimingInfo).min
  logger.info(
    s"\n----rewriting paths from products to out----" +
      s"\n\tnumber of products: ${products.length}"
  )
  products.foreach { product =>
    // TODO: will maxPathLength be a trouble maker? may be when width is extremely large
    val allPaths = pathFinder.getAllPaths(product.vertex, o.vertex, false, 100)
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

  logger.info(s"bits before inflation: ${products.map(_.width).sum}")
  val sortedOperands = operandsAndInfos.sortBy(_._2.time)


  val carrySaves = merge(sortedOperands.map(_._1), sortedOperands.map(_._2))
  val ret = carrySaves(0) +^ carrySaves(1)
  if (redundantVertices.nonEmpty) o := ret.resize(width * 2)


  /** --------
   * full retiming
   * -------- */
  autoPipeline()
  println(s"products: ${products.map(product => retimingInfo(product.vertex))}")
  graphDone()

  override def implNaiveH = Some(new ChainsawModule(this) {
    if(constant.nonEmpty) uintDataOut.head := (uintDataIn.head * constant.get).d(latency)
    else uintDataOut.head := uintDataIn.reduce(_ * _).d(latency)
  })
}

