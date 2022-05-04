package org.datenlord
package arithmetic

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.math.ceil

case class PipelinedBigAdderConfig(addWidth: Int, baseWidthMax: Int = 66) {
  def latency = (addWidth + baseWidthMax - 1) / baseWidthMax
}

/**
 * @param addWidth
 * @param baseWidthMax
 */
case class PipelinedBigAdder(config: PipelinedBigAdderConfig) extends Component {

  import config._

  val N = ceil(addWidth.toDouble / baseWidthMax).toInt

  val dataIn = slave Flow Vec(UInt(addWidth bits), 2)
  val dataOut = master Flow UInt(addWidth + 1 bits)

  val Seq(x, y) = dataIn.payload

  val xLow2High = x.subdivideIn(baseWidthMax bits, strict = false)
  val yLow2High = y.subdivideIn(baseWidthMax bits, strict = false)

  val xDelayed = xLow2High.zipWithIndex.map { case (x, delay) => x.d(delay) }
  val yDelayed = yLow2High.zipWithIndex.map { case (y, delay) => y.d(delay) }

  val carrys = ArrayBuffer[Bool](False)
  val sums = ArrayBuffer[UInt]()

  xDelayed.zip(yDelayed).foreach { case (x, y) =>
    val ret = ((x +^ y) + carrys.last.asUInt(1 bits)).d(1)
    carrys += ret.msb
    sums += ret.takeLow(ret.getBitsWidth - 1).asUInt
  }

  val alignedSums = sums.reverse.zipWithIndex.map { case (segment, i) => segment.d(i) } // high2low
  val ret = carrys.last.asUInt(1 bits) @@ alignedSums.reduce(_ @@ _)

  dataOut.payload := ret
  dataOut.valid := dataIn.valid.validAfter(config.latency)
}