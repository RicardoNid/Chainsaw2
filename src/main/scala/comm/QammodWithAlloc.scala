package org.datenlord
package comm

import breeze.math._
import org.datenlord.{CF, ChainsawGenerator, ChainsawModule, NumericTypeInfo, UIntInfo, logger}
import spinal.core._
import spinal.lib._

import scala.math.sqrt

/** qammod with bit allocation & power allocation
 *
 * @param bitAlloc bit allocation fore each subcarrier
 * @param powAlloc power allocation fore each subcarrier
 */
case class QammodWithAlloc(bitAlloc: Seq[Int], powAlloc: Seq[Double], numericType: NumericTypeInfo)
  extends ChainsawGenerator {

  val availableList = Seq(2, 4, 6, 8)
  require(bitAlloc.forall(availableList.contains(_)))
  require(powAlloc.forall(_ <= 2.0)) // TODO: proper bound?

  override def name = s"qammod_${bitAlloc.hashCode()}_${powAlloc.hashCode()}".replace("-", "N")

  override def impl(dataIn: Seq[Any])  =  {
    val bits = dataIn.asInstanceOf[Seq[BigInt]]
    val segments = bitAlloc.scan(0)(_ + _) // split points
      .prevAndNext { case (start, end) => bits.slice(start, end) } // segments
    val ints: Seq[BigInt] = segments.map(bits => BigInt(bits.map(_.withWidth(1)).reduce(_ + _), 2))
    val rets = ints.zip(bitAlloc).map { case (int, i) => QamAlgo.qammod(Array(int.toInt), 1 << i).head }
    rets.zip(powAlloc).map{ case (complex, pow) => complex * sqrt(pow)}
  }

  override var inputTypes = Seq.fill(bitAlloc.sum)(UIntInfo(1))
  override var outputTypes = bitAlloc.map(_ => numericType)

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    logger.info(
      s"\n----qammod with bit & power alloc----" +
        s"\n\tbitAlloc for adaptive qammod: ${bitAlloc.mkString(" ")}" +
        s"\n\tpowAlloc for adaptive qammod: ${powAlloc.mkString(" ")}"
    )

    val segments = bitAlloc.scan(0)(_ + _) // split points
      .prevAndNext { case (start, end) => dataIn.slice(start, end).reverse.asBits() } // segments

    val qamValues: Map[Int, Array[Complex]] = availableList.map(bit => bit -> QamAlgo.getSymbolsByRms(1 << bit).toArray).toMap // general values from matlab
    val qamLuts = bitAlloc.zip(powAlloc).map { case (bit, pow) =>
      val LUTValues = qamValues(bit).map(_ * sqrt(pow)) // values with powerAlloc
      Mem(LUTValues.map(CF(_, numericType.asSFix))) // soft => hard type
    }

    dataOut.zip(segments.zip(qamLuts)).foreach { case (port, (segment, lut)) => port := lut.readSync(segment.asUInt).asBits }
  }
}
