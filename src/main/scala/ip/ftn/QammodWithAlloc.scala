package org.datenlord
package ip.ftn

import org.datenlord.matlab._
import org.datenlord.{logger, matlabEngine}
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

  val availableList = Seq(2, 4, 6)
  require(bitAlloc.forall(availableList.contains(_)))
  require(powAlloc.forall(_ <= 2.0)) // TODO: proper bound?

  override def name = s"qammod_${bitAlloc.hashCode()}_${powAlloc.hashCode()}".replace("-","N")

  override val impl = null

  override var inputTypes = Seq.fill(bitAlloc.sum)(UIntInfo(1))
  override var outputTypes = bitAlloc.map(_ => numericType)

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = 1

  def getQamSymbols(bit: Int): Array[MComplex] = {
    matlabEngine.eval(s"value = qammod([0:${(1 << bit) - 1}], ${1 << bit}, 'UnitAveragePower', 1);")
    matlabEngine.getVariable("value").asInstanceOf[Array[MComplex]]
  }

  override def implH: ChainsawModule = new ChainsawModule(this) {

    logger.info(
      s"\n----qammod with bit & power alloc----" +
        s"\n\tbitAlloc for adaptive qammod: ${bitAlloc.mkString(" ")}" +
        s"\n\tpowAlloc for adaptive qammod: ${powAlloc.mkString(" ")}"
    )

    val segments = bitAlloc.scan(0)(_ + _) // split points
      .prevAndNext { case (start, end) => dataIn.slice(start, end).reverse.asBits() } // segments

    val qamValues: Map[Int, Array[MComplex]] = availableList.map(bit => bit -> getQamSymbols(bit)).toMap // general values from matlab
    val qamLuts = bitAlloc.zip(powAlloc).map { case (bit, pow) =>
      val LUTValues = qamValues(bit).map(_ * sqrt(pow)) // values with powerAlloc
      Mem(LUTValues.map(CF(_, symbolType.asSFix))) // soft => hard type
    }

    dataOut.zip(segments.zip(qamLuts)).foreach { case (port, (segment, lut)) => port := lut.readSync(segment.asUInt).asBits }
  }
}
