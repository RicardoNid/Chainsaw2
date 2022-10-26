package org.datenlord
package flowConverters

import org.datenlord.{ChainsawGenerator, ChainsawModule, UIntInfo}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class S2P(s: Int, p: Int, bitWidth: Int)
  extends ChainsawGenerator {

  require(p % s == 0)

  override def name = s"S2P_s${s}_p$p"

  override def impl(dataIn: Seq[Any])  =  dataIn

  override var inputTypes = Seq.fill(s)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(p)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormat(s, p / s)
  override var outputFormat = MatrixFormatAddBubble(p, 1, p / s - 1)
  override var latency = p / s

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val counter = CounterFreeRun(p / s)
    when(lastIn)(counter.clear())

    // write
    val buffers = (0 until p / s - 1).map(i =>
      RegNextWhen(dataIn, counter.value === U(i, log2Up(p / s) bits)))

    // read
    dataOut := Vec(buffers.map(_.toSeq).reduce(_ ++ _) ++ dataIn).d(1)
  }
}
