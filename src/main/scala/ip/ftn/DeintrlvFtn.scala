package org.datenlord
package ip.ftn

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object DeintrlvFtn extends ChainsawGenerator {
  override def name = "deintrlvFtn"

  override def impl(dataIn:Seq[Any]) = null
  override var inputTypes = Seq.fill(N1)(UIntInfo(1))
  override var outputTypes = Seq.fill(N1)(UIntInfo(1))

  override var inputFormat = codedFrameFormat
  override var outputFormat = codedFrameFormat
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val rawValue = localCounter.valueNext << 2
    val shiftValue = Mux(rawValue >= N1, rawValue - N1, rawValue).take(8).asUInt
    val inBits = dataIn.asBits
    val ret = inBits.rotateRight(shiftValue).d(1)
    dataOut := ret.subdivideIn(1 bits)
  }
}
