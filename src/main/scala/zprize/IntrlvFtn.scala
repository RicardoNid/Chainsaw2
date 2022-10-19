
package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object IntrlvFtn extends ChainsawGenerator {
  override def name = "intrlvFtn"

  override val impl = null
  override var inputTypes = Seq.fill(N1)(UIntInfo(1))
  override var outputTypes = Seq.fill(N1)(UIntInfo(1))

  override var inputFormat = codedFrameFormat
  override var outputFormat = codedFrameFormat
  override var latency = 1

  override def implH: ChainsawModule = new ChainsawModule(this) {
    val rawValue = localCounter.valueNext << 2
    val shiftValue = Mux(rawValue >= N1, rawValue - N1, rawValue).take(8).asUInt
    val inBits = dataIn.asBits
    val ret = inBits.rotateLeft(shiftValue).d(1)
    dataOut := ret.subdivideIn(1 bits)
  }
}
