package org.datenlord
package ip.ftn

import comm.QamdemodWithAlloc
import flowConverters.P2S

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object QamdemodFtn extends ChainsawGenerator {
  override def name = "QamdemodFtn"

  val coreGen = QamdemodWithAlloc(bitAlloc, powAlloc, symbolType)
  val p2sGen = P2S(N1 * 4, N1, 1)

  override def impl(dataIn: Seq[Any]) = null

  override var inputTypes = Seq.fill(N1)(symbolType)
  override var outputTypes = Seq.fill(N1)(UIntInfo(1))

  override var inputFormat = symbolFrameFormat
  override var outputFormat = codedFrameFormat
  override var latency = Seq(coreGen, p2sGen).map(_.latency).sum

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val core = coreGen.implH
    val p2s = p2sGen.implH

    core.dataIn := dataIn

    p2s.dataIn := core.dataOut
    p2s.lastIn := lastIn.validAfter(coreGen.latency)
    p2s.validIn := validIn.validAfter(coreGen.latency)

    dataOut := p2s.dataOut
  }
}
