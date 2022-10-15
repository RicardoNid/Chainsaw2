package org.datenlord
package zprize

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

object QammodFtn extends ChainsawGenerator {

  val coreGen = QammodWithAlloc(bitAlloc, powAlloc, symbolType)
  val s2pGen = S2P(N1, N1 * 4, 1)
  //  val p2sGen = P2S(N1, N1 / 4, symbolType.bitWidth)

  override def name = "QammodFtn"

  override val impl = null

  override var inputTypes = Seq.fill(N1)(UIntInfo(1))
  //  override var outputTypes = Seq.fill(N1 / 4)(symbolType)
  override var outputTypes = Seq.fill(N1)(symbolType)

  override var inputFormat = codedFrameFormat
  override var outputFormat = symbolFrameFormat
  //  override var latency = Seq(coreGen, s2pGen, p2sGen).map(_.latency).sum
  override var latency = Seq(coreGen, s2pGen).map(_.latency).sum

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val core = coreGen.implH
    val s2p = s2pGen.implH
    //    val p2s = p2sGen.implH

    s2p.dataIn := dataIn
    s2p.lastIn := lastIn
    s2p.validIn := validIn

    core.dataIn := s2p.dataOut

    //    p2s.dataIn := core.dataOut
    //    p2s.lastIn := lastIn.validAfter(s2pGen.latency + coreGen.latency)
    //    p2s.validIn := validIn.validAfter(s2pGen.latency + coreGen.latency)

    //    dataOut := p2s.dataOut
    dataOut := core.dataOut
  }
}