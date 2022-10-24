package org.datenlord
package ip.ftn

import org.datenlord.comm.QammodWithAlloc
import org.datenlord.flowConverters.S2P

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

    s2p.dataIn := dataIn
    s2p.lastIn := lastIn
    s2p.validIn := validIn

    core.dataIn := s2p.dataOut
    dataOut := core.dataOut
  }
}