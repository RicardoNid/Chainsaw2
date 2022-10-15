package org.datenlord
package zprize

object IfftFtn extends ChainsawGenerator {

  override def name = "ifftFtn"

  override val impl = ???

  override var inputTypes = Seq.fill(N1)(symbolType)
  override var outputTypes = Seq.fill(N2 / 2 + 5)(symbolType)

  override var inputFormat = symbolFrameFormat
  override var outputFormat = realFrameFormat

  /** --------
   * generators
   * -------- */


  override var latency = _

  override def implH = new ChainsawModule(this) {

  }
}
