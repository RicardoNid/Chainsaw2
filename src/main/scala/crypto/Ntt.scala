package org.datenlord
package crypto

import crypto.NttAlgo._

case class Ntt(N: Int, inverse: Boolean, // determining the transformation
               q: BigInt, streamWidth: Int)

  extends ChainsawGenerator {

  val bitWidth = q.bitLength
  val prefix = if (inverse) "intt" else "ntt"

  override def name = s"${prefix}_n${N}_q${q}_sw${streamWidth}"

  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    if (!inverse) NttAlgo.ntt(dataIn.asInstanceOf[Seq[BigInt]].toArray, q).toSeq
    else NttAlgo.intt(dataIn.asInstanceOf[Seq[BigInt]].toArray, q).toSeq
  }

  override var inputTypes = Seq.fill(streamWidth)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(streamWidth)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormat(streamWidth, N / streamWidth)
  override var outputFormat = MatrixFormat(streamWidth, N / streamWidth)
  override var latency = 1

  override def implH = new ChainsawModule(this) {
    uintDataOut := uintDataIn.d(1)
  }
}

object Ntt extends App {
  MatrixFormat(16, 256 / 16).generateWaveform("ntt_format")
}
