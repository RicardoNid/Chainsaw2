package org.datenlord
package ip.ftn

import spinal.core._
import spinal.lib._
import comm._

import scala.language.postfixOps // as JGraphT is based on Java

object ViterbiFtn extends ChainsawGenerator {
  override def name = "viterbiFtn"

  override def impl(dataIn: Seq[Any]) = ??? // not necessary as we use data from simulink

  override var inputFormat = codedFrameFormat
  override var outputFormat = rawFrameFormat

  override var inputTypes = Seq.fill(N1)(UIntInfo(1))
  override var outputTypes = Seq.fill(N1 / 2)(UIntInfo(1))

  val viterbiGen = Viterbi(
    trellis = Trellis(7, Array(171, 133)),
    blockLength = fecLength,
    disWidth = 4,
    copies = 1
  )

  override var latency = viterbiGen.latency

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val viterbiInputs: Seq[Bits] = (0 until N1 / 2).map { i => // reorder
      val source = dataIn(i) ## dataIn(i + N1 / 2)
      Mux(validIn, source, B(0, 2 bits))
    }

    val cores = Seq.fill(N1 / 2)(viterbiGen.implH)

    viterbiInputs.zip(cores).foreach { case (bits, core) =>
      core.dataIn.head := bits
      core.lastIn := lastIn
    }

    dataOut := cores.map(_.dataOut.head)
  }

}
