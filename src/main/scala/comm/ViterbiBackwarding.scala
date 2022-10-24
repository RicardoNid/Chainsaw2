package org.datenlord
package comm

import spinal.core._
import spinal.lib._

import scala.collection.mutable

case class ViterbiBackwarding(trellis: Trellis, disWidth: Int, blockLength: Int)
  extends ChainsawGenerator {

  override def name = s"viterbiBackwarding_dw${disWidth}_trellis${trellis.hashCode()}".replace("-", "N")

  override val impl = (dataIn: Seq[Any]) => {
    val ints = dataIn.asInstanceOf[Seq[BigInt]].map(_.toInt)
    val slices = ints.grouped(trellis.numStates).toSeq
    val stack = mutable.Stack(slices: _*)
    ViterbiAlgo.viterbiBackwarding(stack, trellis)
      .tail.map(_.toBinaryString.padToLeft(trellis.stateBitWidth, '0').head.asDigit).toSeq
      .reverse
  }

  override var inputTypes = Seq.fill(trellis.numStates)(UIntInfo(disWidth))
  override var outputTypes = Seq(UIntInfo(trellis.inputBitWidth))

  override var inputFormat = MatrixFormat(trellis.numStates, blockLength)
  override var outputFormat = MatrixFormat(1, blockLength)
  override var latency = 0

  override def implH: ChainsawModule = new ChainsawModule(this) {

    import trellis._

    val stateWidth = log2Up(numStates)
    val stateType = HardType(UInt(stateWidth bits))
    val discrepancyType = HardType(UInt(disWidth bits))


    val currentState = RegInit(stateType().getZero)

    // pick up discrepancies the data for comparison
    val candidateDiscrepancies = Vec(discrepancyType, numInputSymbols)
    val candidateStates = Vec(stateType, numInputSymbols)
    val candidates: Vec[Bits] = Vec(candidateDiscrepancies.zip(candidateStates).map { case (dis, state) => (dis @@ state).asBits })

    switch(currentState) { // TODO: candidate states can be picked up from a pre-computed ROM
      (0 until numStates).foreach { state =>
        is(state) {
          val prevStates = trellis.getPrevStates(state)
          candidateDiscrepancies.zip(candidateStates).zip(prevStates).foreach { case ((disPort, statePort), prevState) =>
            disPort := dataIn(prevState).asUInt // this is a big MUX
            statePort := U(prevState, stateWidth bits)
          }
        }
      }
    }

    // do comparison and find the winner
    val CS = (a: Bits, b: Bits) => {
      val (disa, statea) = a.splitAt(stateWidth)
      val (disb, stateb) = b.splitAt(stateWidth)
      Mux(disa.asUInt < disb.asUInt, statea, stateb)
    }
    val prevStateMin = candidates.reduceBalancedTree(CS).asUInt

    // control logic
    when(lastIn)(currentState.clearAll())
      .otherwise(currentState := prevStateMin)

    dataOut.head := currentState.takeHigh(inputBitWidth) // the msb of current state is the input
  }
}
