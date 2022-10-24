package org.datenlord
package comm

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** Viterbi decoder hardware
 *
 * @param trellis     convolution code definition
 * @param blockLength fixed length of encoding block
 * @param disWidth    width of discrepancy
 * @see we adopt the terminologies in ''code theory''
 */
case class Viterbi(trellis: Trellis, blockLength: Int, disWidth: Int, copies: Int = 1) extends ChainsawGenerator {

  // TODO: currently, Viterbi module generate outputs in reversed order

  override def name = s"viterbi_${trellis.hashCode()}_l$blockLength".replace("-", "N")

  override val impl = (dataIn: Seq[Any]) => dataIn // TODO: true impl

  override var inputTypes = Seq.fill(copies)(UIntInfo(trellis.outputBitWidth))
  override var outputTypes = Seq.fill(copies)(UIntInfo(trellis.inputBitWidth))

  override var inputFormat = MatrixFormat(copies, blockLength)
  override var outputFormat = MatrixFormat(copies, blockLength)

  val forwardGen = ViterbiForwarding(trellis, disWidth, blockLength)
  val backwardGen = ViterbiBackwarding(trellis, disWidth, blockLength)

  override var latency = blockLength + 1 // 1 for reading

  override def implH: ChainsawModule = new ChainsawModule(this) {

    /** --------
     * components
     * -------- */
    val forward = forwardGen.implH // ACS
    val backward = backwardGen.implH // TB
    val ramDepth = (1 << log2Up(blockLength)) * 2
    val recordStack = Mem(HardType(forward.uintDataOut), ramDepth) // redundant RAM

    /** --------
     * control
     * -------- */
    val lastForWrite = lastIn
    val lastForRead = lastForWrite.validAfter(blockLength)
    val lastForBack = lastForRead.validAfter(1)
    val writeCounter = CounterFreeRun(blockLength)
    val pingPongReg = RegInit(False)
    val writeAddr = pingPongReg.asUInt @@ writeCounter.value
    pingPongReg.toggleWhen(lastForWrite)
    when(lastForWrite)(writeCounter.clear())
    val readCounter = Reg(UInt(log2Up(ramDepth) bits))
    when(lastForRead)(readCounter := writeAddr) // starts from the last written address
      .otherwise(readCounter := readCounter - 1) // count down

    /** --------
     * datapath
     * -------- */
    // input -> forward
    forward.dataIn := dataIn
    forward.lastIn := lastIn
    // forward -> stack
    recordStack.write(writeAddr, Vec(forward.dataOut.map(_.asUInt)))
    // stack -> backward
    backward.dataIn := recordStack.readSync(readCounter).map(_.asBits)
    backward.lastIn := lastForBack
    // backward -> output
    dataOut.head := backward.dataOut.head
  }
}