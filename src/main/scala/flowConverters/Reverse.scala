package org.datenlord
package flowConverters

import spinal.core._
import spinal.lib._

/** reverse the order of data for each port independently
 *
 * @param blockLength
 * @param vecWidth
 * @param bitWidth
 */
case class Reverse(blockLength: Int, vecWidth: Int, bitWidth: Int) extends ChainsawGenerator {
  override def name = s"reverse_l${blockLength}_v${vecWidth}_b${bitWidth}"

  override def impl(dataIn: Seq[Any]): Seq[Any] = dataIn.grouped(vecWidth).toSeq
    .transpose.map(_.reverse).transpose
    .flatten

  override var inputTypes = Seq.fill(vecWidth)(UIntInfo(bitWidth))
  override var outputTypes = Seq.fill(vecWidth)(UIntInfo(bitWidth))

  override var inputFormat = MatrixFormat(vecWidth, blockLength)
  override var outputFormat = MatrixFormat(vecWidth, blockLength)
  override var latency = blockLength + 1

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val ramDepth = (1 << log2Up(blockLength)) * 2
    val stack = Mem(HardType(dataIn), ramDepth) // redundant RAM

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


    val writeLock = RegInit(True)
    when(lastForWrite && writeCounter.willOverflow)()
      .elsewhen(lastForWrite)(writeLock.clear())
      .elsewhen(writeCounter.willOverflow)(writeLock.set())

    // forward -> stack
    stack.write(writeAddr, dataIn, !writeLock)
    // stack -> backward
    dataOut := stack.readSync(readCounter)

  }
}
